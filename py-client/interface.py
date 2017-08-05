import json
import socket
import sys
import os
import traceback
import uuid

from helpers import game_url
from collections import deque

class Punter(object):
    def __init__(self, name, state, fname=None):
        self.fname = fname if fname is not None else str(uuid.uuid4())
        self.state = state
        self.name = name
        self.punter = state['punter']
        self.punters = state['punters']
        self.map = state['map']
        self.sites = {s['id'] for s in state['map'].get('sites', [])}
        self.rivers = state['map'].get('rivers', [])
        self.mines = state['map'].get('mines', [])
        if 'available_rivers' in state:
            self.available_rivers = {tuple(r) for r in state['available_rivers']}
        else:
            self.available_rivers = {(r['source'], r['target']) for r in self.rivers}
        if 'all_turns' in state:
            self.all_turns = state['all_turns']
        else:
            self.all_turns = []
        if 'neighbors' in state:
            self.neighbors = {int(k): set(v) for k,v in state['neighbors'].iteritems()}
        else:
            neighbors = {}
            for r in self.rivers:
                s,d = r['source'], r['target']
                if not s in neighbors:
                    neighbors[s] = set()
                if not d in neighbors:
                    neighbors[d] = set()

                neighbors[s].add(d)
                neighbors[d].add(s)
            self.neighbors = neighbors


        distances = {}
        if 'distances' in state:
            for neighbor in state['distances']:
                distances[int(neighbor)] = {}
                for k,v in state['distances'][neighbor].iteritems():
                    distances[int(neighbor)][int(k)] = v
        else:
            # do our BFS from each mine here
            for m in self.mines:
                distances[m] = self.calc_distances_for_mine(m)

        self.distances = distances


    def get_state(self):
        """State to be saved between turns (offline mode only)"""
        self.log("SAVING STATE FROM INTERFACE")
        state = self.state
        state['available_rivers'] = list(self.available_rivers)
        state['all_turns'] = self.all_turns
        state['neighbors'] = {k: list(v) for k,v in self.neighbors.iteritems()}
        state['distances'] = self.distances
        return state

    def turn(self, state):
        """Your logic per turn goes here"""
        return self.pass_()

    def futures(self):
        """Your futures logic here"""
        return [{"source": 1, "target": 2}]

    def stop(self, state):
        self.save_game()

    def claim(self, source, target):
        # we need to verify the source targets properly

        for river in self.rivers:
            if (river["target"] == target and river["source"] == source) or (river["target"] == source and river["source"] == target):
                return {'claim': {'punter': self.punter, 'source': river["source"], 'target': river["target"]}}

    def pass_(self):
        return {'pass': {'punter': self.punter}}

    def log(self, message, *args):
        print >>sys.stderr, "[%s] %s" % (self.name, (message % map(str, args)))

    def calc_distances_for_mine(self, m):
        seen, todo = { m: 0 }, deque([(m, 0)])
        while todo:
            v, d = todo.popleft()
            if v not in self.neighbors:
                continue

            for c in self.neighbors[v]:
                if c in seen:
                    continue
                todo.append((c, d+1))
                seen[c] = d
        return seen

    def upkeep_punter(self, state):
        if 'move' in state:
            moves = state['move']['moves']
        elif 'stop' in state:
            moves = state['stop']['moves']

        self.all_turns.extend(moves)
        for m in moves:
            if m and 'claim' in m:
                s = m['claim']['source']
                t = m['claim']['target']
                self.available_rivers.discard((s, t))
                self.available_rivers.discard((t, s))

    def save_game(self):
        try:
            os.makedirs("output")
        except OSError:
            pass

        json_obj = {
            "turns" : self.all_turns,
            "num_players" : self.punters,
            "map" : self.map
        }

        json_str = json.dumps(json_obj)
        fname = os.path.join('output', self.fname + '.json')
        with open(fname, "w") as f:
            f.write(json_str)
        self.log("SAVED GAME TO {} SIZE IS {}".format(game_url(fname), len(json_str)))


class Interface(object):
    def __init__(self, name, punter_class, server):
        self.name = name
        self.punter_class = punter_class
        self.punter = None
        self.server = server

    def _send(self, msg):
        msg = json.dumps(msg)
        msg = '{}:{}'.format(len(msg), msg)
        self.log('sending message: %s', msg)
        self.server.write(msg)
        self.server.flush()

    def _recv(self):
        s = ''
        c = self.server.read(1)
        while c != ':':
            s += c
            c = self.server.read(1)
        line = self.server.read(int(s))
        self.log('receiving message: %s', line)
        if not line:
            raise EOFError()
        return json.loads(line)

    def log(self, message, *args):
        print >>sys.stderr, "[%s] %s" % (self.name, (message % map(str, args)))

    def run(self):
        self._send({'me': self.name})
        self._recv()
        init = self._recv()
        self.log("init: %s", str(init))
        self.punter = self.punter_class(self.name, init)
        settings = init.get('settings', {})
        msg = {
            'ready': init['punter']
        }
        if settings.get('futures', False):
            msg['futures'] = self.punter.futures()
        self._send(msg)
        while True:
            state = self._recv()
            self.punter.upkeep_punter(state)

            if 'stop' in state:
                self.punter.stop(state)
                break


            turn = self.punter.turn(state)
            self._send(turn)

class OfflineInterface(object):
    def __init__(self, name, punter_class):
        self.name = name
        self.punter_class = punter_class
        self.punter = None
        self.fd = open('offline' + str(uuid.uuid4()) + '.log', 'w')

    def _send(self, msg):
        msg = json.dumps(msg)
        msg = '{}:{}'.format(len(msg), msg)
        self.log('sending message: %s', msg)
        sys.stdout.write(msg)
        sys.stdout.flush()

    def _recv(self):
        s = ''
        c = sys.stdin.read(1)
        while c != ':':
            s += c
            c = sys.stdin.read(1)
        line = sys.stdin.read(int(s))
        self.log('receiving message: %s', line)
        if not line:
            raise EOFError()
        try:
            return json.loads(line)
        except e:
            self.log('error: %s', str(e))

    def log(self, message, *args):
        print >>self.fd, "[%s] %s" % (self.name, (message % map(str, args)))

    def run(self):
        self._send({'me': self.name})
        msg = self._recv()
        msg = self._recv()
        if 'punter' in msg:
            try:
                self.punter = self.punter_class(self.name, msg)
                settings = msg.get('settings', {})
                msg = {
                    'ready': msg['punter'],
                    'state': self.punter.get_state()
                }
                if settings.get('futures', False):
                    msg['futures'] = self.punter.futures()
                self._send(msg)
            except:
                self.log('error: %s', traceback.format_exc())
        elif 'move' in msg:
            try:
                self.punter = self.punter_class(self.name, msg['state'])
                self.punter.upkeep_punter(msg)
                msg = self.punter.turn(msg)
                msg['state'] = self.punter.get_state()
                self._send(msg)
            except:
                self.log('error: %s', traceback.format_exc())
        elif 'stop' in msg:
            try:
                self.punter = self.punter_class(self.name, msg['state'])
                self.punter.upkeep_punter(msg)
                self.punter.stop(msg)
            except:
                self.log('error: %s', traceback.format_exc())
        self.fd.close()


def MakeInterface(name, punter, offline=False):
    if not offline or sys.argv[1] == 'local':
        # if we are using sockets
        import socket, sys
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        if len(sys.argv) < 2:
            s.connect(('localhost', 9000))
        else:
            s.connect(('punter.inf.ed.ac.uk', int(sys.argv[1])))
        iface = Interface(name, punter, s.makefile())
    else:
        iface = OfflineInterface(name, punter)

    return iface
