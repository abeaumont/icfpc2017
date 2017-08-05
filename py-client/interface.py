import json
import socket
import sys
import os
import uuid

from helpers import game_url

class Punter(object):
    def __init__(self, name, state, fname=None):
        self.fname = fname if fname is not None else str(uuid.uuid4())
        self.state = state
        self.name = name
        self.punter = state['punter']
        self.punters = state['punters']
        self.map = state['map']
        self.sites = {s['id'] for s in state['map']['sites']}
        self.rivers = state['map']['rivers']
        self.mines = state['map']['mines']
        if 'available_rivers' in state:
            self.available_rivers = state['available_rivers']
        else:
            self.available_rivers = {(r['source'], r['target']) for r in self.rivers}
        if 'all_turns' in state:
            self.all_turns = state['all_turns']
        else:
            self.all_turns = []
        if 'neighbors' in state:
            self.neighbors = state['neighbors']
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

    def get_state(self):
        """State to be saved between turns (offline mode only)"""
        state = self.state
        state['sites'] = self.sites
        state['available_rivers'] = self.available_rivers
        state['all_turns'] = self.all_turns
        state['neighbors'] = self.neighbors
        return state

    def turn(self, state):
        """Your logic per turn goes here"""
        self.pass_()

    def stop(self, state):
        self.save_game()

    def claim(self, source, target):
        return {'claim': {'punter': self.punter, 'source': source, 'target': target}}

    def pass_(self):
        return {'pass': {'punter': self.punter}}

    def log(self, message, *args):
        print >>sys.stderr, "[%s] %s" % (self.name, (message % map(str, args)))

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
        line = self.server.readline()
        self.log('receiving message: %s', line)
        if not line:
            raise EOFError()
        return json.loads(line.split(':', 1)[1])

    def log(self, message, *args):
        print >>sys.stderr, "[%s] %s" % (self.name, (message % map(str, args)))

    def run(self):
        self._send({'me': self.name})
        self._recv()
        init = self._recv()
        self.log("init: %s", str(init))
        self._send({'ready': init['punter']})
        self.punter = self.punter_class(self.name, init)
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
                self._send({
                    'ready': msg['punter'],
                    'state': self.punter.get_state()
                })
            except Exception as e:
                self.log('error: %s', str(e))
        elif 'move' in msg:
            self.punter = self.punter_class(self.name, msg['state'])
            self.punter.upkeep_punter(msg)
            msg = self.punter.turn(msg)
            msg['state'] = self.punter.get_state()
            self._send(msg)
        elif 'stop' in msg:
            self.punter = self.punter_class(self.name, msg['state'])
            self.punter.upkeep_punter(msg)
            self.punter.stop(msg)
        self.fd.close()
