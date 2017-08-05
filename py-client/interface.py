import json
import socket
import sys
import os

from helpers import game_url

class Punter(object):
    def __init__(self, name, init_state):
        self.name = name
        self.punter = init_state['punter']
        self.punters = init_state['punters']
        self.map = init_state['map']
        self.sites = {s['id'] for s in init_state['map']['sites']}
        self.rivers = init_state['map']['rivers']
        self.mines = init_state['map']['mines']

    def turn(self, state):
        """Your logic per turn goes here"""
        self.pass_()

    def stop(self, state):
        """End of game"""
        self.log("end of game!")

    def claim(self, source, target):
        return {'claim': {'punter': self.punter, 'source': source, 'target': target}}

    def pass_(self):
        return {'pass': {'punter': self.punter}}

    def log(self, message, *args):
        print >>sys.stderr, "[%s] %s" % (self.name, (message % map(str, args)))

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
        print "SAVED GAME TO", game_url(fname), "SIZE IS", len(json_str)
        



class Interface(object):
    def __init__(self, name, punter_class, server):
        self.name = name
        self.punter_class = punter_class
        self.punter = None
        self.server = server

    def _send(self, msg):
        msg = json.dumps(msg)
        msg = '{}:{}'.format(len(msg), msg)
        print('sending message: ', msg)
        self.server.write(msg)
        self.server.flush()

    def _recv(self):
        line = self.server.readline()
        print('receiving message: ', line)
        if not line:
            raise EOFError()
        return json.loads(line.split(':', 1)[1])

    def setup_punter(self, punter):
        punter.all_turns = []
        punter.available_rivers = {(r['source'], r['target']) for r in punter.rivers}

        neighbors = {}
        for r in punter.rivers:
            s,d = r['source'], r['target']
            if not s in neighbors:
                neighbors[s] = set()
            if not d in neighbors:
                neighbors[d] = set()

            neighbors[s].add(d)
            neighbors[d].add(s)
        punter.neighbors = neighbors

    def upkeep_punter(self, punter, state):
        if 'move' in state:
            moves = state['move']['moves']
        elif 'stop' in state:
            moves = state['stop']['moves']

        punter.all_turns.extend(moves)
        for m in moves:
            if m and 'claim' in m:
                s = m['claim']['source']
                t = m['claim']['target']
                punter.available_rivers.discard((s, t))
                punter.available_rivers.discard((t, s))




    def run(self):
        self._send({'me': self.name})
        self._recv()
        init = self._recv()
        print init
        self._send({'ready': init['punter']})
        self.punter = self.punter_class(self.name, init)

        self.setup_punter(self.punter)

        while True:
            state = self._recv()
            self.upkeep_punter(self.punter, state)

            if 'stop' in state:
                self.punter.stop(state)
                break


            turn = self.punter.turn(state)
            self._send(turn)

