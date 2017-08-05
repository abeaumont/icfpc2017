import json
import socket
import sys

class Punter(object):
    def __init__(self, name, init_state):
        self.name = name
        self.punter = init_state['punter']
        self.punters = init_state['punters']
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
        punter.all_turns.extend(state['move']['moves'])
        for m in state['move']['moves']:
            if 'claim' in m:
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
            if 'stop' in state:
                self.punter.stop(state)
                break

            self.upkeep_punter(self.punter, state)

            turn = self.punter.turn(state)
            self._send(turn)

