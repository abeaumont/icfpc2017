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
        print('sending message: ', msg)
        self.server.write(json.dumps(msg) + '\n')
        self.server.flush()

    def _recv(self):
        line = self.server.readline()
        print('receiving message: ', line)
        if not line:
            raise EOFError()
        return json.loads(line)
        
    def run(self):
        self._send({'me': self.name})
        self._recv()
        init = self._recv()
        print init
        self._send({'ready': init['punter']})
        self.punter = self.punter_class(self.name, init)
        while True:
            state = self._recv()
            if 'stop' in state:
                self.punter.stop(state)
                break
            turn = self.punter.turn(state)
            self._send(turn)
        
