import json
import os
import interface
import random
import uuid

class RandPunter(interface.Punter):
    def __init__(self, name, init_state, fname=None):
        super(RandPunter, self).__init__(name, init_state)
        self.fname = fname if fname is not None else str(uuid.uuid4())
        self.all_turns = []
        random.seed()
        
    def turn(self, state):
        self.log("state: %s", state)
        if len(self.available_rivers) == 0:
            return self.pass_()
        else:
            r = random.randint(0, len(self.available_rivers) - 1)
            for i in range(r):
                self.available_rivers.add(self.available_rivers.pop())
            return self.claim(*self.available_rivers.pop())

if __name__ == '__main__':
    import socket, sys
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    if len(sys.argv) < 2:
        s.connect(('localhost', 9000))
    else:
        s.connect(('punter.inf.ed.ac.uk', int(sys.argv[1])))
    iface = interface.Interface("Random", RandPunter, s.makefile())
    iface.run()
