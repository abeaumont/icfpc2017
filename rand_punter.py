import json
import os
import interface
import uuid

class RandPunter(interface.Punter):
    def __init__(self, name, init_state, fname=None):
        super(RandPunter, self).__init__(name, init_state)
        self.fname = fname if fname is not None else str(uuid.uuid4())
        self.all_turns = []
        
    def turn(self, state):
        self.log("state: %s", state)
        if len(self.available_rivers) == 0:
            return self.pass_()
        else:
            return self.claim(*self.available_rivers.pop())

    def stop(self, state):
        try:
            os.makedirs("output")
        except OSError:
            pass

        json_obj = {
            "turns" : self.all_turns,
            "num_players" : self.punters
        }
        json_str = json.dumps(json_obj)
        fname = os.path.join('output', self.fname + '.json')
        if not os.path.exists('output'):
            os.makedirs('output')
        with open(fname, "w") as f:
            f.write(json_str)
        print "SAVED GAME TO", fname, "SIZE IS", len(json_str)
        

if __name__ == '__main__':
    import socket, sys
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    if len(sys.argv) < 2:
        s.connect(('localhost', 9000))
    else:
        s.connect(('punter.inf.ed.ac.uk', int(sys.argv[1])))
    iface = interface.Interface("Random", RandPunter, s.makefile())
    iface.run()
