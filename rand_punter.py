import interface

class RandPunter(interface.Punter):
    def __init__(self, name, init_state):
        super(RandPunter, self).__init__(name, init_state)
        self.available_rivers = {(r['source'], r['target']) for r in self.rivers}
        
    def turn(self, state):
        self.log("state: %s", state)
        for m in state['move']['moves']:
            if 'claim' in m:
                s = m['claim']['source']
                t = m['claim']['target']
                self.available_rivers.discard((s, t))
                self.available_rivers.discard((t, s))
        if len(self.available_rivers) == 0:
            return self.pass_()
        else:
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
