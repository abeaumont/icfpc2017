import interface

class RandPunter(interface.Punter):
    def __init__(self, name, init_state):
        interface.Punter(self, name, init_state)
        self.available_rivers = {(r['source'], r['target']) for r in self.rivers}
        
    def turn(self, state):
        for m in state['move']['moves']:
            if claim in m:
                s = m['claim']['source']
                t = m['claim']['target']
                self.available_rivers((s, t))
                self.available_rivers((t, s))
        if len(self.available_rivers) == 0:
            self.pass_()
        else:
            self._claim(*self.available_rivers.pop())
        

if __name__ == '__main__':
    import socket
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect(('localhost', 9000))
    iface = interface.Interface("Random", RandPunter, s.makefile())
    iface.run()
