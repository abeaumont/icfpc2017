import interface
import random

class RandPunter(interface.Punter):
    def __init__(self, name, init_state, fname=None):
        super(RandPunter, self).__init__(name, init_state)
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
    iface = interface.MakeInterface('Random Punter', RandPunter)
    iface.run()
