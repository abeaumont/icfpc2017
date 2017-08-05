import json
import os
import interface
import uuid

class DFSPunter(interface.Punter):
    def __init__(self, name, init_state, fname=None):
        super(DFSPunter, self).__init__(name, init_state)
        self.fname = fname if fname is not None else str(uuid.uuid4())
        self.dfs_stack = [m for m in self.mines]
        self.visited = {}
        print "STARTING WITH MINES", self.dfs_stack

    def turn(self, state):
        self.log("state: %s", state)

        print "NEIGHBORS", self.neighbors

        while len(self.dfs_stack):
            next_site = self.dfs_stack.pop()
            if next_site not in self.neighbors:
                print "SITE", next_site, "HAS NO NEIGHBORS"
                continue

            # TODO: fix this to properly do a DFS, not this awkward one
            for neighbor in self.neighbors[next_site]:
                if not neighbor in self.visited and (next_site, neighbor) in self.available_rivers:
                    self.dfs_stack.append(next_site) # re-enqueue ourselves just in case
                    self.dfs_stack.append(neighbor)
                    self.visited[neighbor] = True
                    self.visited[next_site] = True

                    return self.claim(next_site, neighbor)


        if len(self.available_rivers) == 0:
            return self.pass_()
        else:
            return self.claim(*self.available_rivers.pop())

    def stop(self, state):
        self.save_game()

if __name__ == '__main__':
    import socket, sys
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    if len(sys.argv) < 2:
        s.connect(('localhost', 9000))
    else:
        s.connect(('punter.inf.ed.ac.uk', int(sys.argv[1])))
    iface = interface.Interface("DFS Hunter", DFSPunter, s.makefile())
    iface.run()
