import json
import os
import interface
import uuid
from collections import deque

class BFSPunter(interface.Punter):
    def __init__(self, name, init_state, fname=None):
        super(BFSPunter, self).__init__(name, init_state)
        self.fname = fname if fname is not None else str(uuid.uuid4())
        self.bfs_queue = deque([m for m in self.mines])
        self.visited = {}
        print "STARTING WITH MINES", self.bfs_queue

    def turn(self, state):
        self.log("state: %s", state)


        while len(self.bfs_queue):
            next_site = self.bfs_queue.popleft()
            if next_site not in self.neighbors:
                print "SITE", next_site, "HAS NO NEIGHBORS"
                continue



            # TODO: fix this to properly do a BFS, not this awkward one
            for neighbor in self.neighbors[next_site]:
                if next_site > neighbor:
                    edge = (neighbor, next_site)
                else:
                    edge = (next_site, neighbor)

                if not neighbor in self.visited and edge in self.available_rivers:
                    self.bfs_queue.append(next_site) # re-enqueue ourselves just in case
                    self.bfs_queue.append(neighbor)
                    self.visited[neighbor] = True
                    self.visited[next_site] = True


                    return self.claim(*edge)


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
    iface = interface.Interface("BFS Hunter", BFSPunter, s.makefile())
    iface.run()