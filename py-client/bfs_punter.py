import interface
from collections import deque

class BFSPunter(interface.Punter):
    def __init__(self, name, init_state, fname=None):
        super(BFSPunter, self).__init__(name, init_state)
        self.bfs_queue = deque([m for m in self.mines])
        self.visited = {}
        self.log("STARTING WITH MINES: {}".format(self.bfs_queue))

    def turn(self, state):
        self.log("state: %s", state)


        while len(self.bfs_queue):
            next_site = self.bfs_queue.popleft()
            if next_site not in self.neighbors:
                self.log("SITE {} HAS NO NEIGHBORS".format(next_site))
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


if __name__ == '__main__':
    iface = interface.OfflineInterface("BFS Punter", BFSPunter)
    iface.run()
