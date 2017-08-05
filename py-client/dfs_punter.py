import os
import interface



# optimizations to make:
# * prioritize which nodes we visit by their score
# * make sure that the score we consider a node is based on their distance to the connected mine, and not by their absolute depth in the DFS
# * try to build paths that connect multiple mines
class DFSPunter(interface.Punter):
    def __init__(self, name, init_state, fname=None):
        super(DFSPunter, self).__init__(name, init_state)

        self.dfs_stack = [m for m in self.mines]
        self.visited = {}

        if 'visited' in init_state:
            self.visited = init_state['visited']
        if 'dfs_stack' in init_state:
            self.dfs_stack = init_state['dfs_stack']

        print "STARTING WITH MINES", self.dfs_stack

    def get_state(self):
        state = super(DFSPunter, self).get_state()
        state['visited'] = self.visited
        state['dfs_stack'] = self.dfs_stack
        return state

    def turn(self, state):
        self.log("state: %s", state)


        def visit(next_site, neighbor):
            if next_site > neighbor:
                edge = (neighbor, next_site)
            else:
                edge = (next_site, neighbor)

            if not neighbor in self.visited and edge in self.available_rivers:
                self.dfs_stack.append(next_site) # re-enqueue ourselves just in case
                self.dfs_stack.append(neighbor)
                self.visited[neighbor] = True
                self.visited[next_site] = True

                return self.claim(*edge)



        while len(self.dfs_stack):
            next_site = self.dfs_stack.pop()
            if next_site not in self.neighbors:
                print "SITE", next_site, "HAS NO NEIGHBORS"
                continue

            # prioritize mines we can reach
            for neighbor in self.neighbors[next_site]:
                if neighbor in self.mines:
                    ret = visit(next_site, neighbor)
                    if ret:
                        return ret

            for neighbor in self.neighbors[next_site]:
                ret = visit(next_site, neighbor)
                if ret:
                    return ret

        if len(self.available_rivers) == 0:
            return self.pass_()
        else:
            return self.claim(*self.available_rivers.pop())

if __name__ == '__main__':
    iface = interface.MakeInterface("DFS Punter", DFSPunter)
    iface.run()
