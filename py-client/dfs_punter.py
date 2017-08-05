import os
import interface

import heapq


# optimizations to make:
# * prioritize which nodes we visit by their score
# * make sure that the score we consider a node is based on their distance to the connected mine, and not by their absolute depth in the DFS
# * try to build paths that connect multiple mines
class DFSPunter(interface.Punter):
    def __init__(self, name, init_state, fname=None):
        super(DFSPunter, self).__init__(name, init_state)

        # the dfs stack is: score, node, originating mine
        self.dfs_stack = [(1, m, m) for m in self.mines]
        self.visited = {}

        if 'visited' in init_state:
            self.visited = init_state['visited']
        if 'dfs_stack' in init_state:
            self.dfs_stack = init_state['dfs_stack']

    def get_state(self):
        state = super(DFSPunter, self).get_state()
        state['visited'] = self.visited
        state['dfs_stack'] = self.dfs_stack
        return state

    def turn(self, state):
        self.log("state: %s", state)


        def visit(mine, next_site, neighbor, consider_visit=True):
            if next_site > neighbor:
                edge = (neighbor, next_site)
            else:
                edge = (next_site, neighbor)

            should_visit = False
            if not neighbor in self.visited:
                should_visit = True
            elif self.visited[neighbor] != mine:
                should_visit = True

            if neighbor in self.mines:
                should_visit = True

            if should_visit and edge in self.available_rivers:
                score = self.distances[mine][neighbor]
                this_score = self.distances[mine][next_site]

                heapq.heappush(self.dfs_stack, (this_score, next_site, mine) )
                heapq.heappush(self.dfs_stack, (score, neighbor, mine) )

                if consider_visit:
                    self.visited[neighbor] = mine
                    self.visited[next_site] = mine

                return self.claim(*edge)


        # try to grab any mines we can when the game first starts
        for m in self.mines:
            if m in self.visited:
                continue

            for neighbor in self.neighbors[m]:
                ret = visit(m, m, neighbor)
                if ret:
                    return ret

        while len(self.dfs_stack):
            next_score, next_site, mine = self.dfs_stack.pop()
            if next_site not in self.neighbors:
                continue

            # prioritize mines we can reach
            for neighbor in self.neighbors[next_site]:
                if neighbor in self.mines:
                    ret = visit(mine, next_site, neighbor)
                    if ret:
                        return ret

            for neighbor in self.neighbors[next_site]:
                ret = visit(mine, next_site, neighbor)
                if ret:
                    return ret

        if len(self.available_rivers) == 0:
            return self.pass_()
        else:
            return self.claim(*self.available_rivers.pop())

if __name__ == '__main__':
    iface = interface.MakeInterface("DMb Punter", DFSPunter)
    iface.run()
