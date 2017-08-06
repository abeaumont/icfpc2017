#!/usr/bin/env python

import os
import interface

import heapq
import random


# optimizations to make:
# * prioritize which nodes we visit by their score
# * make sure that the score we consider a node is based on their distance to the connected mine, and not by their absolute depth in the DFS
# * try to build paths that connect multiple mines
class DFSPunter(interface.Punter):
    def __init__(self, name, init_state, fname=None):
        super(DFSPunter, self).__init__(name, init_state)

        # the dfs stack is: score, node, originating mine
        self.dfs_stack = []
        self.visited = {}

        if 'visited' in init_state:
            for k,v in init_state['visited'].iteritems():
                self.visited[int(k)] = v
        if 'dfs_stack' in init_state:
            self.dfs_stack = init_state['dfs_stack']

    def get_state(self):
        state = super(DFSPunter, self).get_state()
        state['visited'] = self.visited
        state['dfs_stack'] = self.dfs_stack
        return state

    def turn(self, state):
        self.log("state: %s", state)


        def visit(mine, next_site, neighbor):
            edge = (neighbor, next_site)
            edge_rev = (next_site, neighbor)

            if mine in self.visited:
                mine = self.visited[mine]
            if next_site in self.visited:
                mine = self.visited[next_site]

            should_visit = False
            if not neighbor in self.visited:
                should_visit = True
            elif abs(self.visited[neighbor]) != abs(mine):
                should_visit = True


            # if we aren't connected yet, we try to connect
            if should_visit and (edge in self.available_rivers or edge_rev in self.available_rivers):
                score = 0
                this_score = 0

                for m in self.mines:
                    if m is not mine and m in self.visited and self.visited[m] == m:
                        score -= self.distances[m][neighbor]**2
                        this_score -= self.distances[m][next_site]**2
                    else:
                        score += self.distances[m][neighbor]**2
                        this_score += self.distances[m][next_site]**2

                if neighbor in self.visited:
                    next_mine = abs(self.visited[neighbor])
                    converted = 0
                    if abs(next_mine) != abs(mine):
                        for node in self.visited.keys():
                            if abs(self.visited[node]) == next_mine:
                                converted += 1
                                self.visited[node] = mine

                    self.visited[mine] = -mine
                    self.visited[next_mine] = -mine

                heapq.heappush(self.dfs_stack, (this_score, next_site, mine) )
                heapq.heappush(self.dfs_stack, (score, neighbor, mine) )

                self.visited[neighbor] = abs(mine)
                self.visited[next_site] = abs(mine)


                return self.claim(*edge)


        # try to grab any mines we can when the game first starts
        mines = list(self.mines)
        random.shuffle(mines)
        for m in mines:
            if m in self.visited:
                continue
            if m not in self.neighbors:
                continue

            if not m in self.neighbors:
                continue

            # we should order the neighbors based on their distance from all
            # the other mines
            neighbor_scores = {}

            for neighbor in self.neighbors[m]:
                neighbor_scores[neighbor] = 0
                for mm in self.mines:
                    neighbor_scores[neighbor] += self.distances[mm][neighbor]

            for neighbor in sorted(self.neighbors[m], key=lambda n: neighbor_scores[n]):
                ret = visit(m, m, neighbor)
                if ret:
                    return ret


        # look through all available edges and see if the two nodes have different owners
        for n1, n2 in self.available_rivers:
            if n1 not in self.visited:
                continue
            if n2 not in self.visited:
                continue

            if abs(self.visited[n1]) != abs(self.visited[n2]):
                ret = visit(self.visited[n1], n1, n2)
                if ret:
                    return ret

        # actually execute our DFS STACK
        while len(self.dfs_stack):
            next_score, next_site, mine = self.dfs_stack.pop()
            if next_site not in self.neighbors:
                continue

            neighbors = list(self.neighbors[next_site])
            random.shuffle(neighbors)
            for neighbor in neighbors:
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
