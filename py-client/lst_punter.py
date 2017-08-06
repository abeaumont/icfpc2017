import networkx as nx
import interface
import sys

class LSTPunter(interface.Punter):
    def __init__(self, name, init_state, fname=None):
        super(LSTPunter, self).__init__(name, init_state)
        self.own_edges = init_state.get('own_edges', [])

    def get_state(self):
        state = super(LSTPunter, self).get_state()
        state['own_edges'] = self.own_edges
        return state

    def turn(self, state):
        g = nx.Graph()
        g.add_nodes_from(self.sites)
        for (s, t) in self.available_rivers:
            w = 1
            g.add_edge(s, t, weight=w)

        self_nodes = set()
        for (s, t) in self.own_edges:
            g.add_edge(s, t, weight=0)
            self_nodes.add(s)
            self_nodes.add(t)
        T = nx.minimum_spanning_tree(g)

        # try to grab any mines we can when the game first starts
        for m in self.mines:
            m = int(m)
            if m in self_nodes:
                continue

            self.log("LOOKING FOR MINE %s" % m)

            # we should order the neighbors based on their distance from all
            # the other mines
            neighbor_scores = {}
            for neighbor in self.neighbors[m]:
                neighbor_scores[neighbor] = 0
                for mm in self.mines:
                    neighbor_scores[neighbor] += self.distances[mm][neighbor]

            for neighbor in sorted(self.neighbors[m], key=lambda n: neighbor_scores[n]):
                e = (m, neighbor)
                ee = (neighbor, m)
                if e not in self.available_rivers and ee not in self.available_rivers:
                    continue

                self.own_edges.append(e)
                return self.claim(*e)

        # If the edge is a connected to an owned node, pick it
        for e in T.edges():
            if e in self.available_rivers:
                for e2 in self.own_edges:
                    if e[0] == e2[0] or e[0] == e2[1] or e[1] == e2[0] or e[1] == e2[1]:
                        self.own_edges.append(e)
                        return self.claim(*e)
        # If the edge is a available, pick it
        for e in T.edges():
            if e in self.available_rivers:
                self.own_edges.append(e)
                return self.claim(*e)
        # Nothing left in LST, pick any available edge
        e = self.available_rivers.pop()
        self.own_edges.append(e)
        return self.claim(*e)

if __name__ == '__main__':
    iface = interface.MakeInterface("Least Punter", LSTPunter)
    iface.run()
