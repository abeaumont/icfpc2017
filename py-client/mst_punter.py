import networkx as nx
import interface

class MSTPunter(interface.Punter):
    def __init__(self, name, init_state, fname=None):
        super(MSTPunter, self).__init__(name, init_state)
        self.own_edges = init_state.get('own_edges', [])

    def get_state(self):
        state = super(MSTPunter, self).get_state()
        state['own_edges'] = self.own_edges
        return state

    def turn(self, state):
        g = nx.Graph()
        g.add_nodes_from(self.sites)
        for (s, t) in self.available_rivers:
            g.add_edge(s, t, weight=1)
        for (s, t) in self.own_edges:
            g.add_edge(s, t, weight=0)
        T = nx.minimum_spanning_tree(g)
        # If the edge is a mine, pick it
        for e in T.edges():
            if e in self.available_rivers and (e[0] in self.mines or e[1] in self.mines):
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
        # Nothing left in MST, pick any available edge
        e = self.available_rivers.pop()
        self.own_edges.append(e)
        return self.claim(*e)

if __name__ == '__main__':
    iface = interface.MakeInterface("Most Punter", MSTPunter)
    iface.run()
