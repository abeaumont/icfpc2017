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
        for e in T.edges():
            if e in self.available_rivers:
                self.own_edges.append(e)
                return self.claim(*e)
        e = self.available_rivers.pop()
        self.own_edges.append(e)
        return self.claim(*e)
