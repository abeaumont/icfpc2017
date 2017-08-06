import math
import networkx as nx
import interface
import unionfind

def distance(edge):
    x = edge[0]
    y = edge[1]
    return math.sqrt(x * x + y * y)

class GreedyPunter(interface.Punter):
    def __init__(self, name, init_state, fname=None):
        super(GreedyPunter, self).__init__(name, init_state)
        self.own_edges = init_state.get('own_edges', [])
        self.sets = init_state.get('sets', None)
        if self.sets is None:
            self.sets = {m: unionfind.set(m) for m in self.mines}
        self.own_mines = init_state.get('own_mines', [])

    def get_state(self):
        state = super(GreedyPunter, self).get_state()
        state['own_edges'] = self.own_edges
        state['sets'] = self.sets
        state['own_mines'] = self.own_mines
        return state

    def select(self, e):
        if e[0] not in self.sets:
            self.sets[e[0]] = unionfind.set(e[0])
        if e[1] not in self.sets:
            self.sets[e[1]] = unionfind.set(e[1])
        unionfind.union(self.sets[e[0]], self.sets[e[1]])
        #self.log('select>>> {}'.format(e))
        self.own_edges.append(e)

    def turn(self, state):
        # 1) Try to get rivers connected directly to mine
        g = nx.Graph()
        g.add_nodes_from(self.sites)
        for (s, t) in self.available_rivers:
            g.add_edge(s, t, weight=1)
        for (s, t) in self.own_edges:
            g.add_edge(s, t, weight=0)
        subgraphs = set()
        for m in self.mines:
            s = unionfind.find(self.sets[m])
            subgraphs.add(s[0])
        mines = []
        for m in self.mines:
            if m in self.own_mines: continue
            if m not in self.neighbors: continue
            for n in self.neighbors[m]:
                if (n, m) in self.available_rivers:
                    mines.append((n, m))
                elif (m, n) in self.available_rivers:
                    mines.append((m, n))
        if mines:
            def cmp(e1, e2):
                if e1[0] in self.mines and e1[1] in self.mines and e2[0] in self.mines and e2[1] in self.mines:
                    return 0
                if e1[0] in self.mines and e1[1] in self.mines:
                    return 1
                if e2[0] in self.mines and e2[1] in self.mines:
                    return -1
                if e1[0] in self.mines:
                    e1s = e1[1]
                    e1m = e1[0]
                else:
                    e1s = e1[0]
                    e1m = e1[1]
                if e2[0] in self.mines:
                    e2s = e2[1]
                    e2m = e1[0]
                else:
                    e2s = e2[0]
                    e2m = e1[1]
                d1, d2 = 10**8, 10**8
                for s in subgraphs:
                    if e1m == s: continue
                    try:
                        d = nx.shortest_path_length(g, e1s, s)
                        d1 = min(d, d1)
                    except:
                        pass
                for s in subgraphs:
                    if e2m == s: continue
                    try:
                        d = nx.shortest_path_length(g, e2s, s)
                        d2 = min(d, d2)
                    except:
                        pass
                return d1 - d2
            mines.sort(cmp=cmp)
            e = mines[0]
            if e[0] in self.mines:
                self.own_mines.append(e[0])
            if e[1] in self.mines:
                self.own_mines.append(e[1])
            self.select(e)
            return self.claim(*e)
        # 2) Try to find shortest path between two subgraphs
        next = None
        mind = 10**8
        for (s, t) in self.own_edges:
            parent = unionfind.find(self.sets[s])[0]
            for sg in subgraphs:
                if parent != sg:
                    try:
                        p = nx.shortest_path(g, s, sg)
                        d = 1
                        src = None
                        dst = None
                        for i in p:
                            if i not in self.sets or unionfind.find(self.sets[i])[0] not in [parent, sg]:
                                d += 1
                                if dst is None:
                                    dst = i
                            elif d == 1:
                                if unionfind.find(self.sets[i])[0] == parent:
                                    src = i
                                elif dst is None:
                                    dst = i
                        #print '{} -> {}: {} {}({}-{})'.format(s, sg, d, p, src, dst)
                        if d < mind:
                            mind = d
                            if (src, dst) in self.available_rivers:
                                next = (src, dst)
                            else:
                                next = (dst, src)
                    except:
                        pass
            for sg in subgraphs:
                if parent != sg:
                    try:
                        p = nx.shortest_path(g, t, sg)
                        d = 1
                        src = None
                        dst = None
                        for i in p:
                            if i not in self.sets or unionfind.find(self.sets[i])[0] not in [parent, sg]:
                                d += 1
                                if dst is None:
                                    dst = i
                            elif d == 1:
                                if unionfind.find(self.sets[i])[0] == parent:
                                    src = i
                                elif dst is None:
                                    dst = i
                        #print '{} -> {}: {} {}({}-{})'.format(t, sg, d, p, src, dst)
                        if d < mind:
                            mind = d
                            if (src, dst) in self.available_rivers:
                                next = (src, dst)
                            else:
                                next = (dst, src)
                    except:
                        pass
        if next:
            self.select(next)
            return self.claim(*next)

        # 3) Grab any other mines, with largest distances first
        mines = []
        for m in self.mines:
            if m not in self.neighbors: continue
            for n in self.neighbors[m]:
                if (n, m) in self.available_rivers:
                    mines.append((n, m))
                elif (m, n) in self.available_rivers:
                    mines.append((m, n))
        if mines:
            mines.sort(cmp=lambda e1, e2: int((distance(e1) - distance(e2)) * 100), reverse=True)
            e = mines[0]
            self.select(e)
            return self.claim(*e)
        # 4) If the edge is a connected to an owned node, pick it
        next = None
        mind = 10 ** 8
        for e in self.available_rivers:
            if e[0] in self.sets or e[1] in self.sets:
                self.select(e)
                return self.claim(*e)
        
        # 5) Pick any available edge
        e = self.available_rivers.pop()
        self.select(e)
        return self.claim(*e)

if __name__ == '__main__':
    iface = interface.MakeInterface("Greedy Punter", GreedyPunter)
    iface.run()
