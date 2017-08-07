import math
import networkx as nx
import interface
import unionfind
import json

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
            self.sets = unionfind.new()
            for m in self.mines:
                unionfind.make(self.sets, str(m))
        self.own_mines = init_state.get('own_mines', [])

    def get_state(self):
        state = super(GreedyPunter, self).get_state()
        state['own_edges'] = self.own_edges
        state['sets'] = self.sets
        state['own_mines'] = self.own_mines
        return state

    def select(self, e):
        if str(e[0]) not in self.sets['parents']:
            unionfind.make(self.sets, str(e[0]))
        if str(e[1]) not in self.sets['parents']:
            unionfind.make(self.sets, str(e[1]))
        unionfind.union(self.sets, str(e[0]), str(e[1]))
        #self.log('select>>> {}'.format(e))
        self.own_edges.append(e)

    def turn(self, state):
        self.log(json.dumps(self.sets))
        # 1) Try to get rivers connected directly to mine
        g = nx.Graph()
        g.add_nodes_from(self.sites)
        for (s, t) in self.available_rivers:
            g.add_edge(s, t, weight=1)
        for (s, t) in self.own_edges:
            g.add_edge(s, t, weight=0)
        subgraphs = set()
        for m in self.mines:
            s = unionfind.find(self.sets, str(m))
            subgraphs.add(s)
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

        # 1.5) look through all taken edges and see if the two nodes have different owners
        # if so, we try to exercise our options
        if self.used_options < len(self.mines) and self.has_options:
            for n1, n2 in self.taken_rivers:

                if str(n1) not in self.sets['parents']:
                    continue
                if str(n2) not in self.sets['parents']:
                    continue

                m1 = unionfind.find(self.sets, str(n1))
                m2 = unionfind.find(self.sets, str(n2))

                if m1 != m2:
                    self.log("USING OPTION! %s %s" % (n1, n2))
                    self.used_options += 1
                    self.taken_rivers.discard((n1, n2))
                    self.taken_rivers.discard((n2, n1))
                    return self.option(n1,n2)

        # 2) Try to find shortest path between two subgraphs
        next = None
        mind = 10**8
        for (s, t) in self.own_edges:
            parent = unionfind.find(self.sets, str(s))
            for sg in subgraphs:
                if parent != sg:
                    try:
                        p = nx.shortest_path(g, s, sg)
                        d = 1
                        src = None
                        dst = None
                        for i in p:
                            if str(i) not in self.sets['parents'] or unionfind.find(self.sets, str(i)) not in [parent, sg]:
                                d += 1
                                if dst is None:
                                    dst = i
                            elif d == 1:
                                if unionfind.find(self.sets, str(i)) == parent:
                                    src = i
                                elif dst is None:
                                    dst = i
                        #print '{} -> {}: {} {}({}-{})'.format(s, sg, d, p, src, dst)
                        if d < mind:
                            mind = d
                            if (src, dst) in self.available_rivers:
                                next = (src, dst)
                            elif (dst, src) in self.available_rivers:
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
                            if str(i) not in self.sets['parents'] or unionfind.find(self.sets, str(i)) not in [parent, sg]:
                                d += 1
                                if dst is None:
                                    dst = i
                            elif d == 1:
                                if unionfind.find(self.sets, str(i)) == parent:
                                    src = i
                                elif dst is None:
                                    dst = i
                        #print '{} -> {}: {} {}({}-{})'.format(t, sg, d, p, src, dst)
                        if d < mind:
                            mind = d
                            if (src, dst) in self.available_rivers:
                                next = (src, dst)
                            elif (dst, src) in self.available_rivers:
                                next = (dst, src)
                    except:
                        pass
        if next and next[0] != None:
            self.select(next)
            return self.claim(*next)

        # we should pick the max possible scoring node, i guess
        best_score = 0
        best_edge = None
        for e in self.available_rivers:
            dest = None

            node_owner = None
            if not str(e[0]) in self.sets['parents']:
                dest = e[0]

            if not str(e[1]) in self.sets['parents']:
                # if neither e[0] or e[1] are in our sets, continue
                if dest != None:
                    continue

                dest = e[1]
                node_owner = unionfind.find(self.sets, str(e[0]))
            else:
                node_owner = unionfind.find(self.sets, str(e[1]))


            if dest is None:
                continue

            score = 0
            for m in self.mines:
                if str(m) in self.sets['parents']:
                    mine_owner = unionfind.find(self.sets, str(m))
                    if mine_owner == node_owner:
                        score += self.distances[m][dest]**2

            if score > best_score:
                best_score = score
                best_edge = e


        if best_edge:
            self.select(best_edge)
            return self.claim(*best_edge)

        # 5) Pick any available edge
        e = self.available_rivers.pop()
        self.select(e)
        return self.claim(*e)

if __name__ == '__main__':
    iface = interface.MakeInterface("Greedy Punter", GreedyPunter)
    iface.run()
