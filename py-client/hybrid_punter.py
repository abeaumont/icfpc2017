import interface
import greedy_punter
import dfs_punter
import mst_punter

def HybridPunter(name, init_state):
    sites = init_state['map']['sites']
    rivers = init_state['map']['rivers']

    density = float(len(rivers)) / float(len(sites))

    if density < 2 and len(sites) < 200:
        return greedy_punter.GreedyPunter(name, init_state)
    else:
        return dfs_punter.DFSPunter(name, init_state)

    
if __name__ == '__main__':
    iface = interface.MakeInterface("Hyb Punter", HybridPunter)
    iface.run()

