import interface
import greedy_punter
import dfs_punter
import mst_punter

def HybridPunter(name, init_state):
    sites = init_state['map']['sites']

    if len(sites) <= 50:
        return dfs_punter.DFSPunter(name, init_state)
    elif len(sites) <= 200:
        return greedy_punter.GreedyPunter(name, init_state)
    else:
        return dfs_punter.DFSPunter(name, init_state)

    
if __name__ == '__main__':
    iface = interface.MakeInterface("Hyb Punter", HybridPunter)
    iface.run()

