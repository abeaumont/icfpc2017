import argparse
import interface
import rand_punter
import bfs_punter
import dfs_punter
import mst_punter
import lst_punter
import greedy_punter
import hybrid_punter

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Launch a punter.')
    parser.add_argument('-t', '--type', type=str, choices=['rand', 'bfs', 'dfs', 'mst', 'lst', 'greedy'], default='rand',
                        help='punter type (default rand)')
    parser.add_argument('-H', '--host', type=str, default='punter.inf.ed.ac.uk',
                        help='host to connect to (default Opunter.inf.ed.ac.uk", online mode only)')
    parser.add_argument('-p', '--port', type=int, default=9000,
                        help='port to connect to (default 9000, online mode only)')
    parser.add_argument('-n', '--name', type=str, default=None,
                        help='bot name')
    parser.add_argument('-o', '--offline', action="store_true",
                        help='offline mode (default online)')
    args = parser.parse_args()
    if args.type == 'dfs':
        punter = dfs_punter.DFSPunter
    elif args.type == 'bfs':
        punter = bfs_punter.BFSPunter
    elif args.type == 'mst':
        punter = mst_punter.MSTPunter
    elif args.type == 'lst':
        punter = lst_punter.LSTPunter
    elif args.type == 'greedy':
        punter = greedy_punter.GreedyPunter
    elif args.type == 'hybrid':
        punter = hybrid_punter.HybridPunter
    else:
        punter = rand_punter.RandPunter
    if args.offline:
        iface = interface.OfflineInterface(args.type, punter)
    else:
        import socket, sys
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.connect((args.host, args.port))
        iface = interface.Interface(args.name if args.name else args.type, punter, s.makefile())
    iface.run()
