## TODO:

* [x] download maps from http://punter.inf.ed.ac.uk/maps/
* [x] implement online game server
* [x] add skeleton for online client
* [x] add random online client
* [ ] add scoring to game server [kadoban]
* [x] add url param to game viewer that preloads file, then add url printing to server
* [x] move the python clients to a subdir
* [x] add visualizer for game states (from the server view)
* [x] polish visualizer [jkhl]
* [x] save game state to file
* [x] save client game state to file
* [ ] add skeleton for offline client
* [ ] make game server robust
* [ ] add greedy online client
* [ ] understand the limitations of what we can store in game state
* [x] understand if all pairs shortest path is feasible (it's not)

### Strategy stuff

* [ ] brainstorm strategies [all]
* [ ] implement MST based strategy
* [ ] implement client strategy [yuuri]
* [ ] implement strategy to connect multiple mines via tree
* [ ] implement optimized DFS strategy (that reaches for points)


## GOALS:

Day 1:

 * [x] implement online server and clients
 * [x] implement multiple strategies to get an idea of whats possible
 * [ ] add game scoring to online server
 * [x] understand how big the maps can get
 * [x] try out a random client in the online mode on their server

Day 2:

  * [ ] add tournaments for running against multiple AI
  * [ ] add some statistics / historical logging, so we can get an idea of an AI's ability
  * [ ] convert to offline mode

Day 3:

  * TBD

## Strategy Ideas

* put your ideas here!
* Brute-force solver for testing on small. Make it usable near the end of games
  as well.
* MCTS (monte carlo tree search), code template is in mcts_punter.py, just needs random_playout implemented
* policy function (move evaluation) using up to about 50 features, optimize weights by self-play
* possibly can then even guide the MCTS using the policy function (powerful approach)
* BFS to find nearest mine and shortest path to it
* Greedy rules based on how valuable each river is. Probably doomed unless we
  come up with clever rules.
* tit for tat: try a symmetric strategy (nim-like)
* should we limit the opponent's opportunities as much as possible?

### Greedy Ideas

* Do a BFS from each mine, labeling each node as its squared distance from the mine. Then during gameplay, we try to obtain the highest value nodes
* Make an optimal play at each step
* Make a greedy algorithm that has tuneable co-efficients for optimizing

### River Features for an unclaimed river
* couldntPossiblyConnectToAMine/orphaned/unreachable -- able to detect/rule out using BFS/DFS
* isAdjacentToMine
* wouldLink2Mines
* isBridge/bridgeValue/isTheOnlyRiverToSubGraphConnectedComponentOfSizeS https://en.wikipedia.org/wiki/Bridge_(graph_theory)
* totalDistanceFromAllMines
* howLikelyToLinkMines - ?how to compute
* numRiversItWouldLink
* totalNumberOfOutgoingRiversFromHeadAndTailNodes
* numNodesReachableFromHeadNode
* numNodesReachableFromTailNode
* ? -- ?fast enough

###### Terms
* -r-: unclaimed river/blue river r
* =r=: your river/green river r
* xrx: enemy river/grey river r
* "link" (nn/vb): "is-claimed/claim an adjacent edge between" eg: 1=r=2  r is a link from 1 to 2
* "bridge" (nn/vb): "is-reachable/make reachable (connected)" eg 1===2-r-3===4 r is an unclaimed bridge from 1 to 4

O(3632) rivers in examples, 2s movetimelimit .'. features in O(V^2)
