## TODO:

* [x] download maps from http://punter.inf.ed.ac.uk/maps/
* [x] implement online game server
* [x] add skeleton for online client
* [x] add random online client
* [ ] add scoring to game server [kadoban]
* [x] add visualizer for game states (from the server view)
* [ ] polish visualizer [okay]
* [x] save game state to file
* [x] save client game state to file
* [ ] add skeleton for offline client
* [ ] make game server robust
* [ ] add greedy online client
* [ ] brainstorm strategies
* [ ] understand the limitations of what we can store in game state
* [x] understand if all pairs shortest path is feasible (it's not)
* [ ] implement MST based strategy
* [ ] implement client strategy [yuuri]


## GOALS:

Day 1:

 * implement online server and clients
 * add game scoring to online server
 * implement multiple strategies to get an idea of whats possible
 * understand how big the maps can get
 * try out a random client in the online mode on their server

Day 2:

  * add tournaments for running against multiple AI
  * add some statistics / historical logging, so we can get an idea of an AI's ability
  * convert to offline mode

## Strategy Ideas

* put your ideas here!

* Brute-force solver for testing on small. Make it usable near the end of games
  as well.
* MCTS (monte carlo tree search)
* policy function (move evaluation) using up to about 50 features, optimize weights by self-play
* Greedy rules based on how valuable each river is. Probably doomed unless we
  come up with clever rules.
* tit for tat: try a symmetric strategy (nim-like)
* should we limit the opponent's opportunities as much as possible?

### Greedy Ideas

* Do a BFS from each mine, labeling each node as its squared distance from the mine. Then during gameplay, we try to obtain the highest value nodes
* Make an optimal play at each step
* Make a greedy algorithm that has tuneable co-efficients for optimizing

### Edge Features (heuristics)
* wouldLink2Mines
* isCutEdge
* distanceFromMines
* howLikelyWeAreToLinkMines - how to compute?

O(3632) rivers in examples .'. each feature in O(V^2)
