## TODO:

* [ ] implement random playout
* [ ] add scoring to game server [kadoban]
* [ ] add skeleton for offline client
* [ ] make game server robust
* [ ] add greedy online client
* [ ] understand the limitations of what we can store in game state
* [x] download maps from http://punter.inf.ed.ac.uk/maps/
* [x] implement online game server
* [x] add skeleton for online client
* [x] add random online client
* [x] add url param to game viewer that preloads file, then add url printing to server
* [x] move the python clients to a subdir
* [x] add visualizer for game states (from the server view)
* [x] polish visualizer [jkhl]
* [x] save game state to file
* [x] save client game state to file
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
 * [x] understand how big the maps can get -- 2000 sites?, 4000 rivers? http://punter.inf.ed.ac.uk/graph-viewer/
 * [x] try out a random client in the online mode on their server
 * [ ] MCTS with random playouts

Day 2:

  * [ ] add tournaments for running against multiple AI
  * [ ] add some statistics / historical logging, so we can get an idea of an AI's ability
  * [ ] convert to offline mode
  * [ ] efficient self-play!! -- be able to test paramaterized bots vs each other, to optimize the weights of their linear policy (move evaluation) function

Day 3:
  * have all the plumbing done! ready to focus solely on optimization and strategies
  * 
  * TBD

## Strategy Ideas

* put your ideas here!
* Brute-force solver for testing on small. Make it usable near the end of games
  as well. 
* minimax+alpha-beta is best search for few moves remaining. how to adapt it for multiple players? is this correct: http://www.cplusplus.com/forum/general/118289/ ?
* BFS from each mine intially to get vertex distances from nearest-mines (and maybe total distance to mines) (kadoban/cherim)
* MCTS (monte carlo tree search), code template is in mcts_punter.py, just needs random_playout implemented
* policy function (move evaluation) using up to about 50 features, optimize weights by self-play
* then able to guide the MCTS and/or alpha-beta search using the policy function
* tit for tat: try a symmetric strategy (nim-like)
* should we limit the opponent's opportunities as much as possible?
* greedy

### Greedy Ideas

* Do a BFS from each mine, labeling each node as its squared distance from the mine. Then during gameplay, we try to obtain the highest value nodes
* Make an optimal play at each step by score
* Make an optimal play at each step by score+mineDistance
* Make a greedy algorithm that has tuneable co-efficients for optimizing

### River Features for an unclaimed river
* go nuts here with ideas, because we could use up to about 50 features
* (can probably afford n times BFS)
* -- any indicator that hints anything about how valuable a river might be

1. couldntPossiblyConnectToAMine/orphaned/unreachable? -- able to detect/rule out using BFS/DFS
2. directScoreIncreaseValue
3. directOpponentScoreDenialValue
4. isBridge?/bridgeValue/isTheOnlyRiverToSubGraphConnectedComponentOfSizeS https://en.wikipedia.org/wiki/Bridge_(graph_theory)
5. isAdjacentToMine?
6. wouldLink2Mines?
7. distanceFromNearestMineToIt
8. numRiversItWouldLink
9. totalDistanceFromAllOfYourRivers
10. totalDistanceFromAllOfEnemyRivers
11. numNodesReachableFromNewlyAccessibleNode
12. fanOutFromHeadPlusTail
13. totalDistanceFromAllMines
14. howLikelyToLinkMines - ?how to compute
15. ? -- ?fast enough

###### Terms
* -r-: unclaimed river/blue river r
* =r=: your river/green river r
* xrx: enemy river/grey river r
* "link" (nn/vb): "is-claimed/claim an adjacent edge between" eg: 1=r=2  r is a link from 1 to 2
* "bridge" (nn/vb): "is-reachable/make reachable (connected)" eg 1===2-r-3===4 r is an unclaimed bridge from 1 to 4

O(3632) rivers in examples, 2s movetimelimit .'. features in O(V^2)
