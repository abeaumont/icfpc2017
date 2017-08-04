## TODO:

* [x] download maps from http://punter.inf.ed.ac.uk/maps/
* [-] implement online game server 
* [ ] add scoring to game server
* [ ] add skeleton for online client
* [ ] add skeleton for offline client
* [ ] make game server robust
* [ ] add random online client
* [ ] add greedy online client
* [ ] brainstorm strategies
* [ ] understand the limitations of what we can store in game state


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
* MCTS
* Greedy rules based on how valuable each river is. Probably doomed unless we
  come up with clever rules.
