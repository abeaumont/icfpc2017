# to run a game

* game server: `python server.py`
* use `-m` to specify map and use `-n` to specify number of players.
* choose the AI you want to run, then run them: `python py-client/rand_punter.py`. Make sure to run enough so the server starts the game
* a new file game.json will have been generated in output/
* run `bash scripts/runviewer.sh` to open a webpage and watch the game JSON. load a custom JSON via the text input box

or
```
$ bash play.sh <bot1> <bot2>   
## defbot = py-clients/rand_punter.py   defmap = maps/circle.json  gonna do specified maps later
```

