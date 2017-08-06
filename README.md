*NOTE* Do _not_ use `print` for debugging purposes.
Offline mode uses stdio for bot communication so it won't work.
use `self.log` instead, like:
```python
self.log('whatever: {} and {}'.format(a, b))
```

# to run a game

* game server: `python server.py`
* use `-m` to specify map and use `-n` to specify number of players.
* choose the AI you want to run, then run them: `python py-client/rand_punter.py`. Make sure to run enough so the server starts the game
* a new file game.json will have been generated in output/
* run `bash scripts/runviewer.sh` to open a webpage and watch the game JSON. load a custom JSON via the text input box

or
```
bash play.sh <bot1>                   ## plays bot1 vs defbot
bash play.sh <bot1> <bot2>            ## defbot = py-clients/rand_punter.py
bash play.sh -m <map> <bot1> <bot2>   ## defmap = maps/circle.json
```

# to run a game in offline mode

* you need lambda-duct installed
* you can use either our server or online servers
* game server: `python server.py`
* client against local server (from `py-client`): `lamduct --game-hostname localhost --game-port 9000 --log-level 3 ./rand.sh`
* client against remote server (from `py-client`): `lamduct --game-port 9009 --log-level 3 ./rand.sh`


# To run a game with different maps

`python tournament.py [-n players] [-m mapdir]`

This will create a server that iterates through the maps in `mapdir`.
Just connect bots normally.


