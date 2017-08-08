# this is the actual game handling code
import time
import datetime
import threading
import json
import os
from score import score_game
from util import *

helpers = __import__("py-client").helpers

class Player():
    def __init__(self):
        pass

    def get_move(self, prev_round):
        #print "PROMPTING PLAYER FOR MOVE", self.id, "PREV ROUND:", prev_round
        start = time.time()
        self.request._send({
            'move': {'moves': prev_round},
            'settings': {'futures': True, 'options': True}
        })
        msg = self.request._recv()
        if 'state' in msg:
            del msg['state']
        end = time.time()
        print "{} - {}> {}".format(
            datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S'),
            self.name,
            end - start)
        return msg

    def get_ready(self, punters, game_map):
        #print "Player setup", self.id
        self.request._send({
            'punter': self.id,
            'punters': punters,
            'map': game_map,
            'settings': {'futures': True, 'options': True}
        })
        return self.request._recv()

    def stop(self, round, scores):
        self.request._send({"stop": {
            "moves": round,
            "scores": scores
        }})


class Game():
    def __init__(self, maps, num_players):
        #print "LOADING SERVER..."
        print "Server up for {} players", num_players

        self.l = threading.Lock()
        self.players = {}
        self.maps = maps
        self.current_map = 0
        self.num_maps = len(self.maps)
        self.num_players = num_players

    def add_player(self, player_name, player_request):
        with self.l:
            pid = len(self.players)
            player = Player()
            player.name = player_name
            player.id = pid
            player.request = player_request
            self.players[pid] = player
            print "BEFORE:", len(self.players), "PLAYERS IN THE GAME"
            print "ADDING PLAYER", player, player.id, player.name
            print "NOW THEY are", self.players, len(self.players), "PLAYERS IN THE GAME"


            if len(self.players) == self.num_players:
                game_thread = threading.Thread(target=self.start)
                game_thread.start()

            return player

    def start(self):
        game_map = self.maps[self.current_map % self.num_maps]
        self.current_map += 1
        with open(game_map, "r") as f:
            data = f.read()
        self.map = json.loads(data)
        print "{} - map> {}, rivers: {}, sites: {}, mines {}".format(
            datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S'),
            game_map,
            len(self.map["rivers"]) if 'rivers' in self.map else 0,
            len(self.map["sites"]) if 'sites' in self.map else 0,
            len(self.map["mines"]) if 'mines' in self.map else 0
        )
        players = len(self.players)
        self.all_turns = []
        self.futures = []

        for i, player in self.players.iteritems():
            msg = player.get_ready(players, self.map)
            futures = msg.get('futures', [])
            for f in futures:
                f['punter'] = i
                self.futures.append(f)

        turns = len(self.map["rivers"]) if 'rivers' in self.map else 0

        player_turn = 0
        round = [{"pass": {"punter": player}} for player in range(1, players)]
        for i in range(turns):
            player = self.players[i % players]
            turn = {"pass": {"punter": player_turn}}
            try:
                turn = player.get_move(round)
            except Exception, e:
                print "PLAYER", player.id, "HAD ERROR. PASSING TURN", i
                print " ", e

            round.append(turn)
            self.all_turns.append(turn)

            if len(round) > players:
                round.pop(0)

            # end of turn book keeping
            player_turn += 1

        mines = self.map['mines'] if 'mines' in self.map else []
        rivers = self.map['rivers'] if 'rivers' in self.map else []
        scores = [{"punter": p, "score": s}
                  for p, s in zip(range(players),
                                  score_game(mines,
                                      conv_rivers(rivers),
                                      to_claimed_rivers(self.all_turns)))]
        #for k, s in zip(sorted(self.players.keys()), scores):
        print '{} - score> {}'.format(
            datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S'),
            ', '.join(['{}: {}'.format(self.players[d['punter']].name, d['score'])
                       for d in scores]))
        players = self.players.values()
        for p in sorted(players):
            p.stop(round, scores)
            player.request.running = False

        # TODO: evaluate the actual game
        self.save_game(filename='output/' + os.path.basename(game_map))

        # we reset players after saving game, bc we still need their names
        self.players = {}


    def save_game(self, filename="output/game.json"):
        try:
            os.makedirs("output")
        except OSError:
            pass

        json_obj = {
            "futures": self.futures,
            "turns" : self.all_turns,
            "num_players" : len(self.players),
            "players" : [ p.name for i,p in self.players.iteritems() ],
            "map" : self.map
        }
        json_str = json.dumps(json_obj)

        with open(filename, "w") as f:
            f.write(json_str)

        with open("output/game.json", "w") as f:
            f.write(json_str)

        print "{} - save> {} ({})".format(
            datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S'),
            helpers.game_url("game.json"),
            len(json_str))
