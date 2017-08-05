# this is the actual game handling code
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
        print "PROMPTING PLAYER FOR MOVE", self.id, "PREV ROUND:", prev_round
        self.request._send({"move": {"moves": prev_round}})
        return self.request._recv()

    def get_ready(self, punters, game_map):
        print "Player setup", self.id
        self.request._send({
            'punter': self.id,
            'punters': punters,
            'map': game_map})
        return self.request._recv()

    def stop(self, round, score):
        self.request._send({"stop": {
            "moves": round,
            "score": score
        }})


class Game():
    def __init__(self, game_map, num_players):
        with open(game_map, "r") as f:
            data = f.read()

        json_map = json.loads(data)
        print "LOADING SERVER..."
        print "LOADING MAP", game_map
        print "MAKING GAME"
        print "EXPECTED PLAYERS", num_players

        self.l = threading.Lock()
        self.players = {}
        self.map = json_map
        self.num_players = num_players

        print "MAP HAS %s RIVERS" % (len(json_map["rivers"]))
        print "MAP HAS %s SITES" % (len(json_map["sites"]))
        print "MAP HAS %s MINES" % (len(json_map["mines"]))

    def add_player(self, player_name, player_request):
        with self.l:
            pid = len(self.players)
            player = Player()
            player.name = player_name
            player.id = pid
            player.request = player_request
            self.players[pid] = player
            print "ADDING PLAYER", player, player.id, player.name


            if len(self.players) == self.num_players:
                game_thread = threading.Thread(target=self.start)
                game_thread.start()

            return player

    def start(self):
        print "SPINNING UP GAME SERVER!"
        game_map = self.map
        players = len(self.players)
        self.all_turns = []

        for player in self.players.values():
            player.get_ready(players, game_map)
        turns = len(game_map["rivers"])

        player_turn = 0
        round = []
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
        scores = score_game(self.map[u'mines'],
                            conv_rivers(self.map[u'rivers']),
                            to_claimed_rivers(self.all_turns))
        for k, s in zip(sorted(self.players.keys()), scores):
            round.append(self.players[k].stop(round, s))
            round.pop(0)

        print "ENDING GAME!"
        for player in self.players.values():
            player.request.running = False

        # TODO: evaluate the actual game
        self.save_game()
        self.players = {}


    def save_game(self, filename="output/game.json"):
        try:
            os.makedirs("output")
        except OSError:
            pass

        json_obj = {
            "turns" : self.all_turns,
            "num_players" : len(self.players),
            "map" : self.map
        }
        json_str = json.dumps(json_obj)
        with open(filename, "w") as f:
            f.write(json_str)
        print "SAVED GAME TO", helpers.game_url(filename), "SIZE IS", len(json_str)
