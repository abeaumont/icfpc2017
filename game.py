# this is the actual game handling code
import threading
import json

class Player():
    def __init__(self):
        pass

    def get_move(self, prev_round):
        print "PROMPTING PLAYER FOR MOVE", self.id, "PREV ROUND:", prev_round
        self.request.wfile.write(json.dumps({"move": {"moves": prev_round}}) + '\n')
        return json.loads(self.request.rfile.readline())

    def get_ready(self, punter, punters, game_map):
        print "Player setup", self.id
        self.request.wfile.write(json.dumps({
            'punter': punter,
            'punters': punters,
            'map': game_map}) + '\n')
        line = self.request.rfile.readline()
        print line
        return json.loads(line)
        

class Game():
    def __init__(self, game_map, num_players):
        self.l = threading.Lock()
        self.players = {}
        self.map = game_map
        self.num_players = num_players

        print "MAP HAS %s RIVERS" % (len(game_map["rivers"]))
        print "MAP HAS %s SITES" % (len(game_map["sites"]))
        print "MAP HAS %s MINES" % (len(game_map["mines"]))

    def add_player(self, player_name, player_request):
        with self.l:
            pid = len(self.players)
            player = Player()
            player.name = player_name
            player.id = pid
            player.request= player_request
            self.players[pid] = player
            print "ADDING PLAYER", player, player.id, player.name


            if len(self.players) == self.num_players:
                game_thread = threading.Thread(target=self.start)
                game_thread.start()

            return player
        
    def start(self):
        print "SPINNING UP GAME SERVER!"
        game_map = self.map
        for pid in self.players:
            self.players[pid].get_ready(pid, len(self.players), game_map)
        turns = len(game_map["rivers"])
        players = len(self.players)

        moves = []
        player_turn = 0
        round = []
        for i in range(turns):
            player = self.players[i % players]
            try:
                round.append(player.get_move(round))
            except Exception, e:
                print "PLAYER", player.id, "HAD ERROR. PASSING TURN", turn_num
                print " ", e
                round.append({"pass": {"punter": player_turn}})
            if len(round) > players:
                round.pop(0)

            # end of turn book keeping
            player_turn += 1 

        print "ENDING GAME!"
        for player in self.players.values():
            player.request.running = False

        # TODO: evaluate the actual game
