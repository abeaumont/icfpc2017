# this is the actual game handling code
import threading
import json

class Player():
    def __init__(self):
        pass

    def get_move(self, prev_round):
        print "PROMPTING PLAYER FOR MOVE", self.id, "PREV ROUND:", prev_round
        self.request.wfile.write(json.dumps(prev_round))
        move = json.loads(self.request.rfile.readline())
        

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
        num_turns = len(game_map["rivers"])

        moves = []

        turn_num = 0
        round_num = 0
        player_turn = 0
        prev_round = { }
        for pid in self.players:
            prev_round[pid] = "na"

        this_round = {}
        while turn_num < num_turns:
            player = self.players[player_turn]
            try:
                this_round[player.id] = player.get_move(prev_round)
            except Exception, e:
                print "PLAYER", player.id, "HAD ERROR. PASSING TURN", turn_num
                print " ", e
                this_round[player.id] = "na"


            # end of turn book keeping
            player_turn += 1 
            if player_turn == self.num_players:
                round_num += 1
                player_turn = 0

                prev_round = this_round
                this_round = {}

            turn_num += 1
            # TODO: save the game state for every player on each turn or something?

        print "ENDING GAME!"
        # TODO: evaluate the actual game
