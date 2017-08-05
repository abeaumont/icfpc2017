import interface
import random

def play_the_rest_of_the_game_randomly():
    # TODO 
    # try to put a policy (move evaluation/probability) function in here too, 
    # one that gives each move a random score for now,
    # so that later we can put a smarter policy
    pass

def won_the_simulation():
    #TODO
    return True

def MCTS(state):
    if state.numberOfSimulations == 0:

        ####################################
        play_the_rest_of_the_game_randomly()
        ####################################

        state.numberOfSimulations += 1
        ########################
        if won_the_simulation():
        ########################
            return "won"
        else:
            state.wonGames += 1
            return "lost"

    bestScore = -infinity
    ##############################
    for move in state.get_moves(): ## self.available_rivers
    ##############################
        childState = state.makeMove(move)
            if childState.numberOfSimulations == 0:
                moveScore = infinity
            else:
                moveScore = childState.wonGames/childState.numberOfSimulations + sqrt(2*log(state.numberOfSimulations)/childState.numberOfSimulations)
            if moveScore > bestScore:
                bestScore = moveScore
                bestMove = move
    simulationResult = MCTS(state.makeMove(bestMove))
    state.numberOfSimulations += 1
    if simulationResult == "won":
        state.wonGames += 1
        return "lost"
    else:
        return "won"

class RandPunter(interface.Punter):
    def __init__(self, name, init_state, fname=None):
        super(RandPunter, self).__init__(name, init_state)
        random.seed()
        
    def turn(self, state):
        self.log("state: %s", state)
        if len(self.available_rivers) == 0:
            return self.pass_()
        else:
            r = random.randint(0, len(self.available_rivers) - 1)
            for i in range(r):
                self.available_rivers.add(self.available_rivers.pop())
            return self.claim(*self.available_rivers.pop())

if __name__ == '__main__':
    import socket, sys
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    if len(sys.argv) < 2:
        s.connect(('localhost', 9000))
    else:
        s.connect(('punter.inf.ed.ac.uk', int(sys.argv[1])))
    iface = interface.Interface("Random", RandPunter, s.makefile())
    iface.run()