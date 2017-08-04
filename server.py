import sys
import threading
import json
import time

import SocketServer
GAME_PORT=9000

import game

GAME = None
class ThreadedTCPServer(SocketServer.ThreadingMixIn, SocketServer.TCPServer):
    def log_request(self, *args, **kwargs):
        pass

class GameHandler(SocketServer.StreamRequestHandler):
    def _recv(self):
        msg = self.request.recv(10**9)
        print '<<<', msg
        return json.loads(msg.split(':', 1)[1])

    def _send(self, msg):
        msg = json.dumps(msg)
        msg = '{}:{}'.format(len(msg), msg)
        print '>>>', msg
        self.wfile.write(msg + '\n')
        self.wfile.flush()

    def handle(self):
        print "RECEIVED NEW CLIENT"


        # the first thing every client does is identify

        try:
            client_id = self._recv()
            print "CLIENT ID RECEIVED", client_id

            if "me" in client_id:
                player_id = client_id["me"]
                self._send({ "you" : player_id })
                print "PLAYER HANDSHAKE FINISHED", player_id
                GAME.add_player(player_id, self)
            else:
                print "INITIAL MSG DID NOT CONTAIN 'me' HANDSHAKE, EXITING REQUEST"
                return

        except Exception, e:
            print e
            print "CLIENT DID NOT ESTABLISH HANDSHAKE PROPERLY, EXITING REQUEST"
            return



        self.running = True
	while self.running:
            # TODO: sleep until our game is over
            time.sleep(0.5)
        print "ENDING GAME FOR THREAD", self


SERVER=None
def serve_game(game_port, HandlerClass = GameHandler):
    global SERVER
    SocketServer.TCPServer.allow_reuse_address = True
    gamed = ThreadedTCPServer(("", game_port), HandlerClass)
    print("Serving Game on", game_port)

    SERVER = gamed
    SERVER.serve_forever()

def start():
    port = int(GAME_PORT)

    def run_gameserve():
        serve_game(port)

    game_thread = threading.Thread(target=run_gameserve)
    game_thread.daemon = True
    game_thread.start()


    return game_thread

MAP="maps/lambda.json"
PLAYERS=1
def main():
    global GAME
    with open(MAP, "r") as f:
        data = f.read()

    json_map = json.loads(data)
    print "LOADING SERVER..."
    print "LOADING MAP", MAP
    print "MAKING GAME"
    GAME = game.Game(json_map, PLAYERS)
    print "EXPECTED PLAYERS", PLAYERS

    t = start()

    while True:
        t.join(0.5)

        if not t.isAlive():
            print "GAME SERVER DIED, EXITING"
            break


if __name__ == "__main__":
    main()
