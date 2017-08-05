import argparse
import signal
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
        s = ''
        c = self.rfile.read(1)
        while c != ':':
            s += c
            c = self.rfile.read(1)
        msg = self.rfile.read(int(s))
        print '<<<', msg
        return json.loads(msg)

    def _send(self, msg):
        msg = json.dumps(msg)
        msg = '{}:{}'.format(len(msg), msg)
        print '>>>', msg
        self.wfile.write(msg)
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

def main():
    parser = argparse.ArgumentParser(description='Game Server.')
    parser.add_argument('-m', '--map', default='maps/circle.json', help='map to use')
    parser.add_argument('-n', type=int, default=2, help='number of players')
    args = parser.parse_args()
    MAP = args.map
    PLAYERS = args.n
    global GAME
    GAME = game.Game(MAP, PLAYERS)
    t = start()

    while True:
        t.join(0.5)

        if not t.isAlive():
            print "GAME SERVER DIED, EXITING"
            break

def signal_handler(signal, frame):
    print("Stopping the server")
    for player in GAME.players.itervalues():
        player.request.running = False

    sys.exit(0)
signal.signal(signal.SIGINT, signal_handler)

if __name__ == "__main__":
    main()
