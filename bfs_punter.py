import json
import os
import interface
import uuid

class RandPunter(interface.Punter):
    def __init__(self, name, init_state, fname=None):
        super(RandPunter, self).__init__(name, init_state)
        self.fname = fname if fname is not None else str(uuid.uuid4())
        self.all_turns = []
        self.available_rivers = {(r['source'], r['target']) for r in self.rivers}
        
    def turn(self, state):
        self.log("state: %s", state)
        self.all_turns.extend(state['move']['moves'])
        for m in state['move']['moves']:
            if 'claim' in m:
                s = m['claim']['source']
                t = m['claim']['target']
                self.available_rivers.discard((s, t))
                self.available_rivers.discard((t, s))
        if len(self.available_rivers) == 0:
            return self.pass_()
        else:
            return self.claim(*self.available_rivers.pop())

    def stop(self, state):
        try:
            os.makedirs("output")
        except OSError:
            pass

        json_obj = {
            "turns" : self.all_turns,
            "num_players" : self.punters
        }
        json_str = json.dumps(json_obj)
        fname = os.path.join('output', self.fname + '.json')
        if not os.path.exists('output'):
            os.makedirs('output')
        with open(fname, "w") as f:
            f.write(json_str)
        print "SAVED GAME TO", fname, "SIZE IS", len(json_str)
        

if __name__ == '__main__':
    import socket, sys
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    if len(sys.argv) < 2:
        s.connect(('localhost', 9000))
    else:
        s.connect(('punter.inf.ed.ac.uk', int(sys.argv[1])))
    iface = interface.Interface("Random", RandPunter, s.makefile())
    iface.run()

    
    """
    
def add_move(src, sqr):
  if sqr != None: 
    q.put(sqr)
    if parent.get(sqr) == None: parent[sqr] = src

def enqueue_succs(src):
  pos = index2d(grid.squares, src)
  add_move( src, offs(pos, -2, -1) )
  add_move( src, offs(pos, -2,  1) )
  add_move( src, offs(pos, -1, -2) )
  add_move( src, offs(pos, -1,  2) )
  add_move( src, offs(pos,  1, -2) )
  add_move( src, offs(pos,  1,  2) )
  add_move( src, offs(pos,  2, -1) )
  add_move( src, offs(pos,  2,  1) )

parent = {} 
grid = Grid()
q = Queue()

def answer(src, dst):
  global parent, q # ...
  #print src, dst
  parent = {}
  q = Queue()
  # bfs
  q.put(src)
  while not q.empty():
    print q.queue, '\n', parent, '\n'
    node=q.get()
    if node==dst: break
    enqueue_succs(node)

  mvs = 0
  node = dst
  while node != src:
    #print node
    node = parent.get(node)
    mvs+=1
  print mvs
  return mvs
  
"""