from collections import deque
import json
from util import *
import sys

def score_game(mines, rivers, players):
  ''' mines: iterable of integer mines, in any order: [2, 5, 20]
      rivers: iterable of pairs of integers [(1, 2), (2, 5), (5, 3)],
              must have *all* rivers, not just the ones players chose.
      players: iterable of one iterable for each player, enumerating
               which rivers each player owns.

      result: iterable of integer scores for each player, in the same order
              as the 'players' iterable.
  '''
  rivers_m = to_assoc_list(rivers)
  acc = [0 for _ in players]
  player_comps = [connected_components(to_assoc_list(x)) for x in players]
  def bfs(start):
    seen, todo = set([start]), deque([(start, 0)])
    while todo:
      v, d = todo.popleft()
      yield (v, d)
      for c in rivers_m.get(v, []):
        if c not in seen:
          todo.append((c, d+1))
          seen.add(c)
  for m in mines:
    ps = [filter(lambda comp: m in comp, x) for x in player_comps]
    for v, d in bfs(m):
      for i, p in enumerate(ps):
        try:
          if v in p[0]:
            acc[i] += d**2
        except IndexError:
          pass
  return acc

def to_assoc_list(rivers):
  res = {}
  def add_one(a, b):
    res.setdefault(a, set()).add(b)
  for x, y in rivers:
    add_one(x, y)
    add_one(y, x)
  return res

def connected_components(rivers_m):
  def go(start):
    seen, todo = set(), [start]
    while todo:
      v = todo.pop()
      if v not in seen:
        seen.add(v)
        for c in rivers_m.get(v, []):
          if c not in seen:
            todo.append(c)
    return seen
  vs = set(rivers_m.keys())
  acc = []
  while vs:
    res = go(vs.pop())
    for v in res:
      vs.discard(v)
    acc.append(res)
  return acc

test_cases = [
    ([1, 5],
     [(0, 1), (0, 7), (1, 2), (1, 3), (1, 7), (2, 3), (3, 4), (3, 5), (4, 5),
      (5, 6), (5, 7)],
     [[(2, 1), (2, 3), (4, 3)], [(1, 7), (7, 5)]],
     [6, 10])
    ]

def test_score_game():
  for mines, rivers, players, answer in test_cases:
    assert(score_game(mines, rivers, players) == answer)

def score_json(m):
  scores = score_game(m[u'map'][u'mines'],
                      conv_rivers(m[u'map'][u'rivers']),
                      to_claimed_rivers(m[u'turns']))
  return scores

if __name__ == "__main__":
  with open(sys.argv[1]) as f:
    print(score_json(json.load(f)))
