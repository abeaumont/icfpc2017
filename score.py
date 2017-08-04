from collections import deque

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
    seen, todo = set(), deque([(start, 0)])
    while todo:
      v, d = todo.popleft()
      if v not in seen:
        seen.add(v)
        yield (v, d)
        try:
          for c in rivers_m[v]:
            if c not in seen:
              todo.append((c, d+1))
        except KeyError:
          pass
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
        try:
          for c in rivers_m[v]:
            if c not in seen:
              todo.append(c)
        except KeyError:
          pass
    return seen
  vs = set(rivers_m.keys())
  acc = []
  while vs:
    res = go(vs.pop())
    for v in res:
      vs.discard(v)
    acc.append(res)
  return acc
