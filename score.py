
def score_game(mines, rivers, players):
  ''' mines: iterable of integer mines, in any order: [2, 5, 20]
      rivers: iterable of pairs of integers [(1, 2), (2, 5), (5, 3)],
              must have *all* rivers, not just the ones players chose.
      players: iterable of one iterable for each player, enumerating
               which rivers each player owns.

      result: iterable of integer scores for each player, in the same order
              as the 'players' iterable.
  '''
  # TODO: implement
  return [0 for _ in players]

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
