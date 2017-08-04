
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
