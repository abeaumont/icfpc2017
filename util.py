
def conv_river(r):
    return (r[u'source'], r[u'target'])

def conv_rivers(l):
    return map(conv_river, l)

def to_claimed_rivers(l):
    punters = {}
    for x in l:
        try:
          v = x[u'claim']
          punters.setdefault(v[u'punter'], []).append(conv_river(v))
        except KeyError:
          punters.setdefault(x[u'pass'][u'punter'], [])
    res = []
    for k in sorted(punters.keys()):
        res.append(punters[k])
    return res
