
def conv_river(r):
    return (r[u'source'], r[u'target'])

def conv_rivers(l):
    return map(conv_river, l)

def to_claimed_rivers(l):
    punters = {}
    for x in l:
        v = x[u'claim']
        punters.setdefault(v[u'punter'], []).append(conv_river(v))
    res = []
    for k in sorted(punters.keys()):
        res.append(punters[k])
    return res
