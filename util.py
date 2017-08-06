
def conv_river(r):
    return (r[u'source'], r[u'target'])

def conv_rivers(l):
    return map(conv_river, l)

def to_claimed_rivers(l):
    punters = {}
    for x in l:
        if 'claim' in x:
            v = x[u'claim']
            punters.setdefault(v[u'punter'], []).append(conv_river(v))
        if 'option' in x:
            v = x[u'option']
            punters.setdefault(v[u'punter'], []).append(conv_river(v))
        if 'pass' in x:
            punters.setdefault(x[u'pass'][u'punter'], [])


    res = []
    for k in sorted(punters.keys()):
        res.append(punters[k])
    return res
