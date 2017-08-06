# credit: https://www.ics.uci.edu/~eppstein/PADS/UnionFind.py

def new():
    """Create a new empty union-find structure."""
    forest = {}
    forest['weights'] = {}
    forest['parents'] = {}
    return forest

def find(forest, obj):
    """Find and return the name of the set containing the object."""
    # check for previously unknown object
    if obj not in forest['parents']:
        forest['parents'][obj] = obj
        forest['weights'][obj] = 1
        return obj
    # find path of objects leading to the root
    path = [obj]
    root = forest['parents'][obj]
    while root != path[-1]:
        path.append(root)
        root = forest['parents'][root]
    # compress the path and return
    for ancestor in path:
        forest['parents'][ancestor] = root
    return root

def union(forest, *objs):
    """Find the sets containing the objects and merge them all."""
    roots = [find(forest, x) for x in objs]
    heaviest = max([(forest['weights'][r], r) for r in roots])[1]
    for r in roots:
        if r != heaviest:
            forest['weights'][heaviest] += forest['weights'][r]
            forest['parents'][r] = heaviest
