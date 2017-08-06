def set(x): return [x, 0]

def union(x, y):
    xroot = find(x)
    yroot = find(y)
    if xroot[1] > yroot[1]:
        yroot[0] = xroot
    elif xroot[1] < yroot[1]:
        xroot[0] = yroot
    elif xroot != yroot:
        yroot[0] = xroot
        xroot[1] += 1
    
def find(x):
    if isinstance(x[0], int):
        return x
    else:
        x[0] = find(x[0])
        return x[0]
