debug = False

def enable_logging():
    global debug
    debug = True

def log(message, *args):
    global debug
    if debug:
        print >>sys.stderr, message % map(str, args)
