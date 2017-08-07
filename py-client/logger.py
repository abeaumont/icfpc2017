import sys

debug = False

def enable_logging():
    global debug
    debug = True

def log(message, *args):
    global debug
    if debug:
        print >>sys.stdout, message % map(str, args)
