def game_url(fname):
    fname = fname.replace(".json", "").replace("output/", "")
    return "http://localhost:8000/viewer/?game=%s" % fname

