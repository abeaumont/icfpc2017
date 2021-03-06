#!bin/bash
map="maps/circle.json" # def
echo "<C-c> here to close stuff"

#if [ $1 = "-h" ]; then
  #echo "usage: bash play.sh <solver1> <solver2>"
  ##echo "verbose output: bash play.sh -v <solver1> <solver2>"
  #echo "default solver: py-client/ran_punter.py"
#fi

while test $# -gt 0; do
  case "$1" in
    -m)
      shift
      map=$1
      echo "map: $1"
      echo
      shift
      ;;
    -n)
      shift
      echo "$1 players"
      echo
      shift
      ;;
    *)
      break
      ;;
  esac
done

echo "${1:-py-client/rand_punter.py} (0) (blue)"
echo "          vs"
echo "${2:-py-client/rand_punter.py} (1) (grey)"

#if [ $1 = "-v" ]; then
  # verbose
  xterm -e python server.py --map $map &
  #xterm -e python server.py & 
  sleep 1
  xterm -e bash -c "python ${1:-py-client/rand_punter.py}; read -r" &
  xterm -e bash -c "python ${2:-py-client/rand_punter.py}; read -r" &
  xterm -e bash -c "sh scripts/runviewer.sh; read -r"
#else
  ## quiet (default)
  #xterm -e python server.py & 
  #sleep 1
  #xterm -e python ${1:-py-client/rand_punter.py} & # arg1 or def
  #xterm -e python ${2:-py-client/rand_punter.py} & # arg2 or def
  #xterm -e python scripts/runviewer.sh
#fi
