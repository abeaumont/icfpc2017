#!bin/bash

#if [ $1 = "-h" ]; then
  #echo "usage: bash play.sh <solver1> <solver2>"
  ##echo "verbose output: bash play.sh -v <solver1> <solver2>"
  #echo "default solver: py-client/ran_punter.py"
#fi

echo
echo "${1:-py-client/rand_punter.yp} (0)"
echo "vs"
echo "${2:-py-client/rand_punter.py} (1)"

#if [ $1 = "-v" ]; then
  # verbose
  xterm -e python server.py & 
  sleep 1
  xterm -e bash -c 'python ${2:-py-client/rand_punter.py}; read -r' &
  xterm -e bash -c 'python ${3:-py-client/rand_punter.py}; read -r' &
  xterm -e bash -c 'sh scripts/runviewer.sh; read -r'
#else
  ## quiet (default)
  #xterm -e python server.py & 
  #sleep 1
  #xterm -e python ${1:-py-client/rand_punter.py} & # arg1 or def
  #xterm -e python ${2:-py-client/rand_punter.py} & # arg2 or def
  #xterm -e python scripts/runviewer.sh
#fi

# love from jkhl
