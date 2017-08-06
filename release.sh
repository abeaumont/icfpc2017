#!/bin/sh
CODE=924bdb4c-8d8d-4705-a32d-75c42e574c29
SRC=py-client
DEST=icfp-$CODE

mkdir -p $DEST/src
cp $SRC/*.py $DEST/
cp $SRC/*.py $DEST/src
tar -C $DEST/ -czvf $DEST.tar.gz --exclude='*.pyc' .
rm $DEST/*.py
rm $DEST/src/*.py
