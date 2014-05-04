#!/bin/sh

EXTENSIVE=0

if [ $# -gt 0 ]; then
    if [ "$1" -eq "1" ]; then
        EXTENSIVE=1
    fi
fi

for i in `seq -10000000000 29012901 10000000000`
do
    date -u "+%s:%Y:%m:%d:%T:%A:%j" --date="@$i"
done

if [ ${EXTENSIVE} -eq 1 ]; then
    for i in `seq 362790000 1 362797200`
    do
        date -u "+%s:%Y:%m:%d:%T:%A:%j" --date="@$i"
    done
fi
