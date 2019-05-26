#!/bin/bash

PWD=`pwd`
NAME=`basename $PWD`

source ~/.bashrc

echo "penv $NAME"
penv $NAME

set -Eeuxo pipefail

if [ $@ ] ; then
    NAME=$@
else
    NAME="$NAME test"
fi

find . -name \*.py | xargs /code/env/black/bin/black -l 79 -S
# mypy $NAME
flake8  # $NAME test
pytest test
