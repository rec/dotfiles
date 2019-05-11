#!/bin/bash

PWD=`pwd`
NAME=`basename $PWD`

source ~/.bashrc

echo "penv $NAME"
penv $NAME

set -Eeuxo pipefail

find . -name \*.py | xargs /code/env/black/bin/black -l 79 -S
mypy $NAME test
flake8  # $NAME test
pytest test
