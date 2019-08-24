#!/bin/bash

PWD=`pwd`
NAME=`basename $PWD`
BIN=/code/env/clean/bin

source ~/.bashrc

echo "penv $NAME"
penv $NAME

set -Eeuxo pipefail

$BIN/black -l 79 -S
$BIN/autoflake -ir .
