#
# Python virtualenv tools.
#

function denv() {
    if [ "$1" == "" ]; then
        default_env=`/development/dotfiles/python/default_env.py`
        if [ "$default_env" == "" ]; then
           return 0
        fi
    else
        /development/dotfiles/python/default_env.py $1
        default_env=$1
    fi
    penv $default_env
}

# Activate a virtualenv.
function penv() {
    [[ -z $1 ]] && \
        echo "ERROR: penv needs an argument" && \
        return -1
    source /development/env/$1/bin/activate
    export PS1="[$1] $PS1_GIT $PS1_ORIGINAL"
    export VIRTUALENV=$1
}

# Quit a virtualenv
function qenv() {
    [[ -z $VIRTUALENV ]] && return 0

    deactivate
    export PS1="$PS1_GIT $PS1_ORIGINAL"
    export VIRTUALENV=
}

# Create a new virtualenv.
function new-env() {
    [[ -z $1 ]] && \
        echo "ERROR: new-env needs an argument" && \
        return -1
    qenv


    if [ "$2" == "" ]; then
        PYTHON=python3.4
    else
        PYTHON=$2
    fi

    virtualenv /development/env/$1 -p $PYTHON && \
        penv $1 && \
        pip install --upgrade pip
}

function list-env() {
    ls -1 /development/env
}

function delete-env() {
    [[ -z $1 ]] && \
        echo "ERROR: delete-env needs an argument" && \
        return -1
    qenv
    for i in $@
    do
        rm -R /development/env/$i/
    done
}
