#
# Python virtualenv tools.
#

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
    echo "one"
    [[ -z $1 ]] && \
        echo "ERROR: new-env needs an argument" && \
        return -1
    echo "two"
    qenv
    echo "three"


    if [ "$2" == "" ]; then
        echo "four"
        PYTHON=python3.4
    else
        echo "five"
        PYTHON=$2
    fi


    echo "six $PYTHON"
    virtualenv /development/env/$1 -p $PYTHON && \
        penv $1 && \
        pip install --upgrade pip
    echo "five"

}

function list-env() {
    ls -1 /development/env
}

function delete-env() {
    qenv
    [[ -z $1 ]] && \
        echo "ERROR: delete-env needs an argument" && \
        return -1
    for i in $@
    do
        rm -R /development/env/$i/
    done
}
