#
# Python virtualenv tools.
#

# Create a new virtualenv.
function new-env() {
    virtualenv /development/env/$1 -p python3 && \
        penv $1 && \
        pip install --upgrade pip
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

function qenv() {
    [[ -z $VIRTUALENV ]] && \
        echo "ERROR: not in a virtualenv" && \
        return -1
    deactivate
    export PS1="$PS1_GIT $PS1_ORIGINAL"
    export VIRTUALENV=
}

function lenv() {
    ls -1 /development/env
}

function delete-env() {
    [[ -z $1 ]] && \
        echo "ERROR: delete-env needs an argument" && \
        return -1
    rm -R /development/env/$1/
}
