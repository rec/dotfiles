#
# Python virtualenv tools.
#

# Create a new virtualenv.
function new_env() {
    virtualenv /development/env/$1 -p python3 && penv $1
}

# Activate a virtualenv.
function penv() {
    source /development/env/$1/bin/activate
    export PS1="[$1] $PS1_GIT $PS1_ORIGINAL"
}

function nenv() {
    deactivate
    export PS1="$PS1_GIT $PS1_ORIGINAL"
}
