#
# Python virtualenv tools.
#

# Create a new virtualenv.
function nenv() {
    virtualenv /development/env/$1 && penv $1
}

# Activate a virtualenv.
function penv() {
    source /development/env/$1/bin/activate
}
