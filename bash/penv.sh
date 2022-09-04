#
# Bash tools to manage Python virtualenvs.
# There is a "default virtualenv" which you can set
# and clear with set-denv and clear-denv - and activate with denv.

#
# The following environment variables are used:
#
# PENV_ROOT
#   Directory containing all the virtualenv data
#   Default ~/.virtualenvs
#   Each virtualenv take at least 10MB and often above 100MB of disk space
#
# PENV_PYTHON
#    The default version of Python
#    Default python3
#
# PENV_DEFAULT:
#   Store the current default virtualenv
#   Default ~/.default_penv

penv-root() {
    echo ${PENV_ROOT:-~/.virtualenvs}
}

# Activate a virtualenv.
penv() {
    [[ -z $1 ]] && echo "ERROR: penv needs an argument" && return -1
    source `penv-root`/$1/bin/activate
    export PS1="[$1] $PS1_GIT $PS1_ORIGINAL"
    export VIRTUALENV=$1
}

# Quit a virtualenv
qenv() {
    [[ -z $VIRTUALENV ]] && return 0
    deactivate
    export PS1="$PS1_GIT $PS1_ORIGINAL"
    export VIRTUALENV=
}

# Create a new virtualenv.
new-env() {
    [[ -z $1 ]] && \
        echo "USAGE: new-env <env-name> [<python-version>]" && \
        return -1
    qenv

    PYTHON=${2:-${PENV_PYTHON:-python3}}

    echo $PYTHON
    echo "python3.8 -m virtualenv `penv-root`/$1 -p $PYTHON"
    mkdir -p `penv-root` && \
        echo one && \
        python3.8 -m virtualenv `penv-root`/$1 -p $PYTHON && \
        echo two && \
        penv $1 && \
        echo three && \
        pip install --upgrade setuptools && \
        pip install --upgrade pip && \
        pip install --upgrade wheel
}

nenv() {
    new-env $1 $2 && reqs
}

reqs() {
    for i in requirements.txt *_requirements.txt; do
        [ -f "$i" ] || break
        echo "-> Installing from $i"
        pip install -Ur $i
    done
}

newp() {
    [[ -z $1 ]] && echo "ERROR: new-penv needs an argument" && return -1
    delete-env $1
    new-env $1 $2 && reqs
}

list-env() {
    # List the virtual environments
    ls -1 `penv-root`
}

delete-env() {
    # Delete an virtualenv with no recourse!
    [[ -z $1 ]] && \
        echo "ERROR: delete-env needs an argument" && \
        return -1
    qenv
    for i in $@
    do
        rm -R `penv-root`/$i/
    done
}

archive-env() {
    # Archive virtualenvs
    [[ -z $1 ]] && \
        echo "ERROR: archive-env needs an argument" && \
        return -1
    qenv
    for i in $@
    do
        penv $i
        DIR=`penv-root`/.archive/$i
        mkdir -p $DIR
        pip freeze > $DIR/requirements.txt
        python --version freeze > $DIR/version.txt
    done
}

clear-denv() {
    # Clear the default virtualenv
    DENV=${PENV_DEFAULT:-~/.default_penv}
    rm -f $DENV
    echo "$DENV cleared"
    qenv
}

denv() {
    # Start or set and start the default virtualenv
    DENV=${PENV_DEFAULT:-~/.default_penv}
    if [ "$1" == "" ]; then
        if [ -s $DENV ]; then
            penv `cat $DENV`
        fi
    else
        penv "$1" && \
            echo "$1" > $DENV
    fi
}
