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

    mkdir -p `penv-root` && \
        virtualenv `penv-root`/$1 -p $PYTHON && \
        penv $1 && \
        pip install --upgrade setuptools && \
        pip install --upgrade pip && \
        pip install --upgrade wheel
}

nenv() {
    new-env $1 $2 && reqs
}

reqs() {
    for i in *requirements.txt; do
        [ -f "$i" ] || break
        echo "-> Installing from $i"
        pip install -Ur $i
    done
}

new-penv() {
    new-env $1 $2
    reqs
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

penv-root() {
    echo ${PENV_ROOT:-~/.virtualenvs}
}

# Code below is probably only useful to me.

old_denv() {
    # Start up the default environment
    if [ "$1" == "" ]; then
        default_env=`/code/dotfiles/python/default_env.py`
        if [ "$default_env" == "" ]; then
           return 0
        fi
    else
        /code/dotfiles/python/default_env.py $1
        default_env=$1
    fi
    penv $default_env
}

# Create a new virtualenv (historical purposes)
new-env-old-pip() {
    [[ -z $1 ]] && \
        echo "ERROR: new-env needs an argument" && \
        return -1
    qenv


    if [ "$2" == "" ]; then
        PYTHON=python3.4
    else
        PYTHON=$2
    fi

    virtualenv `penv-root`/$1 -p $PYTHON && \
        penv $1
}
