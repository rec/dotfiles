alias penv=workon
alias qenv=deactivate
alias list-env=lsvirtualenv
alias delete-env=rmvirtualenv

# Create a new virtualenv.
new-env() {
    [[ -z $1 ]] && \
        echo "USAGE: new-env <env-name> [<python-version>]" && \
        return -1
    qenv

    PYTHON=${2:-${PENV_PYTHON:-python3}}
    mkvirtualenv $1 -p $PYTHON
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
