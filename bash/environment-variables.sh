export BASH_SILENCE_DEPRECATION_WARNING=1

export DIRENV_LOG_FORMAT=

export EDITOR=emacsclient

export LANG=en_US.UTF-8

export VIRTUALENV_CONFIG_FILE=/code/dotfiles/virtualenv.ini

parse_git_branch() {
     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1) /'
}

export PS1="\[\033[32m\]\$(parse_git_branch)\[\033[00m\]\u@\h:\w$ "

PV=/Library/Frameworks/Python.framework/Versions

export PATH=\
/Users/tom/bin:\
/code/gitz:\
/code/dotfiles/bash/:\
/code/engora-search/scripts:\
$PV/2.7/bin:\
$PV/3.6/bin:\
$PV/3.7/bin:\
$PV/3.8/bin:\
$PV/3.9/bin:\
$PV/3.10/bin

if [[ "$(/bin/hostname)" == "bantam.local" ]]; then

    export PATH=$PATH:\
/Applications/Postgres.app/Contents/Versions/13/bin:\
/usr/local/opt/openssl/bin:\
/usr/local/bin

elif [[ "$(/bin/hostname)" == "bolt.local" ]]; then
    export PATH=$PATH:\
/opt/homebrew/bin:\
/opt/homebrew/opt/postgresql@13/bin:\
/usr/local/bin

fi

export PATH=$PATH:\
/usr/bin:\
/bin:\
/usr/sbin:\
/sbin

# export PYTHONSTARTUP=~/bin/python_startup.py
