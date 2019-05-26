#!/usr/bin/env python3
import os, sys

DEFAULT_FILE = os.path.expanduser('~/.default_env')


def default_env(name=None):
    if name is None:
        try:
            print(open(DEFAULT_FILE).read().strip())
        except:
            pass
    else:
        with open(DEFAULT_FILE, 'w') as fp:
            fp.write(name)
            fp.write('\n')


if __name__ == '__main__':
    default_env(*sys.argv[1:])
