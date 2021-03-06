#!/usr/bin/env python

import os

NAMES = {'BiblioPixel': 'dev'}


def get_branch():
    parts = os.getcwd().split('/')  # *nix only.
    try:
        while not parts[0]:
            parts.pop(0)
        if parts[0] == 'code':
            return NAMES[parts[1]]
    except:
        pass
    return 'master'


if __name__ == '__main__':
    print(get_branch())
