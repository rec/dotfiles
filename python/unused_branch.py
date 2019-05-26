#!/usr/bin/env python3

import os, subprocess, sys


def get_usused_branch(branches):
    def get():
        subout = subprocess.check_output(('git', 'branch'))
        for line in subout.splitlines():
            line = line.decode()
            while line.startswith('*'):
                line = line[1:]
            yield line.strip()

    existing = set(get())
    unused = sorted(existing - set(branches))
    return unused and unused[0]


if __name__ == '__main__':
    branch = get_usused_branch(sys.argv[1:])
    if not branch:
        print('ERROR: Can\'t delete all branches', file=sys.stderr)
        sys.exit(-1)

    print(branch)
