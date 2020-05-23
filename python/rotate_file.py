#!/usr/bin/env python3

import os, sys

INIT_FILE = '__init__.py'
PYTHON_TEST_SUFFIX = '_test.py'
PYTHON_SUFFIX = '.py'


def rotate_py(filename):
    dirname, basename = os.path.split(filename)

    # Move up directories until you get to one without an init file.
    path = []
    while os.path.exists(os.path.join(dirname, INIT_FILE)):
        dirname, child = os.path.split(dirname)
        path.insert(0, child)

    rotated_base = basename.replace(PYTHON_TEST_SUFFIX, PYTHON_SUFFIX)
    if rotated_base == basename:
        # Not a test
        path.append(basename.replace(PYTHON_SUFFIX, PYTHON_TEST_SUFFIX))
        rotated = os.path.join(dirname, *path)
        if os.path.exists(rotated):
            return rotated

        rotated = os.path.join(dirname, 'test', *path)
        if os.path.exists(rotated):
            return rotated

    else:
        path.append(rotated_base)
        rotated = os.path.join(dirname, *path)
        if os.path.exists(rotated):
            return rotated

        if path.pop(0) == 'test':
            rotated = os.path.join(dirname, *path)
            if os.path.exists(rotated):
                return rotated

    return filename


def rotate(filename):
    base, suffix = os.path.splitext(filename)
    if suffix == '.py':
        return rotate_py(filename)
    return filename


if __name__ == '__main__':
    print(rotate(*sys.argv[1:]), end='')
