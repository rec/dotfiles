#!/usr/bin/env python3

import os, sys
from os.path import exists

INIT_FILE = '__init__.py'
PYTHON_TEST_SUFFIX = '_test.py'
PYTHON_SUFFIX = '.py'


def rotate(filename):
    def rotate_py():
        dirname, basename = os.path.split(filename)

        # Move up directories until you get to one without an init file.
        path = []
        while exists(os.path.join(dirname, INIT_FILE)):
            dirname, child = os.path.split(dirname)
            path.insert(0, child)

        rotated_base = basename.replace(PYTHON_TEST_SUFFIX, PYTHON_SUFFIX)
        if rotated_base == basename:
            # Not a test
            path.append(basename.replace(PYTHON_SUFFIX, PYTHON_TEST_SUFFIX))
            rotated = os.path.join(dirname, *path)
            if exists(rotated):
                return rotated

            rotated = os.path.join(dirname, 'test', *path)
            if exists(rotated):
                return rotated

        else:
            path.append(rotated_base)
            rotated = os.path.join(dirname, *path)
            if exists(rotated):
                return rotated

            if path.pop(0) == 'test':
                rotated = os.path.join(dirname, *path)
                if exists(rotated):
                    return rotated

        return filename


    def existing(*names):
        return next((n for n in names if exists(n)), filename)

    def rotate_h():
        return existing(base + '.cc', base + '_unittest.cc')

    def rotate_cc():
        if base.endswith('_unittest'):
            b = base[:-len('_unittest')]
            return existing(b + '.h', b + '.cc')
        return existing(base + '_unittest.cc', base + '.h')

    base, suffix = os.path.splitext(filename)
    if suffix == '.py':
        return rotate_py()

    if suffix == '.cc':
        return rotate_cc()

    if suffix == '.h':
        return rotate_h()

    return filename


if __name__ == '__main__':
    print(rotate(*sys.argv[1:]), end='')
