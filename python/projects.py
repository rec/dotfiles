#!/usr/bin/env python3

"""
Keep a stack of current projects - directories, essentially.

* go to
  * current project
  * project n

* rotate [default +1]
  * rotating by 1 means putting the current project at the back

* push a new project
  * at the top
  * at position n

* pop
  * the current project
  * project n

* undo the last operation

* list projects

"""

import os
import json
import sys

CONFIG_FILE = os.path.expanduser('~/.projects.yml')
COMMANDS = 'clear', 'goto', 'rotate', 'push', 'pop', 'undo', 'list'


class Projects:
    def __init__(self):
        # self.projects is a stack, with the top element at 0.
        try:
            with open(CONFIG_FILE) as fp:
                self.projects, self.undo = json.load(fp)
        except FileNotFoundError:
            self.projects, self.undo = [], []
        self.original_projects = self.projects[:]

    @property
    def project(self):
        return self.projects and self.projects[0] or os.getcwd()

    def goto(self, project=0):
        return self.projects[int(project)]

    def rotate(self, steps=1):
        steps = int(steps)
        self.projects = self.projects[steps:] + self.projects[:steps]
        self._write()

    def push(self, project=None, step=0):
        project = os.path.abspath(
            os.path.expanduser(os.path.expandvars(project or os.getcwd()))
        )
        if project in self.projects:
            raise ValueError('Cannot insert the same project twice')

        step = int(step)
        self.projects.insert(int(step), project)
        self._write()

    def pop(self, step=0):
        print('Popped', self.projects.pop(int(step)), file=sys.stderr)
        self._write()

    def undo(self):
        self.projects = self.undo
        self._write()

    def clear(self):
        self.projects.clear()
        self._write()

    def list(self):
        for i, p in enumerate(self.projects):
            print('%d: %s' % (i, p), file=sys.stderr)
        return os.getcwd()

    def _write(self):
        with open(CONFIG_FILE, 'w') as fp:
            json.dump([self.projects, self.undo], fp)


def main(command='go', *args):
    try:
        command = next(c for c in COMMANDS if c.startswith(command))
    except StopIteration:
        raise ValueError('Do not understand command %s' % command)

    projects = Projects()
    try:
        print(getattr(projects, command)(*args) or projects.project)
    except:
        print(os.getcwd())


if __name__ == '__main__':
    main(*sys.argv[1:])
