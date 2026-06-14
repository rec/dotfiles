import fileinput
import re

MESSAGE_RE = re.compile(r'( (?:\w+/)+ \w+ \.py :\d+)(?::\d+)?(.*)', re.VERBOSE)


def pr(*a):
    if False:
        print(*a)


def fix_error_messages():
    for line in fileinput.input():
        if m := MESSAGE_RE.search(line):
            file, message = m.groups()
            print(f"{file}: {message.strip()}")
        elif not line:
            break
        else:
            print(line.rstrip())


if __name__ == '__main__':
    fix_error_messages()
