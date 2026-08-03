from bisect import bisect
from pathlib import Path

FILE = Path(__file__).parent / 'git-commit-ids.txt'
DAY = 60 * 60 * 24


def _print_times(time_ids):
    times, ids = zip(*time_ids)
    days = (int(times[-1]) - int(times[0])) / DAY
    count = len(set(ids))
    print(
        count,
        'commits /',
        round(days, 2),
        'days =',
        round((count / days), 2),
        'a day',
    )


def _time_ids():
    for i, line in enumerate(FILE.open()):
        try:
            id, time = line.split()
            yield time, id
        except ValueError:
            print('Bad line %d: "%s"' % (i, line[:-1]))


def _print_all():
    time_ids = list(_time_ids())
    _print_times(time_ids)
    last_time = int(time_ids[-1][0])

    for days in 365, 120, 30, 7, 1:
        i = bisect(time_ids, (str(last_time - DAY * days), ''))
        _print_times(time_ids[i:])


if __name__ == '__main__':
    _print_all()
