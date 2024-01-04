from __future__ import annotations
from pathlib import Path
import re
import itertools
import functools
import typing as t
try:
    # import superduperdb as s
    pass
except ImportError:
    pass


print('...loaded PYTHONSTARTUP at', __file__)
print('Symbols:', *sorted(i for i in globals() if not i.startswith('_')))
