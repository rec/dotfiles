from __future__ import annotations
from pathlib import Path
import re
import itertools
import json
import functools
import typing as t


print('...loaded PYTHONSTARTUP at', __file__)
print('Symbols:', *sorted(i for i in globals() if not i.startswith('_')))
