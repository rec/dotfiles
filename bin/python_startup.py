from __future__ import annotations
try:
    import numpy as np
except Exception:
    pass
from pathlib import Path
import dataclasses as dc
import functools
import itertools
import json
import re
import typing as t


print('...loaded PYTHONSTARTUP at', __file__)
print('Symbols:', *sorted(i for i in globals() if not i.startswith('_')))
