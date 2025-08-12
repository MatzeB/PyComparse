import this
import sys as _sys
import os.path as osp, os as _os
import argparse, functools

print(_sys.implementation.name)
print(osp.dirname("/foo/bar"))
_os.chdir("/")

from os.path import (
basename,
    dirname as dn,
)
from builtins import len as l
from shlex import quote, join as j

print(basename("/foo/bar") + dn("/baz/bam"))
print(l("jojo"))
print(quote("a bc"))
print(j(("a", "b c")))
