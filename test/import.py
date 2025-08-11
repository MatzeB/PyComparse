import this
import sys as _sys
import os.path as osp, os as _os
import argparse, functools

print(_sys.implementation.name)
print(osp.dirname("/foo/bar"))
_os.chdir("/")
