from __future__ import print_function
import os, sys, subprocess

directories = os.listdir('.')
directories = filter(os.path.isdir, directories)
directories = filter(lambda p: p[0] not in ['.','_'], directories)

def gup(paths):
	proc = subprocess.Popen(['gup'] + list(paths))
	if proc.wait() != 0:
		sys.exit(1)

def gup_all(suffix):
	targets = map(lambda base: os.path.join(base, suffix), directories)
	return gup(targets)
