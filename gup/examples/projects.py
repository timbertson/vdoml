from __future__ import print_function
import os, sys, subprocess

directories = os.listdir('.')
directories = filter(os.path.isdir, directories)
directories = filter(lambda p: p[0] not in ['.','_'], directories)

def build(paths):
	cmd = ['dune', 'build'] + list(paths)
	if os.environ.get('GUP_XTRACE', '0') == '1':
		print(' + ' + ' '.join(cmd))
	proc = subprocess.Popen(cmd)
	if proc.wait() != 0:
		sys.exit(1)

def build_all(suffix):
	targets = map(lambda base: os.path.join(base, suffix), directories)
	return build(targets)

