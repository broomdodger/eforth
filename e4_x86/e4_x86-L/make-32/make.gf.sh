#!/bin/sh

# cd to the directory this .sh file is in
# the quotes allow spaces in the path name
# the /* means back up to the last /
# it removes the filename from the path

cd "${0%/*}";pwd

gforth -e ": MacOSX ;" ../../load.gf

