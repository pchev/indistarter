#!/bin/bash

vcount=$(git rev-list --count --first-parent HEAD)
vnum=$(git log -n 1 --pretty=format:"%h")

echo "// git revision" > revision.inc
echo "const RevisionStr = '$vcount-$vnum';" >> revision.inc

