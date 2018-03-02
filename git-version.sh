#!/bin/bash

vcount=$(git rev-list --count --first-parent HEAD)
vnum=$(git describe --always HEAD)

echo "// git revision" > revision.inc
echo "const RevisionStr = '$vcount-$vnum';" >> revision.inc

