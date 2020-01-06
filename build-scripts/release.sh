#!/bin/bash

help() {
    cat <<EOF
This script writes the given version number where it is needed:
- next.asd
- debian/control
- debian/changelog

It adds a release line to debian/changelog.

It could be more:
- push a Git tag
- build and rsync the Debian package.
EOF
}

if [[ $# -ne 1 ]]; then
    echo "Usage: release <version number>"
    exit 1
fi

if [[ $1 = "-h" ]]; then
    help
    exit 0
fi

echo "next.asd"
sed -r --in-place 's/version "[0-9\.?]+"/version "'"$1"'"/' next.asd

echo "debian/"
sed -r -i 's/next \([0-9\.?]+\)/next ('"$1"')/' debian/changelog
sed -r -i 's/Standards-Version: [0-9\.?]+/Standards-Version: '"$1"'/' debian/control

output="$(awk 'NR==3{print "  * release '$1'"}1' debian/changelog)"
echo "$output" > debian/changelog

echo "done."
