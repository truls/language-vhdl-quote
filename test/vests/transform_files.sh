#!/bin/sh

set -eu

test_dirs="vhdl-93/ashenden/compliant vhdl-93/billowitch/compliant vhdl-93/clifton-labs/compliant"

replace_dir() {
    sed 's/vhdl-93/vhdl-93-transformed/g'
}

# Create output directories
find ${test_dirs} -type d | replace_dir | xargs mkdir -p

# Copy exp fiels
for f in `find ${test_dirs} -name \*.exp`; do
    cp $f $(echo $f | replace_dir)
done

# Rebuild
stack build

# Transform vhdl files using our parser
if [ $# -gt 0 ]; then
    for f in `find ${test_dirs} -type f | grep \.vhd | grep $@`; do
        echo "Transforming file ${f}"
        stack exec dumpast -- -p $f > $(echo $f | replace_dir)
    done
else
    for f in `find ${test_dirs} -type f | grep \.vhd`; do
        echo "Transforming file ${f}"
        stack exec dumpast -- -p $f > $(echo $f | replace_dir)
    done
fi
