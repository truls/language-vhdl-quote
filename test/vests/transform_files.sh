#!/bin/bash

set -eu

test_dirs="vhdl-93/ashenden/compliant vhdl-93/billowitch/compliant vhdl-93/clifton-labs/compliant"

replace_dir() {
    sed 's/vhdl-93/vhdl-93-transformed/g'
}
export -f replace_dir

# Create output directories
find ${test_dirs} -type d | replace_dir | xargs mkdir -p

# Copy exp files
for f in `find ${test_dirs} -name \*.exp`; do
    cp $f $(echo $f | replace_dir)
done

stack_local_root=$(stack path | grep local-install-root |
                       awk -F: '{ gsub(/^[ \t]+/, "", $2); print $2 }')
dumpast=${stack_local_root}/bin/dumpast
export dumpast
run_test() {
    echo "Transforming file ${1}"
    $dumpast -p $1 > $(echo $1 | replace_dir)
}
export -f run_test

# Transform vhdl files using our parser
if [ $# -gt 0 ]; then
    #find ${test_dirs} -type f | grep \.vhd | grep $@ | parallel run_test
    find ${test_dirs} -type f | grep \.vhd | grep $@ | run_test

else
    #find ${test_dirs} -type f | grep \.vhd | parallel run_test
    find ${test_dirs} -type f | grep \.vhd | run_test

fi
