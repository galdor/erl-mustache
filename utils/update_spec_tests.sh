#!/bin/sh

set -eu
set -o pipefail

root=$(realpath $(dirname $0)/..)

repository="https://github.com/mustache/spec.git"

process_file() {
    local file base_name mod_name output

    file=$1

    base_name=$(basename $file)
    mod_name=${base_name%.json}
    output=$root/test/mustache_spec_${mod_name}_test.erl

    echo "generating $output"
    $root/utils/generate_spec_test.rb $file >$output
}

rm -rf $root/tmp
mkdir $root/tmp

rm -f $root/test/mustache_spec_*_test.erl

echo "cloning $repository"
git clone -q $repository $root/tmp/spec

find $root/tmp/spec -name '*.json' | grep -v "lambdas" |
    while read file; do
        process_file $file
    done
