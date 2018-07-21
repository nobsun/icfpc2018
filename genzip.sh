#!/bin/sh

set -e

usage() {
    cat <<EOF
Usage: $0 DIRECTORY_TO_SUBMIT
EOF
}

dir="$1"

if [ x"$dir" = x ]; then
    echo "DIRECTORY_TO_SUBMIT is required."
    usage
    exit 1
fi

if [ ! -d "$dir" ]; then
    echo "DIRECTORY_TO_SUBMIT is required."
    usage
    exit 1
fi

prv=$(cat data/private-id.txt)

zip_path=${dir}/../icfpc2018-sampou-submit.zip
sha_path=${dir}/../icfpc2018-sampou-submit.sha256

( cd $dir && zip -P $prv ../icfpc2018-sampou-submit.zip *.nbt > ../icfpc2018-sampou-submit.ziplog )
shasum -a 256 ${zip_path} | cut -d ' ' -f 1 > ${sha_path}

ls -l ${zip_path}
cat ${sha_path}
