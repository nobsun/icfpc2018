#!/bin/sh

set -e

usage() {
    cat <<EOF
Usage: $0 DIRECTORY_TO_SUBMIT GOOGLE_DRIVE_FILE_ID
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

file_id="$2"

if [ x"$file_id" = x ]; then
    echo "GOOGLE_DRIVE_FILE_ID is required."
    usage
    exit 1
fi

prv=$(cat data/private-id.txt)

sha256=$(cat ${dir}/../icfpc2018-sampou-submit-full.sha256)
url="https://drive.google.com/uc?id=${file_id}"

set -x

curl -L \
  --data-urlencode action=submit \
  --data-urlencode privateID="$prv" \
  --data-urlencode submissionURL="$url" \
  --data-urlencode submissionSHA="$sha256" \
  https://script.google.com/macros/s/AKfycbzQ7Etsj7NXCN5thGthCvApancl5vni5SFsb1UoKgZQwTzXlrH7/exec
