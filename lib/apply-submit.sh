#!/bin/sh

set -e

usage() {
    cat <<EOF
Usage: $0 ZIP_FILENAME
EOF
}

zip_fn="$1"

if [ x"$zip_fn" = x ]; then
    echo "ZIP_FILENAME is required."
    usage
    exit 1
fi

zip_path=/home/icfpc-export/${zip_fn}
if [ ! -r "${zip_path}" ]; then
    echo "file not found: ${zip_path}"
    usage
    exit 1
fi

ls -l ${zip_path}

prv=$(cat data/private-id.txt)
url="http://reasoning.tokyo/icfpc/$(basename ${zip_path})"
sha256=$(shasum -a 256 ${zip_path} | cut -d ' ' -f 1)

set -x

curl -L \
  --data-urlencode action=submit \
  --data-urlencode privateID="$prv" \
  --data-urlencode submissionURL="$url" \
  --data-urlencode submissionSHA="$sha256" \
  https://script.google.com/macros/s/AKfycbzQ7Etsj7NXCN5thGthCvApancl5vni5SFsb1UoKgZQwTzXlrH7/exec
