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

./genzip.sh ${dir}

zip_src=${dir}/../icfpc2018-sampou-submit-full.zip

for i in $(seq 1 15) ; do
    zip_dst=/home/icfpc-export/icfpc2018-sampou-full-$(date +"%d-%H%M%S").zip
    if [ ! -r ${zip_dst} ] ; then
        cp -a ${zip_src} ${zip_dst}
        copied=1
        break
    fi
    sleep 1
done

if [ x"$copied" != x1 ]; then
    cat <<EOF
Collision detected.
EOF
    exit 1
fi

ls -l ${zip_dst}

prv=$(cat data/private-id.txt)
url="http://reasoning.tokyo/icfpc/$(basename ${zip_dst})"
sha256=$(shasum -a 256 ${zip_dst} | cut -d ' ' -f 1)

set -x

curl -L \
  --data-urlencode action=submit \
  --data-urlencode privateID="$prv" \
  --data-urlencode submissionURL="$url" \
  --data-urlencode submissionSHA="$sha256" \
  https://script.google.com/macros/s/AKfycbzQ7Etsj7NXCN5thGthCvApancl5vni5SFsb1UoKgZQwTzXlrH7/exec
