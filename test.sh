#!/bin/sh

set -eu

expected=`cat lorem_ipsum.txt | wc`
echo "$expected"
actual=`cat lorem_ipsum.txt | stack exec wc-exe 2>/dev/null`
echo "$actual"
if [ "$expected" = "$actual" ]; then
    echo 'ok'
else
    echo 'ng'
fi
