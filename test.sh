#!/bin/sh

set -eu

expected=`cat lorem_ipsum.txt | wc`
echo "$expected"
actual=`cat lorem_ipsum.txt | stack exec wc-exe 2>/dev/null`
echo "$actual"

expected=`wc lorem_ipsum.txt`
echo "$expected"
actual=`stack exec wc-exe lorem_ipsum.txt 2>/dev/null`
echo "$actual"
