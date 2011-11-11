#!/bin/sh

set -e 
set -x 

. ./setup.data

echo test foo
test "$foo" = "true"
echo test bar
test "$bar" = "true"
echo test over
test "$over" = "false"
