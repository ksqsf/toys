#!/bin/bash

cat $1 $2 > merged
mv merged $2
rm $1
