#!/bin/sh
cat testcase.txt | while read line
do
   echo $line
   echo $line | ./miniml
done
