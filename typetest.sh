#!/bin/zsh
cat testcase.txt | while read line
do
   echo "$line"
   echo "$line" | ./miniml
done
echo "\n***** 以下、おかしいもの *****\n"
cat ng_testcase.txt | while read line
do
   echo "$line"
   echo "$line" | ./miniml
done
