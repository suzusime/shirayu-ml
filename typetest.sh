#!/bin/zsh
echo "***** 必修課題 *****"
cat testcase.txt | while read line
do
   echo "$line"
   echo "$line" | ./miniml
done
echo "\n"
echo "***** 任意課題 Ex 4.3.6 *****"
cat testcase_rec.txt | while read line
do
   echo "$line"
   echo "$line" | ./miniml
done
echo "\n"
echo "\n***** 以下、おかしいもの *****\n"
cat ng_testcase.txt | while read line
do
   echo "$line"
   echo "$line" | ./miniml
done
