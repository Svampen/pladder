#!/bin/ksh

# Script to generate poe exp per level information as definitions in a .hrl file

file=$1

echo "-define(Exp(Level),"
echo "    case Level of"
cat $file | while read l1 e1 g1 l2 e2 g2; do
  echo $e1 | sed 's/,//g' | read exp1
  echo $e2 | sed 's/,//g' | read exp2
  echo "        $l1 -> $exp1;"
  echo "        $l2 -> $exp2;"
done
echo "        _ -> 0"
echo "    end)."
echo "-define(MaxExpLength(Level), length(integer_to_list(?Exp(Level))))."