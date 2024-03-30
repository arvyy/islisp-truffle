#!/bin/bash

if [ $# -lt 1 ]
then
  echo 1>&2 "$0: First argument must provide islisp invocation command, using FILE as a placeholder for the test file path."
  exit 2
fi

commandTemplate=$1
testingRequireReplacement=$(<portable/testtemplate.lisp.txt)
exitCode=0

for f in portable/*.lisp
do
  name=${f::-5}
  name=${name:9}
  testContent=$(<"$f" )
  echo ${testContent//(requires \"testing.lisp\")/$testingRequireReplacement} > tmp.lisp
  command=${commandTemplate/'FILE'/'tmp.lisp'}
  eval "$command" > tmp.txt
  if ! cmp --silent "tmp.txt" "portable/$name.expect.txt"
  then
    echo "Failed test: $name"
    echo "Expected:"
    echo $(<"portable/$name.expect.txt")
    echo "Was:"
    echo $(<tmp.txt )
    echo -e "\n\n"
    exitCode=1
  fi
done

exit $exitCode