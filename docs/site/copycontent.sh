#!/bin/bash

cd ..
for doc in *.adoc ; do
  if [ -f site/content/_frontmatter/$doc ]; then
    cat site/content/_frontmatter/$doc > site/content/$doc
    cat $doc >> site/content/$doc
  fi
done