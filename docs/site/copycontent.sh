#!/bin/bash

cd ..
for doc in *.adoc ; do
  if [ -f docs/content/_frontmatter/$doc ]; then
    cat docs/content/_frontmatter/$doc > docs/content/$doc
    cat $doc >> docs/content/$doc
  fi
done