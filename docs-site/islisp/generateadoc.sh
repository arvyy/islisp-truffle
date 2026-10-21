#!/bin/bash
ISLISP_EXEC="${ISLISP:-bash ../../launcher/islisp.sh}"

# first arg adoc preamble name
# second arg islisp module name
process() {
    echo '// generated content, do not edit directly. Created with docs-site/apireference/generateadoc.sh \n\n' > ./modules/apireference/pages/$1.adoc
    cat ./preamble/$1.adoc >> ./modules/apireference/pages/$1.adoc
    echo -e '\n\n' >> ./modules/apireference/pages/$1.adoc
    $ISLISP_EXEC ../../tools/documentationexporter.lisp $2 >> ./modules/apireference/pages/$1.adoc
}

process "prelude" "ROOT"
process "introspection" "builtin/introspection.lisp"
process "truffle" "builtin/truffle.lisp"
