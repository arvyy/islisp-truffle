#!/bin/bash
# helper script to run islisp-lsp through compiled java modules instead of building native image
DIR=$(dirname "$0")
if [[ "$DEBUG" == "true" ]]; then
    DEBUG_PARAM="-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=1044"
else
    DEBUG_PARAM=""
fi
java ${DEBUG_PARAM} -p "$DIR/target/deps:$DIR/target/lsp-0.0.1-local.jar" -m com.github.arvyy.islisp.lsp/com.github.arvyy.islisp.lsp.Main "$@"
