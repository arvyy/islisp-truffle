#!/bin/bash
# helper script to run islisp through compiled java modules instead of building native image
DIR=$(dirname "$0")
java -p "$DIR/target/deps:$DIR/target/launcher-0.0.1-local.jar" -m com.github.arvyy.islisp.launcher/com.github.arvyy.islisp.launcher.Main "$@"
