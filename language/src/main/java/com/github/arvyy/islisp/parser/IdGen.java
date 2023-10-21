package com.github.arvyy.islisp.parser;

/**
 * Helper class for giving incremental sequence id's.
 */
class IdGen {
    private int curr = 0;

    int next() {
        return curr++;
    }
}
