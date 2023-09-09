package com.github.arvyy.islisp.parser;

class IdGen {
    private int curr = 0;

    int next() {
        return curr++;
    }
}
