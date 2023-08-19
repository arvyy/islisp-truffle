package com.github.arvyy.islisp.runtime;

// class for reference identifaction of symbols (since symbols can be unnamed, it's not enough to use string)
// custom class instead of plain int for static typing reasons
public class SymbolReference {

    private static int last = 0;

    public final int id;

    public SymbolReference() {
        id = last++;
    }

}
