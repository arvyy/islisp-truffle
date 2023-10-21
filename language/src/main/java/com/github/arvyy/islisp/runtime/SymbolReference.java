package com.github.arvyy.islisp.runtime;

/**
 * Class for reference identifaction of symbols (since symbols can be unnamed, it's not enough to use string)
 * custom class instead of plain int for static typing reasons.
 */
public class SymbolReference {

    private static int last = 0;

    private final int id;

    /**
     * Create symbol reference with new unique id.
     */
    public SymbolReference() {
        id = last++;
    }

    /**
     *
     * @return reference's id.
     */
    public int getId() {
        return id;
    }
}
