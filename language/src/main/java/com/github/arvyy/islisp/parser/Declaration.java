package com.github.arvyy.islisp.parser;

import com.github.arvyy.islisp.runtime.SymbolReference;

/**
 * Misc declarations that can be done in source code * to affect behavior of the interpreter / compiler.
 */
public sealed interface Declaration {

    /**
     * Mark given function for inlining.
     *
     * @param name of the function
     */
    record Inline(SymbolReference name) implements Declaration { }

}
