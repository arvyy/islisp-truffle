package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.interop.TruffleObject;

/**
 * Represents LISP symbol.
 *
 * @param name textual name, if this is named symbol (as opposed to gensym'ed).
 * @param identityReference unique id for a symbol
 */
public record Symbol(
        String name,
        SymbolReference identityReference
) implements TruffleObject {
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Symbol other) {
            return other.identityReference.equals(identityReference);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return identityReference.hashCode();
    }
}
