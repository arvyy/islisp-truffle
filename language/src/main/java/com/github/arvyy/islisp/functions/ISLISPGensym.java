package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.SymbolReference;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * Implements a `gensym` function, that returns a new unnamed symbol distinct from any other symbol.
 */
public class ISLISPGensym extends RootNode {

    ISLISPGensym(TruffleLanguage<?> language) {
        super(language);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        return new Symbol(makeGensymName(), new SymbolReference());
    }

    @CompilerDirectives.TruffleBoundary
    String makeGensymName() {
        return "gensym-" + ISLISPContext.get(this).gensymIndex();
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPGensym(lang).getCallTarget());
    }

}
