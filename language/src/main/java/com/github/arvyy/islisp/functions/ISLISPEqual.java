package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * Implements `equal` function.
 */
public abstract class ISLISPEqual extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    @Child
    DirectCallNode eqCallNode;


    ISLISPEqual(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        var nil = ISLISPContext.get(this).getNil();
        if (frame.getArguments().length != 3) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length - 1, 2, 2);
        }
        var isEq = getEqCallNode().call(null, frame.getArguments()[1], frame.getArguments()[2]);
        if (isEq instanceof Symbol s && s.identityReference() == nil.identityReference()) {
            return executeGeneric(frame.getArguments()[1], frame.getArguments()[2]);
        } else {
            return isEq;
        }
    }

    abstract Object executeGeneric(Object o1, Object o2);

    @Specialization
    Object doStrings(String s1, String s2) {
        var ctx = ISLISPContext.get(this);
        if (s1.equals(s2)) {
            return ctx.getT();
        } else {
            return ctx.getNil();
        }
    }

    @Fallback
    Object doFallback(Object o1, Object o2) {
        var ctx = ISLISPContext.get(this);
        return o1 == o2 ? ctx.getT() : ctx.getNil();
    }


    @CompilerDirectives.TruffleBoundary
    DirectCallNode getEqCallNode() {
        if (eqCallNode == null) {
            var ctx = ISLISPContext.get(this);
            var callNode = DirectCallNode.create(
                ctx.lookupFunction(ctx.namedSymbol("eq").identityReference())
                    .callTarget());
            eqCallNode = insert(callNode);
        }
        return eqCallNode;
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPEqualNodeGen.create(lang).getCallTarget());
    }
}
