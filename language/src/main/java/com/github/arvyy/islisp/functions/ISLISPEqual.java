package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.Utils;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.nodes.ISLISPTypes;
import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.dsl.TypeSystemReference;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.RootNode;

import static com.github.arvyy.islisp.Utils.isNil;

/**
 * Implements `equal` function.
 */
@TypeSystemReference(ISLISPTypes.class)
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
        if (frame.getArguments().length != 3) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length - 1, 2, 2);
        }
        return isEqual(frame.getArguments()[1], frame.getArguments()[2]);
    }

    Object isEqual(Object o1, Object o2) {
        var nil = ISLISPContext.get(this).getNil();
        var isEq = getEqCallNode().call(null, o1, o2);
        if (isEq instanceof Symbol s && s.identityReference() == nil.identityReference()) {
            return executeGeneric(o1, o2);
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

    @Specialization
    Object doPairs(Pair p1, Pair p2) {
        var ctx = ISLISPContext.get(this);
        var nil = ctx.getNil();
        if (isNil(isEqual(p1.car(), p2.car()))) {
            return nil;
        }
        return isEqual(p1.cdr(), p2.cdr());
    }

    @Specialization
    Object doVectors(LispVector v1, LispVector v2) {
        var ctx = ISLISPContext.get(this);
        var nil = ctx.getNil();
        if (v1.values().length != v2.values().length) {
            return nil;
        }
        for (int i = 0; i < v1.values().length; i++) {
            if (isNil(isEqual(v1.values()[i], v2.values()[i]))) {
                return nil;
            }
        }
        return ctx.getT();
    }

    @Specialization
    Object doDoubles(double d1, double d2) {
        return Utils.booleanToSymbol(d1 == d2);
    }

    @Specialization
    Object doBigInts(LispBigInteger i1, LispBigInteger i2) {
        return Utils.booleanToSymbol(i1.data().equals(i2.data()));
    }

    @Fallback
    Object doFallback(Object o1, Object o2) {
        var ctx = ISLISPContext.get(this);
        return o1 == o2 ? ctx.getT() : ctx.getNil();
    }


    @CompilerDirectives.TruffleBoundary
    DirectCallNode getEqCallNode() {
        if (eqCallNode == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
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
