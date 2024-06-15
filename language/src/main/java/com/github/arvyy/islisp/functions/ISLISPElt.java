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
import com.oracle.truffle.api.interop.InteropException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `elt` function, that returns an element in sequence for a given index.
 */
@TypeSystemReference(ISLISPTypes.class)
public abstract class ISLISPElt extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    @CompilerDirectives.CompilationFinal
    LispClass cList, cInteger;

    ISLISPElt(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public String getName() {
        return "elt";
    }

    @Override
    @CompilerDirectives.TruffleBoundary
    public SourceSection getSourceSection() {
        return Source.newBuilder("islisp", "", ISLISPElt.class.getSimpleName())
            .internal(true)
            .build()
            .createSection(1);
    }

    LispClass cList() {
        if (cList == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            var ctx = ISLISPContext.get(this);
            cList = ctx.lookupClass("ROOT", ctx.namedSymbol("<list>").identityReference());
        }
        return cList;
    }

    LispClass cInteger() {
        if (cInteger == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            var ctx = ISLISPContext.get(this);
            cInteger = ctx.lookupClass("ROOT", ctx.namedSymbol("<integer>").identityReference());
        }
        return cInteger;
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 3) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length - 1, 2, 2);
        }
        return executeGeneric(frame.getArguments()[1], frame.getArguments()[2]);
    }

    abstract Object executeGeneric(Object seq, Object index);

    @Specialization
    Object doList(Pair p, int index) {
        var value = p;
        if (index < 0) {
            try {
                return errorSignalerNode.signalIndexOutOfRange(index, getListSize(p));
            } catch (Utils.NotAList e) {
                return errorSignalerNode.signalWrongType(
                    p,
                    cList());
            }
        }
        for (int i = 0; i < index; i++) {
            if (value.cdr() instanceof Pair pair) {
                value = pair;
                continue;
            }
            if (Utils.isNil(value.cdr())) {
                try {
                    return errorSignalerNode.signalIndexOutOfRange(index,  getListSize(p));
                } catch (Utils.NotAList e) {
                    return errorSignalerNode.signalWrongType(
                        p,
                        cList());
                }
            } else {
                return errorSignalerNode.signalDomainError(
                    "Not a proper list",
                    value.cdr(),
                    cList());
            }
        }
        return value.car();
    }

    @CompilerDirectives.TruffleBoundary
    int getListSize(Pair p) throws Utils.NotAList {
        return Utils.readList(p).size();
    }

    @Specialization
    Object doVector(LispVector vec, int index) {
        if (index < 0 || index >= vec.values().length) {
            return errorSignalerNode.signalIndexOutOfRange(index, vec.values().length);
        }
        return vec.values()[index];
    }

    @Specialization
    Object doString(String str, int index) {
        if (index < 0 || index >= str.length()) {
            return errorSignalerNode.signalIndexOutOfRange(index, str.length());
        }
        return new LispChar(str.codePointAt(index));
    }

    @Specialization
    Object doMutableString(LispMutableString str, int index) {
        if (index < 0 || index >= str.chars().length) {
            return errorSignalerNode.signalIndexOutOfRange(index, str.chars().length);
        }
        return str.chars()[index];
    }

    @Specialization(guards = {
        "interop.hasArrayElements(o)"
    }, limit = "3")
    Object doTruffleVector(
        Object o,
        int index,
        @CachedLibrary("o") InteropLibrary interop
    ) {
        try {
            return interop.readArrayElement(o, index);
        } catch (InteropException e) {
            return errorSignalerNode.signalTruffleInteropError(e);
        }
    }

    @Specialization
    Object fallback(Object seq, LispBigInteger index) {
        return errorSignalerNode.signalWrongType(
            seq,
            cList());
    }

    @Fallback
    Object fallback(Object seq, Object index) {
        return errorSignalerNode.signalWrongType(
            index,
            cInteger());
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        var callTarget = ISLISPEltNodeGen.create(lang).getCallTarget();
        return new LispFunction(
            new Closure(null, null, null),
            callTarget,
            false,
            true);
    }

}
