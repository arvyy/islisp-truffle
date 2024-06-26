package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.Utils;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `set-elt` function, that sets an element in sequence for a given index.
 */
public abstract class ISLISPSetElt extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPSetElt(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public String getName() {
        return "set-elt";
    }

    @Override
    @CompilerDirectives.TruffleBoundary
    public SourceSection getSourceSection() {
        return Source.newBuilder("islisp", "", ISLISPSetElt.class.getSimpleName())
            .internal(true)
            .build()
            .createSection(1);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 4) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length - 1, 3, 3);
        }
        return executeGeneric(frame.getArguments()[1], frame.getArguments()[2], frame.getArguments()[3]);
    }

    abstract Object executeGeneric(Object obj, Object seq, Object index);

    @Specialization
    Object doList(Object value, Pair p, int index) {
        var cell = p;
        if (index < 0) {
            try {
                return errorSignalerNode.signalIndexOutOfRange(index, getListSize(p));
            } catch (Utils.NotAList e) {
                return errorSignalerNode.signalWrongType(
                    p,
                    ISLISPContext.get(this).lookupClass("<list>"));
            }
        }
        for (int i = 0; i < index; i++) {
            if (cell.cdr() instanceof Pair pair) {
                cell = pair;
                continue;
            }
            if (Utils.isNil(cell.cdr())) {
                try {
                    return errorSignalerNode.signalIndexOutOfRange(index,  getListSize(p));
                } catch (Utils.NotAList e) {
                    return errorSignalerNode.signalWrongType(
                        p,
                        ISLISPContext.get(this).lookupClass("<list>"));
                }
            } else {
                var ctx = ISLISPContext.get(this);
                return errorSignalerNode.signalDomainError(
                    "Not a proper list",
                    cell.cdr(),
                    ctx.lookupClass("<list>"));
            }
        }
        cell.setCar(value);
        return value;
    }

    @CompilerDirectives.TruffleBoundary
    int getListSize(Pair p) throws Utils.NotAList {
        return Utils.readList(p).size();
    }

    @Specialization
    Object doVector(Object value, LispVector vec, int index) {
        if (index < 0 || index >= vec.values().length) {
            return errorSignalerNode.signalIndexOutOfRange(index, vec.values().length);
        }
        vec.values()[index] = value;
        return value;
    }

    @Specialization
    Object doString(Object obj, String str, int index) {
        return errorSignalerNode.signalDomainError(
            "Cannot change immutable value",
            str,
            ISLISPContext.get(this).lookupClass("<string>"));
    }

    @Specialization
    Object doMutableString(LispChar c, LispMutableString str, int index) {
        if (index < 0 || index >= str.chars().length) {
            return errorSignalerNode.signalIndexOutOfRange(index, str.chars().length);
        }
        str.chars()[index] = c;
        return c;
    }

    @Specialization(guards = {
        "interop.hasArrayElements(o)"
    }, limit = "3")
    Object doTruffleVector(
        Object value,
        Object o,
        int index,
        @CachedLibrary("o") InteropLibrary interop
    ) {
        try {
            interop.writeArrayElement(o, index, value);
            return value;
        } catch (UnsupportedMessageException | InvalidArrayIndexException | UnsupportedTypeException e) {
            return errorSignalerNode.signalTruffleInteropError(e);
        }
    }

    @Fallback
    Object fallback(Object value, Object seq, Object index) {
        return errorSignalerNode.signalWrongType(seq, ISLISPContext.get(this).lookupClass("<list>"));
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPSetEltNodeGen.create(lang).getCallTarget());
    }

}
