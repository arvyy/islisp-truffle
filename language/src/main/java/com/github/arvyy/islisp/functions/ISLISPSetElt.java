package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.*;
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

/**
 * Implements `set-elt` function, that sets an element in sequence for a given index.
 */
//TODO array index out of bounds handling
public abstract class ISLISPSetElt extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPSetElt(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
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
        for (int i = 0; i < index; i++) {
            cell = (Pair) cell.cdr();
        }
        cell.setCar(value);
        return value;
    }

    @Specialization
    Object doVector(Object value, LispVector vec, int index) {
        vec.values()[index] = value;
        return value;
    }

    @Specialization
    Object doString(Object obj, String str, int index) {
        throw new ISLISPError("Can't mutate string", this);
    }

    @Specialization
    Object doMutableString(LispChar c, LispMutableString str, int index) {
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
            throw new ISLISPError("Interop error", this);
        }
    }

    @Fallback
    Object fallback(Object value, Object seq, Object index) {
        throw new ISLISPError("Bad sequence or index", this);
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
