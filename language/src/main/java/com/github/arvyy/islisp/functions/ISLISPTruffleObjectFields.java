package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispVector;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * Implements `truffle-object-fields` function, which returns fields as a list of strings
 * in a given object.
 */
public abstract class ISLISPTruffleObjectFields extends RootNode {

    @Child
    private ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPTruffleObjectFields(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 2) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length - 1, 1, 1);
        }
        try {
            return executeGeneric(frame.getArguments()[1]);
        } catch (Exception e) {
            throw new ISLISPError(e.getMessage(), this);
        }
    }

    abstract Object executeGeneric(Object obj) throws UnsupportedMessageException, InvalidArrayIndexException;

    @Specialization(guards = {
        "interopLibrary.hasMembers(o)"
    }, limit = "3")
    Object doTruffleInterop(
        Object o,
        @CachedLibrary("o") InteropLibrary interopLibrary
    ) throws UnsupportedMessageException, InvalidArrayIndexException {
        InteropLibrary uncached = InteropLibrary.getUncached();
        var membersInterop = interopLibrary.getMembers(o);
        var copy = new Object[(int) uncached.getArraySize(membersInterop)];
        for (int i = 0; i < copy.length; i++) {
            var fieldInterop = uncached.readArrayElement(membersInterop, i);
            var field = uncached.asString(fieldInterop);
            copy[i] = field;
        }
        return new LispVector(copy);
    }

    @Fallback
    Object fallback(Object o) {
        var ctx = ISLISPContext.get(this);
        var expectedClass = ctx.lookupClass("<truffle-object>");
        return errorSignalerNode.signalWrongType(o, expectedClass);
    }

    /**
     * Construct LispFunction using this root node.
     *
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPTruffleObjectFieldsNodeGen.create(lang).getCallTarget());
    }

}
