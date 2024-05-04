package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * Implements `truffle-object-field` method.
 */
public abstract class ISLISPSetTruffleObjectField extends RootNode {

    @Child
    private ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPSetTruffleObjectField(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 4) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length - 1, 3, 3);
        }
        try {
            return executeGeneric(
                frame.getArguments()[1],
                frame.getArguments()[2],
                frame.getArguments()[3]);
        } catch (InteropException e) {
            throw new ISLISPError("Error writing truffle-object field", this);
        }
    }

    abstract Object executeGeneric(Object value, Object obj, Object member)
        throws InteropException;

    @Specialization(guards = {
        "interopLibrary.hasMembers(o)"
    }, limit = "3")
    Object doTruffleInterop(
        Object value,
        Object o,
        String member,
        @CachedLibrary("o") InteropLibrary interopLibrary
    ) throws InteropException {
        interopLibrary.writeMember(o, member, value);
        return value;
    }

    @Fallback
    Object fallback(Object value, Object o, Object member) {
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
        return new LispFunction(ISLISPSetTruffleObjectFieldNodeGen.create(lang).getCallTarget());
    }
}
