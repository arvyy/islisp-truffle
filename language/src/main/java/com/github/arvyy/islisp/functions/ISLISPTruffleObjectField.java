package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
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
public abstract class ISLISPTruffleObjectField extends RootNode {

    @Child
    private ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPTruffleObjectField(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 3) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length - 1, 2, 2);
        }
        try {
            return executeGeneric(frame.getArguments()[1], frame.getArguments()[2]);
        } catch (InteropException e) {
            return errorSignalerNode.signalTruffleInteropError(e);
        }
    }

    abstract Object executeGeneric(Object obj, Object member)
        throws InteropException;

    @Specialization(guards = {
        "interopLibrary.hasMembers(o)"
    }, limit = "3")
    Object doTruffleInterop(
        Object o,
        String member,
        @CachedLibrary("o") InteropLibrary interopLibrary
    ) throws InteropException {
        return interopLibrary.readMember(o, member);
    }

    @Fallback
    Object fallback(Object o, Object member) {
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
        return new LispFunction(ISLISPTruffleObjectFieldNodeGen.create(lang).getCallTarget());
    }
}
