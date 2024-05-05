package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.Utils;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispChar;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispStream;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.RootNode;

import java.io.IOException;

/**
 * Implements `preview-char` procedure.
 */
public abstract class ISLISPPreviewChar extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    @Child
    DirectCallNode standardInput;

    ISLISPPreviewChar(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    abstract Object executeGeneric(Object stream, Object eosErrorP, Object eosValue);

    @CompilerDirectives.TruffleBoundary
    DirectCallNode getStandardInput() {
        if (standardInput == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            var ctx = ISLISPContext.get(this);
            var callNode = DirectCallNode.create(
                ctx.lookupFunction("ROOT", ctx.namedSymbol("standard-input")
                        .identityReference())
                    .callTarget());
            standardInput = insert(callNode);
        }
        return standardInput;
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length > 4) {
            return errorSignalerNode.signalWrongArgumentCount(
                frame.getArguments().length - 1,
                0,
                3
            );
        }
        var ctx = ISLISPContext.get(this);
        Object stream;
        Object eosErrorp;
        Object eosValue;
        if (frame.getArguments().length < 2) {
            stream = getStandardInput().call();
        } else {
            stream = frame.getArguments()[1];
        }
        if (frame.getArguments().length < 3) {
            eosErrorp = ctx.getT();
        } else {
            eosErrorp = frame.getArguments()[2];
        }
        if (frame.getArguments().length < 4) {
            eosValue = ctx.getNil();
        } else {
            eosValue = frame.getArguments()[3];
        }
        return executeGeneric(stream, eosErrorp, eosValue);
    }

    @Specialization
    @CompilerDirectives.TruffleBoundary
    Object doProper(
        LispStream stream,
        Object eosErrorP,
        Object eosValue
    ) {
        try {
            stream.mark(4);
            var codepoint = stream.readCodepoint();
            stream.reset();
            if (codepoint != -1) {
                return new LispChar(codepoint);
            }
            if (Utils.isNil(eosErrorP)) {
                return eosValue;
            } else {
                return errorSignalerNode.signalEndOfStream();
            }
        } catch (IOException e) {
            return errorSignalerNode.signalIOError(e);
        }
    }

    @Fallback
    Object doFallback(Object stream, Object eosErrorP, Object eosValue) {
        var ctx = ISLISPContext.get(this);
        return errorSignalerNode.signalWrongType(stream, ctx.lookupClass("<stream>"));
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPPreviewCharNodeGen.create(lang).getCallTarget());
    }

}
