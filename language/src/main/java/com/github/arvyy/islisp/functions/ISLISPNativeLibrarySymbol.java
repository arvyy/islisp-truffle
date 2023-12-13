package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispNativeLibrary;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.Source;

/**
 * Implements `native-library-symbol` procedure for extracting functions / values bound by symbol.
 */
public abstract class ISLISPNativeLibrarySymbol extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPNativeLibrarySymbol(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 4) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length - 1, 3, 3);
        }
        try {
            return executeGeneric(frame.getArguments()[1], frame.getArguments()[2], frame.getArguments()[3]);
        } catch (Exception e) {
            //TODO
            throw new ISLISPError("Error reading native symbol", this);
        }
    }

    abstract Object executeGeneric(Object nativeLibrary, Object symbol, Object signature)
        throws InteropException;

    @Specialization(limit = "3")
    Object doProper(
        LispNativeLibrary library,
        String symbol,
        String signature
    ) throws InteropException {
        var interopLibrary = InteropLibrary.getUncached();
        var signatureSource = Source
            .newBuilder("nfi", signature, "native-library-symbol")
            .build();
        var signatureValue = ISLISPContext.get(this).getEnv().parseInternal(signatureSource).call();
        var symbolValue = interopLibrary.readMember(library.nativeLibrary(), symbol);
        return interopLibrary.invokeMember(signatureValue, "bind", symbolValue);
    }

    /**
     * Construct LispFunction using this root node.
     *
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPNativeLibrarySymbolNodeGen.create(lang).getCallTarget());
    }

}
