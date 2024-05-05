package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispStream;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.RootNode;

import java.io.IOException;
import java.nio.file.StandardOpenOption;
import java.util.Set;

/**
 * Implements `open-input-file` function.
 */
public abstract class ISLISPOpenInputFile extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPOpenInputFile(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        var args = frame.getArguments().length;
        if (args != 2 && args != 3) {
            return errorSignalerNode.signalWrongArgumentCount(args - 1, 1, 2);
        }
        return executeGeneric(frame.getArguments()[1]);
    }

    abstract Object executeGeneric(Object filename);

    @Specialization
    @CompilerDirectives.TruffleBoundary
    Object doString(String filename) {
        var file = ISLISPContext.get(this).getEnv().getPublicTruffleFile(filename);
        try {
            var channel = file.newByteChannel(Set.of(StandardOpenOption.READ));
            return new LispStream(channel, true, false);
        } catch (IOException e) {
            return errorSignalerNode.signalIOError(e);
        }
    }

    @Specialization(guards = {
        "interopLibrary.isString(o)"
    }, limit = "3")
    Object doInterop(
        Object o,
        @CachedLibrary(value = "o") InteropLibrary interopLibrary
    ) {
        try {
            return doString(interopLibrary.asString(o));
        } catch (UnsupportedMessageException e) {
            return errorSignalerNode.signalTruffleInteropError(e);
        }
    }

    @Fallback
    Object doFallback(Object obj) {
        return errorSignalerNode.signalWrongType(obj, ISLISPContext.get(this).lookupClass("<string>"));
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPOpenInputFileNodeGen.create(lang).getCallTarget());
    }

}
