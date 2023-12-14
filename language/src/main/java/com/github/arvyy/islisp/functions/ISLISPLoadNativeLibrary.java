package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispNativeLibrary;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.Source;

/**
 * Implements `load-native-library` procedure for FFI.
 */
public class ISLISPLoadNativeLibrary extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPLoadNativeLibrary(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        var ctx = ISLISPContext.get(this);
        if (frame.getArguments().length != 2) {
            return errorSignalerNode.signalWrongArgumentCount(
                frame.getArguments().length - 1, 1, 1);
        }
        var name = frame.getArguments()[1];
        if (name instanceof String nameString) {
            return executeBoundary(nameString);
        }
        return errorSignalerNode.signalWrongType(name, ctx.lookupClass("<string>"));
    }

    @CompilerDirectives.TruffleBoundary
    Object executeBoundary(String name) {
        var ctx = ISLISPContext.get(this);
        var source = Source.newBuilder(
            "nfi",
            "load \"" + name + "\"",
            "load-native-library").build();
        var library = ctx.getEnv().parseInternal(source);
        return new LispNativeLibrary(library.call());
    }

    /**
     * Construct LispFunction using this root node.
     *
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPLoadNativeLibrary(lang).getCallTarget());
    }
}
