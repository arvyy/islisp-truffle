package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispStream;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * Implements `get-output-stream-string` function.
 */
public class ISLISPGetOutputStreamString extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPGetOutputStreamString(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 2) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length - 1, 1, 1);
        }
        var arg = frame.getArguments()[1];
        if (arg instanceof LispStream s) {
            return executeBoundary(s);
        }
        return errorSignalerNode.signalNotStringOutputStream(arg);
    }

    @CompilerDirectives.TruffleBoundary
    Object executeBoundary(LispStream s) {
        var str = s.getOutputString();
        if (str.isPresent()) {
            return str.get();
        }
        return errorSignalerNode.signalNotStringOutputStream(s);
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPGetOutputStreamString(lang).getCallTarget());
    }
}
