package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.Utils;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispStream;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * implements `input-stream-p` function.
 */
public class ISLISPInputStreamp extends RootNode {

    @Child
    private ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPInputStreamp(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 2) {
            return errorSignalerNode.signalWrongArgumentCount(
                frame.getArguments().length - 1,
                1, 1
            );
        }
        var obj = frame.getArguments()[1];
        var isInput = (obj instanceof LispStream s && s.inputStream() != null);
        return Utils.booleanToSymbol(isInput);
    }

    /**
     * Construct LispFunction using this root node.
     *
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPInputStreamp(lang).getCallTarget());
    }
}
