package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.Source;

/**
 * Implements `eval` function used for interop with other truffle languages.
 */
public class ISLISPEval extends RootNode {

    @Child
    private ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPEval(TruffleLanguage<?> lang) {
        super(lang);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 3) {
            return errorSignalerNode.signalWrongArgumentCount(
                frame.getArguments().length - 1,
                2,
                2);
        }
        var lang = (String) frame.getArguments()[1];
        var script = (String) frame.getArguments()[2];
        return executeBoundary(lang, script);
    }

    @CompilerDirectives.TruffleBoundary
    Object executeBoundary(String lang, String script) {
        var ctx = ISLISPContext.get(this);
        var env = ctx.getEnv();
        try {
            var source = Source.newBuilder(lang, script, "<eval>").build();
            var target = env.parsePublic(source);
            return target.call();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Construct LispFunction using this root node.
     *
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPEval(lang).getCallTarget());
    }
}
