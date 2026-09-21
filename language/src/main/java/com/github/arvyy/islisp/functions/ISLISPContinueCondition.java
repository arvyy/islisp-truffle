package com.github.arvyy.islisp.functions;

import java.util.List;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPContinueException;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * Implements `conditinue-condition` function, which defers condition handling to the next handler.
 */
public class ISLISPContinueCondition extends RootNode {

    private final static String NAME = "continue-condition";
    private final static List<String> ARGS = List.of("condition", "[value]");
    private final static String REST_ARG = null;
    private final static String RETURN_TYPE = null;
    private final static String DESC = "\"Continues\" from condition by finding the call to signal-condition and arranging for it to perform a normal return of the value, which defaults to nil. The consequences are undefined if the condition is not continuable.";
    
    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPContinueCondition(TruffleLanguage<?> language) {
        super(language);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 3 && frame.getArguments().length != 2) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length - 1, 1, 2);
        }
        Object value;
        if (frame.getArguments().length == 2) {
            value = ISLISPContext.get(this).getNil();
        } else {
            value = frame.getArguments()[2];
        }
        var condition = frame.getArguments()[1];
        throw new ISLISPContinueException(condition, value);
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(
            new LispFunction.Documentation(NAME, ARGS, REST_ARG, RETURN_TYPE, DESC),
            new ISLISPContinueCondition(lang).getCallTarget());
    }
}
