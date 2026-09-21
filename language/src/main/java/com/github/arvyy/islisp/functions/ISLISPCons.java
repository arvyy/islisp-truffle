package com.github.arvyy.islisp.functions;

import java.util.List;

import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Pair;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * Implements `cons` function, creates a new pair from given two arguments.
 */
public class ISLISPCons extends RootNode {
    
    private final static String NAME = "cons";
    private final static List<String> ARGS = List.of("obj1", "obj2");
    private final static String REST_ARG = null;
    private final static String RETURN_TYPE = "<cons>";
    private final static String DESC = "This function builds a cons from two objects, with obj1 as its car (or `left') part and with obj2 as its cdr (or `right') part. An error shall be signaled if the requested cons cannot be allocated (error-id. cannot-create-cons). Both obj1 and obj2 may be any ISLISP object.";

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPCons(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 3) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length - 1, 2, 2);
        }
        return new Pair(frame.getArguments()[1], frame.getArguments()[2]);
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(
            new LispFunction.Documentation(NAME, ARGS, REST_ARG, RETURN_TYPE, DESC),
            new ISLISPCons(lang).getCallTarget());
    }
}
