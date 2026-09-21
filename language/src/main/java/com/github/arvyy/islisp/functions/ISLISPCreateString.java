package com.github.arvyy.islisp.functions;

import java.util.List;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispChar;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispMutableString;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * Implements `create-string` function.
 */
public abstract class ISLISPCreateString extends RootNode {

    private final static String NAME = "create-string";
    private final static List<String> ARGS = List.of("i", "[initial-character]");
    private final static String REST_ARG = null;
    private final static String RETURN_TYPE = "<string>";
    private final static String DESC = "Returns a string of length i. If initial-character is given, then the characters of the new string are initialized with this character, otherwise the initialization is implementation defined. An error shall be signaled if the requested string cannot be allocated (error-id. cannot-create-string). An error shall be signaled if i is not a non-negative integer or if initial-character is not a character (error-id. domain-error).";
    
    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPCreateString(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 2 && frame.getArguments().length != 3) {
            return errorSignalerNode.signalWrongArgumentCount(
                frame.getArguments().length - 1, 1, 2);
        }
        var count = frame.getArguments()[1];
        var character = frame.getArguments().length == 3
            ? frame.getArguments()[2]
            : new LispChar('?');
        return executeGeneric(count, character);
    }

    abstract Object executeGeneric(Object count, Object character);

    @Specialization
    Object doProper(int count, LispChar lispChar) {
        var datum = new LispChar[count];
        for (int i = 0; i < count; i++) {
            datum[i] = lispChar;
        }
        return new LispMutableString(datum);
    }

    @Fallback
    Object fallback(Object count, Object lispChar) {
        var ctx = ISLISPContext.get(this);
        if (lispChar instanceof  LispChar) {
            return errorSignalerNode.signalWrongType(count, ctx.lookupClass("<number>"));
        } else {
            return errorSignalerNode.signalWrongType(lispChar, ctx.lookupClass("<character>"));
        }
    }

    /**
     * Construct LispFunction using this root node.
     *
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(
            new LispFunction.Documentation(NAME, ARGS, REST_ARG, RETURN_TYPE, DESC),
            ISLISPCreateStringNodeGen.create(lang).getCallTarget());
    }

}
