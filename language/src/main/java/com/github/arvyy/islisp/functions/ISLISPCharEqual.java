package com.github.arvyy.islisp.functions;

import java.util.List;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispChar;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `char=` function.
 */
public abstract class ISLISPCharEqual extends RootNode {
    
    private final static String NAME = "char=";
    private final static List<String> ARGS = List.of("char1", "char2");
    private final static String REST_ARG = null;
    private final static String RETURN_TYPE = "boolean";
    private final static String DESC = "The function char= tests whether char1 is the same character as char2.";

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPCharEqual(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public String getName() {
        return "char=";
    }

    @Override
    @CompilerDirectives.TruffleBoundary
    public SourceSection getSourceSection() {
        return Source.newBuilder("islisp", "", ISLISPCharEqual.class.getSimpleName())
            .internal(true)
            .build()
            .createSection(1);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 3) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length, 2, 2);
        }
        return executeGeneric(frame.getArguments()[1], frame.getArguments()[2]);
    }

    abstract Object executeGeneric(Object a, Object b);

    @Specialization
    Object doProper(LispChar a, LispChar b) {
        var ctx = ISLISPContext.get(this);
        return a.codepoint() == b.codepoint() ? ctx.getT() : ctx.getNil();
    }

    @Fallback
    Object fallback(Object a, Object b) {
        var ctx = ISLISPContext.get(this);
        var offender = (a instanceof LispChar) ? b : a;
        return errorSignalerNode.signalWrongType(offender, ctx.lookupClass("<character>"));
    }


    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(
            new LispFunction.Documentation(NAME, ARGS, REST_ARG, RETURN_TYPE, DESC),
            ISLISPCharEqualNodeGen.create(lang).getCallTarget());
    }
}
