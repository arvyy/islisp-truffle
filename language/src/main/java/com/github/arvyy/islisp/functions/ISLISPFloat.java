package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.nodes.ISLISPTypes;
import com.github.arvyy.islisp.runtime.Closure;
import com.github.arvyy.islisp.runtime.LispBigInteger;
import com.github.arvyy.islisp.runtime.LispClass;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.dsl.TypeSystemReference;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `float` function.
 */
//TODO most-positive-float, most-negative-float
@TypeSystemReference(ISLISPTypes.class)
public abstract class ISLISPFloat extends RootNode {

    @Override
    public boolean isCloningAllowed() {
        return true;
    }

    @Child
    private ISLISPErrorSignalerNode errorSignalerNode;

    @CompilerDirectives.CompilationFinal
    private LispClass numberClass;

    ISLISPFloat(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    @CompilerDirectives.TruffleBoundary
    public SourceSection getSourceSection() {
        return Source.newBuilder("islisp", "", ISLISPFloat.class.getSimpleName())
            .internal(true)
            .build()
            .createSection(1);
    }

    @Override
    public String getName() {
        return "float";
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 2) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length - 1, 1, 1);
        }
        return executeGeneric(frame.getArguments()[1]);
    }

    abstract Object executeGeneric(Object arg);


    @Specialization
    double executeInt(int i) {
        return (double) i;
    }

    @Specialization
    double executeFloat(double d) {
        return d;
    }

    @Specialization
    double executeBigNum(LispBigInteger b) {
        return b.data().doubleValue();
    }

    @Fallback
    Object fallback(Object o) {
        if (numberClass == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            numberClass = ISLISPContext.get(this).lookupClass("<number>");
        }
        return errorSignalerNode.signalDomainError("Can't use 'float' function on not a number", o, numberClass);
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        var callTarget = ISLISPFloatNodeGen.create(lang).getCallTarget();
        return new LispFunction(
            new Closure(null, null, null),
            callTarget,
            false,
            true);
    }

}
