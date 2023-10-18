package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.nodes.ISLISPTypes;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.dsl.TypeSystemReference;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.RootNode;

@TypeSystemReference(ISLISPTypes.class)
public abstract class ISLISPAdd extends RootNode {

    @Child
    private ISLISPErrorSignalerNode errorSignalerNode;

    public ISLISPAdd(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode();
    }

    abstract Object executeGeneric(Object a, Object b);

    @Override
    @ExplodeLoop
    public final Object execute(VirtualFrame frame) {
        Number sum = 0;
        for (int i = 1; i < frame.getArguments().length; i++) {
            sum = (Number) executeGeneric(sum, frame.getArguments()[i]);
        }
        return sum;
    }

    @Specialization(rewriteOn = ArithmeticException.class)
    int doInts(int a, int b) {
        return Math.addExact(a, b);
    }

    @Specialization
    double doDoubles(double a, double b) {
        return a + b;
    }

    @Fallback
    Object notNumbers(Object a, Object b) {
        var ctx = ISLISPContext.get(this);
        var numberClass = ctx.lookupClass(ctx.namedSymbol("<number>").identityReference());
        return errorSignalerNode.signalWrongType(b, numberClass);
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPAddNodeGen.create(lang).getCallTarget());
    }

}
