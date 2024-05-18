package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.nodes.ISLISPTypes;
import com.github.arvyy.islisp.nodes.ISLISPTypesGen;
import com.github.arvyy.islisp.runtime.LispBigInteger;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.dsl.TypeSystemReference;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.nodes.UnexpectedResultException;

/**
 * Implements numeric adition function `+`.
 */
@TypeSystemReference(ISLISPTypes.class)
public class ISLISPAdd extends RootNode {

    @Override
    public boolean isCloningAllowed() {
        return true;
    }

    @Child
    private ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPAdd(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        int sum = 0;
        var args = frame.getArguments();
        var l = args.length;
        for (int i = 1; i < l; i++) {
            try {
                sum = Math.addExact(sum, ISLISPTypesGen.expectInteger(args[i]));
            } catch (ArithmeticException ignored) {
                return continueWithBigInts(frame, i, ISLISPTypesGen.asImplicitLispBigInteger(sum));
            } catch (UnexpectedResultException ignored) {
                Object v = args[i];
                if (ISLISPTypesGen.isDouble(v)) {
                    return continueWithDoubles(frame, i, ISLISPTypesGen.asImplicitDouble(sum));
                } else if (ISLISPTypesGen.isLispBigInteger(v)) {
                    return continueWithBigInts(frame, i, ISLISPTypesGen.asImplicitLispBigInteger(sum));
                } else {
                    return signalError(v);
                }
            }
        }
        return sum;
    }

    Object continueWithBigInts(VirtualFrame frame, int index, LispBigInteger prevSum) {
        var sum = prevSum;
        var args = frame.getArguments();
        var l = args.length;
        for (int i = index; i < l; i++) {
            try {
                sum = sum.add(ISLISPTypesGen.asImplicitLispBigInteger(args[i]));
            } catch (IllegalArgumentException ignored) {
                Object v = args[i];
                if (ISLISPTypesGen.isDouble(v)) {
                    return continueWithDoubles(frame, i, ISLISPTypesGen.asImplicitDouble(sum));
                } else {
                    return signalError(v);
                }
            }
        }
        return sum;
    }

    Object continueWithDoubles(VirtualFrame frame, int index, double prevSum) {
        var sum = prevSum;
        var args = frame.getArguments();
        var l = args.length;
        for (int i = index; i < l; i++) {
            try {
                sum += ISLISPTypesGen.asImplicitDouble(args[i]);
            } catch (IllegalArgumentException ignored) {
                Object v = args[i];
                return signalError(v);
            }
        }
        return sum;
    }

    Object signalError(Object value) {
        var ctx = ISLISPContext.get(this);
        var numberClass = ctx.lookupClass("<number>");
        return errorSignalerNode.signalWrongType(value, numberClass);
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPAdd(lang).getCallTarget());
    }

}
