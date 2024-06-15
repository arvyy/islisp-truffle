package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.nodes.ISLISPTypes;
import com.github.arvyy.islisp.nodes.ISLISPTypesGen;
import com.github.arvyy.islisp.runtime.Closure;
import com.github.arvyy.islisp.runtime.LispBigInteger;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.dsl.TypeSystemReference;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;


/**
 * Implements numeric subtraction function `-`.
 */
@TypeSystemReference(ISLISPTypes.class)
@ReportPolymorphism
public abstract class ISLISPSubtract extends RootNode {

    @Override
    public boolean isCloningAllowed() {
        return true;
    }

    @Child
    private ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPSubtract(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public String getName() {
        return "-";
    }

    @Override
    @CompilerDirectives.TruffleBoundary
    public SourceSection getSourceSection() {
        return Source.newBuilder("islisp", "", ISLISPSubtract.class.getSimpleName())
            .internal(true)
            .build()
            .createSection(1);
    }

    @Specialization(
        guards = { "isSingleDouble(frame)" }
    )
    Object doSingleDouble(VirtualFrame frame) {
        return -(ISLISPTypesGen.asImplicitDouble(frame.getArguments()[1]));
    }

    static boolean isSingleDouble(VirtualFrame frame) {
        var args = frame.getArguments();
        return args.length == 2 && ISLISPTypesGen.isImplicitDouble(args[1]);
    }

    @Specialization(
        guards = { "isSingleInt(frame)" }
    )
    Object doSingleInt(VirtualFrame frame) {
        return -(ISLISPTypesGen.asInteger(frame.getArguments()[1]));
    }

    static boolean isSingleInt(VirtualFrame frame) {
        var args = frame.getArguments();
        return args.length == 2 && ISLISPTypesGen.isInteger(args[1]);
    }

    @Specialization(
        guards = { "isSingleBigInt(frame)" }
    )
    Object doSingleBigInt(VirtualFrame frame) {
        return ISLISPTypesGen.asImplicitLispBigInteger(frame.getArguments()[1]).negate();
    }

    static boolean isSingleBigInt(VirtualFrame frame) {
        var args = frame.getArguments();
        return args.length == 2 && ISLISPTypesGen.isImplicitLispBigInteger(args[1]);
    }

    @Specialization(
        rewriteOn = { ArithmeticException.class },
        guards = { "isTwoInts(frame)" }
    )
    Object doTwoInts(VirtualFrame frame) {
        var args = frame.getArguments();
        return Math.subtractExact((int) args[1], (int) args[2]);
    }

    static boolean isTwoInts(VirtualFrame frame) {
        var args = frame.getArguments();
        return args.length == 3
            && ISLISPTypesGen.isInteger(args[1])
            && ISLISPTypesGen.isInteger(args[2]);
    }

    @Specialization(
        guards = { "isTwoDoubles(frame)" }
    )
    Object doTwoDoubles(VirtualFrame frame) {
        var args = frame.getArguments();
        return ISLISPTypesGen.asImplicitDouble(args[1]) - ISLISPTypesGen.asImplicitDouble(args[2]);
    }

    static boolean isTwoDoubles(VirtualFrame frame) {
        var args = frame.getArguments();
        return args.length == 3
            && ISLISPTypesGen.isImplicitDouble(args[1])
            && ISLISPTypesGen.isImplicitDouble(args[2]);
    }

    @Fallback
    Object doFallback(VirtualFrame frame) {
        var args = frame.getArguments();
        var l = args.length;
        if (l == 2) {
            if (ISLISPTypesGen.isInteger(args[1])) {
                return -ISLISPTypesGen.asInteger(args[1]);
            }
            if (ISLISPTypesGen.isImplicitDouble(args[1])) {
                return -ISLISPTypesGen.asDouble(args[1]);
            }
            if (ISLISPTypesGen.isImplicitLispBigInteger(args[1])) {
                return ISLISPTypesGen.asLispBigInteger(args[1]).negate();
            }
            return signalError(args[1]);
        }
        int sum = 0;
        for (int i = 1; i < l; i++) {
            try {
                if (i == 1) {
                    sum = ISLISPTypesGen.expectInteger(args[1]);
                } else {
                    sum = Math.subtractExact(sum, ISLISPTypesGen.expectInteger(args[i]));
                }
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
                if (i == 1) {
                    sum = ISLISPTypesGen.asImplicitLispBigInteger(args[i]);
                } else {
                    sum = sum.add(ISLISPTypesGen.asImplicitLispBigInteger(args[i]).negate());
                }
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
                if (i == 1) {
                    sum = ISLISPTypesGen.asImplicitDouble(args[i]);
                } else {
                    sum -= ISLISPTypesGen.asImplicitDouble(args[i]);
                }
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
        var callTarget = ISLISPSubtractNodeGen.create(lang).getCallTarget();
        return new LispFunction(
            new Closure(null, null, null),
            callTarget,
            false,
            true);
    }

}
