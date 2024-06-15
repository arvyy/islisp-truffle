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
import com.oracle.truffle.api.TruffleSafepoint;
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
 * Implements numeric adition function `+`.
 */
@TypeSystemReference(ISLISPTypes.class)
@ReportPolymorphism
public abstract class ISLISPAdd extends RootNode {

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
    public String getName() {
        return "+";
    }

    @Override
    @CompilerDirectives.TruffleBoundary
    public SourceSection getSourceSection() {
        return Source.newBuilder("islisp", "", ISLISPAdd.class.getSimpleName())
            .internal(true)
            .build()
            .createSection(1);
    }

    @Specialization(
        guards = { "isSingleNumber(frame)" }
    )
    Object doSingleNumber(VirtualFrame frame) {
        return frame.getArguments()[1];
    }

    static boolean isSingleNumber(VirtualFrame frame) {
        var args = frame.getArguments();
        return args.length == 2
            && (ISLISPTypesGen.isImplicitLispBigInteger(args[1])
                || ISLISPTypesGen.isImplicitDouble(args[1]));
    }

    @Specialization(
        rewriteOn = { ArithmeticException.class },
        guards = { "isTwoInts(frame)" }
    )
    Object doTwoInts(VirtualFrame frame) {
        var args = frame.getArguments();
        return Math.addExact((int) args[1], (int) args[2]);
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
        return ISLISPTypesGen.asImplicitDouble(args[1]) + ISLISPTypesGen.asImplicitDouble(args[2]);
    }

    static boolean isTwoDoubles(VirtualFrame frame) {
        var args = frame.getArguments();
        return args.length == 3
            && ISLISPTypesGen.isImplicitDouble(args[1])
            && ISLISPTypesGen.isImplicitDouble(args[2]);
    }

    @Fallback
    Object doFallback(VirtualFrame frame) {
        int sum = 0;
        var args = frame.getArguments();
        var l = args.length;
        for (int i = 1; i < l; i++) {
            try {
                sum = Math.addExact(sum, ISLISPTypesGen.expectInteger(args[i]));
                TruffleSafepoint.poll(this);
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
                TruffleSafepoint.poll(this);
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
                TruffleSafepoint.poll(this);
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
        var callTarget = ISLISPAddNodeGen.create(lang).getCallTarget();
        return new LispFunction(
            new Closure(null, null, null),
            callTarget,
            false,
            true);
    }

}
