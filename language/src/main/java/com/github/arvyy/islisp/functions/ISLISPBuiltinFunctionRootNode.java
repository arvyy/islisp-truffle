package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * Common abstract class for built in functions. Handles validating arg count during execution.
 */
public abstract class ISLISPBuiltinFunctionRootNode extends RootNode {

    private final String name;
    private final int argcMask;

    @Child
    protected ISLISPErrorSignalerNode errorSignalerNode;

    protected ISLISPBuiltinFunctionRootNode(
        String name,
        int argcMask,
        TruffleLanguage<?> language
    ) {
        super(language);
        this.name = name;
        this.argcMask = argcMask;
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        var argc = frame.getArguments().length - 1;
        if (argc <= Integer.SIZE) {
            if (((1 << argc) & argcMask) == 0) {
                var min = Integer.numberOfTrailingZeros(argcMask);
                var max = Integer.SIZE - Integer.numberOfLeadingZeros(argcMask);
                if (max == Integer.SIZE) {
                    max = -1;
                }
                return errorSignalerNode.signalWrongArgumentCount(
                    argc,
                    min,
                    max);
            }
        }
        return executeImpl(frame);
    }

    protected abstract Object executeImpl(VirtualFrame frame);

    /**
    * Construct argc mask for [min; +inf) args.
    * @param min minimum amount of args
    * @return argc mask
    */
    public static int vararg(int min) {
        if (min < 0) {
            throw new IllegalArgumentException();
        }
        if (min > Integer.SIZE) {
            return 0;
        }
        return -1 << min;
    }

    /**
    * Construct argc mask for fixed count of args.
    * @param count amount of args
    * @return argc mask
    */
    public static int arg(int count) {
        if (count < 0) {
            throw new IllegalArgumentException();
        }
        if (count > Integer.SIZE) {
            return 0;
        }
        return 1 << count;
    }

    /**
    * Construct argc mask for [min; max] args.
    * @param min minimum amount of args
    * @param max maximum amount of args
    * @return argc mask
    */
    public static int arg(int min, int max) {
        if (min < 0) {
            throw new IllegalArgumentException();
        }
        if (max < 0) {
            throw new IllegalArgumentException();
        }
        if (max < min) {
            throw new IllegalArgumentException();
        }
        if (min > Integer.SIZE) {
            return 0;
        }
        int minMask = -1 << min;
        int maxMask = -1 >> (Integer.SIZE - max);
        return minMask & maxMask;
    }

    /**
    * Create LispFunction runtime object from this node's call target.
    * @return lisp function
    */
    public LispFunction makeLispFunction() {
        return new LispFunction(null, getCallTarget(), false, false);
    }

}
