package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

/**
 * ISLISP invocable function object.
 *
 * @param closure necessary context to be passed when calling.
 * @param callTarget function implementation
 * @param isGeneric whether this is plain or generic function.
 */
@ExportLibrary(InteropLibrary.class)
public record LispFunction(Closure closure, CallTarget callTarget, boolean isGeneric) implements TruffleObject {

    @ExportMessage
    String toDisplayString(boolean ignored) {
        if (isGeneric) {
            return "#<generic function>";
        }
        if (closure != null) {
            return "#<closure>";
        }
        return "#<function>";
    }

    /**
     * Create plain lisp function.
     * @param callTarget call target
     */
    public LispFunction(CallTarget callTarget) {
        this(new Closure(null, null, null), callTarget, false);
    }

    /**
     * Create lisp function with given frame as closure.
     * @param frame materialized closure frame
     * @param callTarget call target
     */
    public LispFunction(MaterializedFrame frame, CallTarget callTarget) {
        this(new Closure(frame, null, null), callTarget, false);
    }

    /**
     * Create lisp function with given generic invocation context.
     *
     * @param nextMethods applicable next methods
     * @param args initial invocation arguments
     * @param callTarget call target
     */
    public LispFunction(GenericMethodApplicableMethods nextMethods, Object[] args, CallTarget callTarget) {
        this(new Closure(null, nextMethods, args), callTarget, true);
    }

    /**
     * Create lisp function with given closure value.
     * @param closure closure
     * @param callTarget call target
     */
    public LispFunction(Closure closure, CallTarget callTarget) {
        this(closure, callTarget, false);
    }

}
