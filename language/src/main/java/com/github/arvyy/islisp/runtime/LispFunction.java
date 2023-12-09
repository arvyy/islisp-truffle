package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.source.SourceSection;

/**
 * ISLISP invocable function object.
 *
 * @param closure necessary context to be passed when calling.
 * @param callTarget function implementation
 * @param isGeneric whether this is plain or generic function.
 */
@ExportLibrary(InteropLibrary.class)
public record LispFunction(Closure closure, RootCallTarget callTarget, boolean isGeneric) implements TruffleObject {

    /**
     * Create plain lisp function.
     * @param callTarget call target
     */
    public LispFunction(RootCallTarget callTarget) {
        this(new Closure(null, null, null), callTarget, false);
    }

    /**
     * Create lisp function with given frame as closure.
     * @param frame materialized closure frame
     * @param callTarget call target
     */
    public LispFunction(MaterializedFrame frame, RootCallTarget callTarget) {
        this(new Closure(frame, null, null), callTarget, false);
    }

    /**
     * Create lisp function with given generic invocation context.
     *
     * @param nextMethods applicable next methods
     * @param args initial invocation arguments
     * @param callTarget call target
     */
    public LispFunction(GenericMethodApplicableMethods nextMethods, Object[] args, RootCallTarget callTarget) {
        this(new Closure(null, nextMethods, args), callTarget, true);
    }

    /**
     * Create lisp function with given closure value.
     * @param closure closure
     * @param callTarget call target
     */
    public LispFunction(Closure closure, RootCallTarget callTarget) {
        this(closure, callTarget, false);
    }

    @ExportMessage
    boolean hasSourceLocation() {
        return true;
    }

    @ExportMessage
    SourceSection getSourceLocation() {
        return callTarget.getRootNode().getSourceSection();
    }

    @ExportMessage
    boolean isExecutable() {
        return true;
    }

    @ExportMessage
    @ReportPolymorphism
    abstract static class Execute {
        @ExplodeLoop
        @Specialization(guards = "function.callTarget() == prevCallTarget")
        static Object doDirect(
            LispFunction function,
            Object[] args,
            @Cached("function.callTarget()") RootCallTarget prevCallTarget,
            @Cached("create(function.callTarget())") DirectCallNode callNode) {
            var realArgs = new Object[args.length + 1];
            realArgs[0] = function.closure();
            System.arraycopy(args, 0, realArgs, 1, args.length);
            return callNode.call(realArgs);
        }

        @ExplodeLoop
        @Specialization(replaces = "doDirect")
        static Object doIndirect(
            LispFunction function,
            Object[] args,
            @Cached IndirectCallNode callNode) {
            var realArgs = new Object[args.length + 1];
            realArgs[0] = function.closure();
            System.arraycopy(args, 0, realArgs, 1, args.length);
            return callNode.call(function.callTarget(), realArgs);
        }
    }

}
