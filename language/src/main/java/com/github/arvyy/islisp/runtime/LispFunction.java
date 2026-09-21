package com.github.arvyy.islisp.runtime;

import java.util.List;

import com.oracle.truffle.api.CallTarget;
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
 * @param inline whether this function should be inlined when called directly.
 */
@ExportLibrary(InteropLibrary.class)
public record LispFunction(
    Documentation documentation,
    Closure closure,
    CallTarget callTarget,
    boolean isGeneric,
    boolean inline
) implements TruffleObject {

    public record Documentation(
        String name,
        List<String> arguments,
        String restArgument,
        String returnType,
        String docstring
    ) {}

    // TODO remove once callsites add docs
    public LispFunction(
        Closure closure,
        CallTarget callTarget,
        boolean isGeneric,
        boolean inline
    ) {
        this(null, closure, callTarget, isGeneric, inline);
    }
    
    /**
     * Create plain lisp function.
     * @param callTarget call target
     */
    public LispFunction(Documentation documentation, CallTarget callTarget) {
        this(documentation, new Closure(null, null, null), callTarget, false, false);
    }
    // TODO remove once callsites add docs
    public LispFunction(CallTarget callTarget) {
        this(null, new Closure(null, null, null), callTarget, false, false);
    }

    /**
     * Create lisp function with given frame as closure.
     * @param frame materialized closure frame
     * @param callTarget call target
     */
    public LispFunction(Documentation documentation, MaterializedFrame frame, CallTarget callTarget) {
        this(documentation, new Closure(frame, null, null), callTarget, false, false);
    }
    // TODO remove once callsites add docs
    public LispFunction(MaterializedFrame frame, CallTarget callTarget) {
        this(null, new Closure(frame, null, null), callTarget, false, false);
    }

    /**
     * Create lisp function with given generic invocation context.
     *
     * @param nextMethods applicable next methods
     * @param args initial invocation arguments
     * @param callTarget call target
     */
    public LispFunction(Documentation documentation, GenericMethodApplicableMethods nextMethods, Object[] args, CallTarget callTarget) {
        this(documentation, new Closure(null, nextMethods, args), callTarget, true, false);
    }
    // TODO remove once callsites add docs
    public LispFunction(GenericMethodApplicableMethods nextMethods, Object[] args, CallTarget callTarget) {
        this(null, new Closure(null, nextMethods, args), callTarget, true, false);
    }

    /**
     * Create lisp function with given closure value.
     * @param closure closure
     * @param callTarget call target
     */
    public LispFunction(Documentation documentation, Closure closure, CallTarget callTarget) {
        this(documentation, closure, callTarget, false, false);
    }
    // TODO remove once callsites add docs
    public LispFunction(Closure closure, CallTarget callTarget) {
        this(null, closure, callTarget, false, false);
    }

    @ExportMessage
    boolean hasSourceLocation() {
        return callTarget instanceof RootCallTarget;
    }

    @ExportMessage
    SourceSection getSourceLocation() {
        if (callTarget instanceof RootCallTarget r) {
            return r.getRootNode().getSourceSection();
        }
        return null;
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
            @Cached("function.callTarget()") CallTarget prevCallTarget,
            @Cached("createDirectCallNode(function)") DirectCallNode callNode) {
            var realArgs = new Object[args.length + 1];
            realArgs[0] = function.closure();
            System.arraycopy(args, 0, realArgs, 1, args.length);
            return callNode.call(realArgs);
        }

        static DirectCallNode createDirectCallNode(LispFunction fun) {
            var node = DirectCallNode.create(fun.callTarget());
            // generic calls have good chance that they're only used with same
            // type on call site (probably)
            if (fun.isGeneric) {
                node.cloneCallTarget();
            }
            if (fun.inline) {
                node.forceInlining();
            }
            return node;
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
