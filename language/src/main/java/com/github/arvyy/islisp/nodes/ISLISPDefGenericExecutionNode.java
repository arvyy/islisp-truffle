package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.ISLISPTruffleLanguage;
import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.functions.ISLISPClassOf;
import com.github.arvyy.islisp.functions.ISLISPClassOfNodeGen;
import com.github.arvyy.islisp.runtime.GenericFunctionDescriptor;
import com.github.arvyy.islisp.runtime.GenericMethodApplicableMethods;
import com.github.arvyy.islisp.runtime.LispClass;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Helper node that acts as an entry point when invoking a generic function. Collects applicable
 * methods before initiating dispatch chain.
 */
public abstract class ISLISPDefGenericExecutionNode extends RootNode {

    private final String module;
    private final Symbol name;
    private final boolean setf;
    private final SourceSection sourceSection;

    @CompilerDirectives.CompilationFinal
    GenericFunctionDescriptor genericFunctionDescriptor;

    @Child
    ISLISPClassOf classOf;

    @Child
    ISLISPGenericFunctionDispatchNode dispatchNode;

    DirectCallNode classOfCall;

    /**
     * Create defgeneric execution node.
     *
     * @param name generic function name
     * @param setf whether function is of setf form
     * @param language language reference
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPDefGenericExecutionNode(
        String module,
        Symbol name,
        boolean setf,
        TruffleLanguage<?> language,
        SourceSection sourceSection
    ) {
        super(language);
        this.module = module;
        this.name = name;
        this.setf = setf;
        this.sourceSection = sourceSection;
        this.classOf = ISLISPClassOfNodeGen.create(language);
        classOfCall = DirectCallNode.create(classOf.getCallTarget());
        dispatchNode = ISLISPGenericFunctionDispatchNodeGen.create();
    }

    protected ISLISPDefGenericExecutionNode(ISLISPDefGenericExecutionNode other) {
        super(other.getLanguage(ISLISPTruffleLanguage.class));
        module = other.module;
        name = other.name;
        setf = other.setf;
        sourceSection = other.sourceSection;
        classOf = other.classOf;
        classOfCall = other.classOfCall;
        dispatchNode = other.dispatchNode;
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        if (genericFunctionDescriptor == null) {
            genericFunctionDescriptor = ISLISPContext.get(this)
                    .lookupGenericFunctionDispatchTree(module, name.identityReference(), setf);
        }
        if (frame.getArguments().length - 1 < genericFunctionDescriptor.getRequiredArgCount()) {
            throw new ISLISPError("Not enough args", this);
        }
        var argumentTypes = new LispClass[genericFunctionDescriptor.getRequiredArgCount()];
        for (int i = 1; i <= genericFunctionDescriptor.getRequiredArgCount(); i++) {
            var value = frame.getArguments()[i];
            argumentTypes[i - 1] = (LispClass) classOfCall.call(null, value);
        }
        var arguments = new Object[frame.getArguments().length - 1];
        System.arraycopy(frame.getArguments(), 1, arguments, 0, arguments.length);
        return executeGeneric(argumentTypes, arguments);
    }

    abstract Object executeGeneric(LispClass[] classes, Object[] arguments);

    @Specialization(
            guards = "classesEqual(classes, lastClasses)",
            assumptions = "genericFunctionDescriptor.getAssumption()")
    Object doCached(
            LispClass[] classes,
            Object[] arguments,
            @Cached(value = "classes", dimensions = 0) LispClass[] lastClasses,
            @Cached("getApplicableMethods(classes)") GenericMethodApplicableMethods applicableMethods
    ) {
        return dispatchNode.executeDispatch(applicableMethods, arguments);
    }

    @Specialization
    Object doUncached(
        LispClass[] classes,
        Object[] arguments
    ) {
        return dispatchNode.executeDispatch(getApplicableMethods(classes), arguments);
    }

    GenericMethodApplicableMethods getApplicableMethods(LispClass[] classes) {
        return genericFunctionDescriptor.getApplicableMethods(classes);
    }

    boolean classesEqual(LispClass[] classes1, LispClass[] classes2) {
        for (int i = 0; i < classes1.length; i++) {
            if (classes1[i] != classes2[i]) {
                return false;
            }
        }
        return true;
    }

    @Override
    public SourceSection getSourceSection() {
        return sourceSection;
    }

    @Override
    public boolean isCloningAllowed() {
        return true;
    }
}
