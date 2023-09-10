package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.ISLISPError;
import com.github.arvyy.islisp.ISLISPTruffleLanguage;
import com.github.arvyy.islisp.builtins.BuiltinClassOf;
import com.github.arvyy.islisp.builtins.BuiltinClassOfNodeGen;
import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.GenerateWrapper;
import com.oracle.truffle.api.instrumentation.InstrumentableNode;
import com.oracle.truffle.api.instrumentation.ProbeNode;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;

import java.sql.Array;
import java.util.ArrayList;

@GenerateWrapper
public abstract class ISLISPDefGenericExecutionNode extends RootNode implements InstrumentableNode {

    private final Symbol name;
    private final SourceSection sourceSection;

    @CompilerDirectives.CompilationFinal
    GenericFunctionDescriptor genericFunctionDescriptor;

    @Child
    BuiltinClassOf classOf;

    @Child
    ISLISPGenericFunctionDispatchNode dispatchNode;

    DirectCallNode classOfCall;

    public ISLISPDefGenericExecutionNode(Symbol name, TruffleLanguage<?> language, SourceSection sourceSection) {
        super(language);
        this.name = name;
        this.sourceSection = sourceSection;
        this.classOf = BuiltinClassOfNodeGen.create(language);
        classOfCall = DirectCallNode.create(classOf.getCallTarget());
        dispatchNode = ISLISPGenericFunctionDispatchNodeGen.create();
    }

    protected ISLISPDefGenericExecutionNode(ISLISPDefGenericExecutionNode other) {
        super(other.getLanguage(ISLISPTruffleLanguage.class));
        name = other.name;
        sourceSection = other.sourceSection;
        classOf = other.classOf;
        classOfCall = other.classOfCall;
        dispatchNode = other.dispatchNode;
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        if (genericFunctionDescriptor == null) {
            genericFunctionDescriptor = ISLISPContext.get(this).lookupGenericFunctionDispatchTree(name.identityReference());
        }
        if (frame.getArguments().length - 1 < genericFunctionDescriptor.getRequiredArgCount()) {
            throw new ISLISPError("Not enough args", this);
        }
        var argumentTypes = new LispClass[genericFunctionDescriptor.getRequiredArgCount()];
        for (int i = 1; i <= genericFunctionDescriptor.getRequiredArgCount(); i++) {
            var value = (Value) frame.getArguments()[i];
            argumentTypes[i - 1] = (LispClass) classOfCall.call(null, value);
        }
        var arguments = new Value[frame.getArguments().length - 1];
        System.arraycopy(frame.getArguments(), 1, arguments, 0, arguments.length);
        return executeGeneric(frame, argumentTypes, arguments);
    }

    abstract Object executeGeneric(VirtualFrame frame, LispClass[] classes, Value[] arguments);

    @Specialization(
            guards = "classesEqual(classes, lastClasses)",
            assumptions = "genericFunctionDescriptor.getAssumption()")
    Object executeSpecial(
            VirtualFrame frame,
            LispClass[] classes,
            Value[] arguments,
            @Cached("classes") LispClass[] lastClasses,
            @Cached("getApplicableMethods(classes)") GenericMethodApplicableMethods applicableMethods
    ) {
        return dispatchNode.executeDispatch(applicableMethods, arguments);
    }

    GenericMethodApplicableMethods getApplicableMethods(LispClass[] classes) {
        return genericFunctionDescriptor.getApplicableMethods(classes);
    }

    boolean classesEqual(LispClass[] classes1, LispClass[] classes2) {
        for (int i = 0; i < classes1.length; i++) {
            if (classes1[i] != classes2[i])
                return false;
        }
        return true;
    }

    @Override
    public boolean isInstrumentable() {
        return true;
    }

    @Override
    public WrapperNode createWrapper(ProbeNode probe) {
        return new ISLISPDefGenericExecutionNodeWrapper(this, this, probe);
    }

    @Override
    public SourceSection getSourceSection() {
        return sourceSection;
    }
}
