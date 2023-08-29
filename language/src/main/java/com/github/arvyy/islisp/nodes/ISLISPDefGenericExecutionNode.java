package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.ISLISPError;
import com.github.arvyy.islisp.ISLISPTruffleLanguage;
import com.github.arvyy.islisp.builtins.BuiltinClassOf;
import com.github.arvyy.islisp.builtins.BuiltinClassOfNodeGen;
import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.GenerateWrapper;
import com.oracle.truffle.api.instrumentation.InstrumentableNode;
import com.oracle.truffle.api.instrumentation.ProbeNode;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;

import java.util.ArrayList;

@GenerateWrapper
public class ISLISPDefGenericExecutionNode extends RootNode implements InstrumentableNode {

    private final Symbol name;
    private final SourceSection sourceSection;

    @CompilerDirectives.CompilationFinal
    private GenericFunctionDescriptor genericFunctionDescriptor;

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
    public Object execute(VirtualFrame frame) {
        if (genericFunctionDescriptor == null) {
            genericFunctionDescriptor = ISLISPContext.get(this).lookupGenericFunctionDispatchTree(name.identityReference());
        }
        if (frame.getArguments().length - 1 < genericFunctionDescriptor.getRequiredArgCount()) {
            //TODO not enough args
            throw new ISLISPError("Not enough args", this);
        }
        var argumentTypes = new ArrayList<LispClass>();
        for (int i = 1; i <= genericFunctionDescriptor.getRequiredArgCount(); i++) {
            var value = (Value) frame.getArguments()[i];
            argumentTypes.add((LispClass) classOfCall.call(null, value));
        }
        //TODO use assumptions
        var dispatchTree = genericFunctionDescriptor.getDispatchTree();
        var methods = dispatchTree.getApplicableMethods(argumentTypes);
        return dispatchNode.executeDispatch(methods, frame.getArguments());
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
