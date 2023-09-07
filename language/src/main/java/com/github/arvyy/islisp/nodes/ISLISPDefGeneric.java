package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.GenericFunctionDescriptor;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPDefGeneric extends ISLISPExpressionNode {

    private final Symbol name;
    private final int requiredArgsCount;
    private final boolean hasRest;

    @Child
    private ISLISPDefGenericExecutionNode executionNode;


    public ISLISPDefGeneric(Symbol name, int requiredArgsCount, boolean hasRest, SourceSection sourceSection) {
        super(sourceSection);
        this.name = name;
        this.requiredArgsCount = requiredArgsCount;
        this.hasRest = hasRest;
        var ctx = ISLISPContext.get(this);
        executionNode = ISLISPDefGenericExecutionNodeGen.create(name, ctx.getLanguage(), sourceSection);
    }

    @Override
    public Value executeGeneric(VirtualFrame frame) {
        var ctx = ISLISPContext.get(this);
        var descriptor = new GenericFunctionDescriptor(requiredArgsCount, hasRest);
        var function = new LispFunction(null, executionNode.getCallTarget(), true);
        ctx.registerGenericFunction(name.identityReference(), function, descriptor);
        return name;
    }
}
