package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.GenericFunctionDescriptor;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `defgeneric` syntax for creating new generic methods.
 */
public class ISLISPDefGenericNode extends ISLISPExpressionNode {

    private final String module;
    private final Symbol name;
    private final boolean setf;
    private final int requiredArgsCount;
    private final boolean hasRest;

    @Child
    private ISLISPDefGenericExecutionNode executionNode;


    /**
     * Create defgeneric node.
     *
     * @param name generic function name
     * @param setf if function has setf form or not
     * @param requiredArgsCount required arguments count
     * @param hasRest has rest argument
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPDefGenericNode(
        String module,
        Symbol name,
        boolean setf,
        int requiredArgsCount,
        boolean hasRest,
        SourceSection sourceSection
    ) {
        super(true, sourceSection);
        this.module = module;
        this.name = name;
        this.requiredArgsCount = requiredArgsCount;
        this.hasRest = hasRest;
        this.setf = setf;
        var ctx = ISLISPContext.get(this);
        executionNode = ISLISPDefGenericExecutionNodeGen.create(module, name, setf, ctx.getLanguage(), sourceSection);
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        var ctx = ISLISPContext.get(this);
        var descriptor = new GenericFunctionDescriptor(requiredArgsCount, hasRest);
        var function = new LispFunction(null, executionNode.getCallTarget(), true);
        ctx.registerGenericFunction(module, name.identityReference(), setf, function, descriptor);
        return name;
    }
}
