package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.Tag;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements function call of a global function.
 */
public class ISLISPGlobalFunctionCallNode extends ISLISPExpressionNode {

    private final String module;
    private final Symbol name;
    private final boolean setf;

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    @CompilerDirectives.CompilationFinal
    private LispFunction function;

    @Children
    private ISLISPExpressionNode[] arguments;

    @Child
    private ISLISPFunctionDispatchNode dispatchNode;

    /**
     * Create global function call node.
     *
     * @param module module name whose source's this node is part of
     * @param name function's name
     * @param setf is this regular form or through setf
     * @param arguments expressions to be evaluated and used as function arguments
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPGlobalFunctionCallNode(
        String module,
        Symbol name,
        boolean setf,
        ISLISPExpressionNode[] arguments,
        SourceSection sourceSection
    ) {
        super(sourceSection);
        this.module = module;
        this.name = name;
        this.setf = setf;
        this.arguments = arguments;
        this.dispatchNode = ISLISPFunctionDispatchNodeGen.create();
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }


    @Override
    @ExplodeLoop
    public Object executeGeneric(VirtualFrame frame) {
        if (function == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            function = ISLISPContext.get(this).lookupFunction(
                module, name, setf);
            if (function == null) {
                return errorSignalerNode.signalUndefinedFunction(name);
            }
        }
        var argValues = new Object[arguments.length];
        for (int i = 0; i < argValues.length; i++) {
            argValues[i] = arguments[i].executeGeneric(frame);
        }
        return dispatchNode.executeDispatch(function, argValues);
    }

    @Override
    public boolean hasTag(Class<? extends Tag> tag) {
        if (tag == StandardTags.CallTag.class) {
            return true;
        }
        return super.hasTag(tag);
    }
}
