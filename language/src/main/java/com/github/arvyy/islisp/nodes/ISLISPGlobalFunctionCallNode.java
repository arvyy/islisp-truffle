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

public class ISLISPGlobalFunctionCallNode extends ISLISPExpressionNode {

    private final Symbol name;
    private final boolean setf;

    @CompilerDirectives.CompilationFinal
    private LispFunction function;

    @Children
    private ISLISPExpressionNode[] arguments;

    @Child
    private ISLISPFunctionDispatchNode dispatchNode;

    public ISLISPGlobalFunctionCallNode(
        Symbol name,
        boolean setf,
        ISLISPExpressionNode[] arguments,
        SourceSection sourceSection
    ) {
        super(sourceSection);
        this.name = name;
        this.setf = setf;
        this.arguments = arguments;
        this.dispatchNode = ISLISPFunctionDispatchNodeGen.create();
    }


    @Override
    @ExplodeLoop
    public Object executeGeneric(VirtualFrame frame) {
        if (function == null) {
            function = ISLISPContext.get(this).lookupFunction(name.identityReference(), setf);
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
