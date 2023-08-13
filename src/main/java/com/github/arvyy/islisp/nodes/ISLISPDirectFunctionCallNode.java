package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;

public class ISLISPDirectFunctionCallNode extends ISLISPExpressionNode {

    private String name;

    @CompilerDirectives.CompilationFinal
    private LispFunction function;

    @Children
    private ISLISPExpressionNode[] arguments;

    @Child
    private ISLISPFunctionDispatchNode dispatchNode;

    public ISLISPDirectFunctionCallNode(String name, ISLISPExpressionNode[] arguments) {
        this.name = name;
        this.arguments = arguments;
        this.dispatchNode = ISLISPFunctionDispatchNodeGen.create();
    }


    @Override
    @ExplodeLoop
    public Value executeGeneric(VirtualFrame frame) {
        if (function == null) {
            function = ISLISPContext.get(this).lookupFunction(name);
        }
        var argValues = new Value[arguments.length];
        for (int i = 0; i < argValues.length; i++) {
            argValues[i] = arguments[i].executeGeneric(frame);
        }
        return dispatchNode.executeDispatch(function, argValues);
    }
}
