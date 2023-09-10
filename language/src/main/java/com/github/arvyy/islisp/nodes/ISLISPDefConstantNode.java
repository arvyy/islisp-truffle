package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPDefConstantNode extends ISLISPExpressionNode {

    private final Symbol name;
    @Child
    private ISLISPExpressionNode expression;

    public ISLISPDefConstantNode(Symbol name, ISLISPExpressionNode expression, SourceSection sourceSection) {
        super(sourceSection);
        this.name = name;
        this.expression = expression;
    }

    @Override
    public Value executeGeneric(VirtualFrame frame) {
        ISLISPContext.get(this).registerGlobalVar(name.identityReference(), expression.executeGeneric(frame), true);
        return name;
    }
}
