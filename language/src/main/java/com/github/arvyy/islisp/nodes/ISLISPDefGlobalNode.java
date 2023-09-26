package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPDefGlobalNode extends ISLISPExpressionNode {

    private final Symbol name;
    @Child
    private final ISLISPExpressionNode expression;

    public ISLISPDefGlobalNode(Symbol name, ISLISPExpressionNode expression, SourceSection sourceSection) {
        super(true, sourceSection);
        this.name = name;
        this.expression = expression;
    }

    @Override
    public Value executeGeneric(VirtualFrame frame) {
        ISLISPContext.get(this).registerGlobalVar(name.identityReference(), expression.executeGeneric(frame), false);
        return name;
    }
}
