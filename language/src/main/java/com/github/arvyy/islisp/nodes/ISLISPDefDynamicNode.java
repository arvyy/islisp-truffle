package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.ValueReference;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPDefDynamicNode extends ISLISPExpressionNode {

    private final Symbol name;

    @Child
    private ISLISPExpressionNode initializer;

    public ISLISPDefDynamicNode(Symbol name, ISLISPExpressionNode initializer, SourceSection sourceSection) {
        super(sourceSection);
        this.name = name;
        this.initializer = initializer;
    }

    @Override
    public Value executeGeneric(VirtualFrame frame) {
        var ctx = ISLISPContext.get(this);
        var dynamicVar = new ValueReference();
        dynamicVar.setValue(initializer.executeGeneric(frame));
        ctx.registerDynamicVar(name.identityReference(), dynamicVar);
        return name;
    }
}
