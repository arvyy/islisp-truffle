package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.Tag;

public class ISLISPDefunNode extends ISLISPExpressionNode {

    final Symbol name;

    @Child
    ISLISPRootNode functionNode;

    public ISLISPDefunNode(Symbol name, ISLISPRootNode functionNode) {
        super(true, functionNode.getSourceSection());
        this.name = name;
        this.functionNode = functionNode;
    }

    @Override
    public Value executeGeneric(VirtualFrame frame) {
        var ctx = ISLISPContext.get(this);
        ctx.registerFunction(name.identityReference(), new LispFunction(functionNode.getCallTarget()));
        return name;
    }

    @Override
    public boolean hasTag(Class<? extends Tag> tag) {
        if (tag == StandardTags.StatementTag.class) {
            return true;
        }
        return super.hasTag(tag);
    }
}
