package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.Tag;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPDefunNode extends ISLISPExpressionNode {

    final Symbol name;

    @Child
    ISLISPUserDefinedFunctionNode functionNode;

    public ISLISPDefunNode(Symbol name, FrameDescriptor frameDescriptor, int[] namedArgumentSlots, int restArgsSlot, ISLISPExpressionNode body, SourceSection sourceSection) {
        super(true, sourceSection);
        this.name = name;
        var ctx = ISLISPContext.get(this);
        functionNode = new ISLISPUserDefinedFunctionNode(ctx.getLanguage(), frameDescriptor, body, namedArgumentSlots, restArgsSlot, -1, -1, sourceSection);
    }

    @Override
    public Value executeGeneric(VirtualFrame frame) {
        var ctx = ISLISPContext.get(this);
        ctx.registerFunction(name.identityReference(), new LispFunction(functionNode.getCallTarget()));
        return name;
    }

    @Override
    public boolean hasTag(Class<? extends Tag> tag) {
        if (tag == StandardTags.StatementTag.class)
            return true;
        return super.hasTag(tag);
    }
}
