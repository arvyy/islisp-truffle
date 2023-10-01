package com.github.arvyy.islisp.nodes;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPLiteralNode extends ISLISPExpressionNode {

    private final Object value;

    public ISLISPLiteralNode(Object value, SourceSection sourceSection) {
        super(sourceSection);
        this.value = value;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return value;
    }
}
