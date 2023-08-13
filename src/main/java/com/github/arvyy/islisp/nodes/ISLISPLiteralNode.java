package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.frame.VirtualFrame;

public class ISLISPLiteralNode extends ISLISPExpressionNode {

    private final Value value;

    public ISLISPLiteralNode(Value value) {
        this.value = value;
    }

    @Override
    public Value executeGeneric(VirtualFrame frame) {
        return value;
    }
}
