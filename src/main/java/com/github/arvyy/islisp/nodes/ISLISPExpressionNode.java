package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.dsl.TypeSystem;
import com.oracle.truffle.api.dsl.TypeSystemReference;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;

@TypeSystemReference(ISLISPTypes.class)
public abstract class ISLISPExpressionNode extends Node {

    private final boolean isDefinitionNode;

    public ISLISPExpressionNode() {
        isDefinitionNode = false;
    }

    public ISLISPExpressionNode(boolean isDefinitionNode) {
        this.isDefinitionNode = isDefinitionNode;
    }

    public abstract Value executeGeneric(VirtualFrame frame);

    public boolean isDefinitionNode() {
        return isDefinitionNode;
    }

}
