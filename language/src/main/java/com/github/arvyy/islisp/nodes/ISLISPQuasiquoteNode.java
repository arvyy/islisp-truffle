package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.parser.QuasiquoteTree;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPQuasiquoteNode extends ISLISPExpressionNode {

    private final QuasiquoteTree tree;

    @Children
    private final ISLISPExpressionNode[] expressionNodes;

    public ISLISPQuasiquoteNode(
            SourceSection sourceSection,
            QuasiquoteTree tree,
            ISLISPExpressionNode[] expressionNodes
    ) {
        super(sourceSection);
        this.tree = tree;
        this.expressionNodes = expressionNodes;
    }

    @Override
    @ExplodeLoop
    public Value executeGeneric(VirtualFrame frame) {
        Value[] values = new Value[expressionNodes.length];
        for (var i = 0; i < values.length; i++) {
            values[i] = expressionNodes[i].executeGeneric(frame);
        }
        return QuasiquoteTree.evalQuasiquoteTree(tree, values, this);
    }
}
