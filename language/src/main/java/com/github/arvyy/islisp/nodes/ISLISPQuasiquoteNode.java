package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.parser.QuasiquoteTree;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements quasiquote syntax, partial quoting.
 */
public class ISLISPQuasiquoteNode extends ISLISPExpressionNode {

    private final QuasiquoteTree tree;

    @Children
    private final ISLISPExpressionNode[] expressionNodes;

    /**
     * Create quasiquote node.
     *
     * @param sourceSection corresponding source section to this node
     * @param tree parsed partially quoted tree
     * @param expressionNodes expressions for the unquoted parts of the tree
     */
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
    public Object executeGeneric(VirtualFrame frame) {
        Object[] values = new Object[expressionNodes.length];
        for (var i = 0; i < values.length; i++) {
            values[i] = expressionNodes[i].executeGeneric(frame);
        }
        return QuasiquoteTree.evalQuasiquoteTree(tree, values, this);
    }
}
