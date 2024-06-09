package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `while` syntax for simple iteration.
 */
public class ISLISPWhileNode extends ISLISPExpressionNode {

    @Child
    ISLISPExpressionNode test;

    @Children
    ISLISPExpressionNode[] body;

    /**
     * Create while node.
     *
     * @param test test expression.
     * @param body body expression to repeat while test yields truthy value.
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPWhileNode(ISLISPExpressionNode test, ISLISPExpressionNode[] body, SourceSection sourceSection) {
        super(sourceSection);
        this.test = test;
        this.body = body;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        var nil = ISLISPContext.get(this).getNil();
        while (nil != test.executeGeneric(frame)) {
            executeBody(frame);
        }
        return nil;
    }

    @ExplodeLoop
    void executeBody(VirtualFrame frame) {
        for (var i = 0; i < body.length; i++) {
            body[i].executeGeneric(frame);
        }
    }

}
