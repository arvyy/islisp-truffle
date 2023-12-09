package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `with-standard-output` syntax.
 */
public class ISLISPWithStandardInputNode extends ISLISPExpressionNode {

    @Child
    private ISLISPExpressionNode inputExpression;

    @Children
    private ISLISPExpressionNode[] body;

    /**
     * Create `with-standard-input` node.
     *
     * @param inputExpression expression that must yield new input stream value
     * @param body set of expressions for the duration of which the custom input will be installed
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPWithStandardInputNode(
        ISLISPExpressionNode inputExpression,
        ISLISPExpressionNode[] body,
        SourceSection sourceSection
    ) {
        super(sourceSection);
        this.inputExpression = inputExpression;
        this.body = body;
    }

    @Override
    @ExplodeLoop
    public Object executeGeneric(VirtualFrame frame) {
        var ctx = ISLISPContext.get(this);
        var ref = ctx.currentInputStreamReference();
        var oldValue = ref.getValue();
        var newValue = inputExpression.executeGeneric(frame);
        //TODO signal if newValue not a stream
        ref.setValue(newValue);
        try {
            if (body.length == 0) {
                return ctx.getNil();
            }
            for (var i = 0; i < body.length - 1; i++) {
                body[i].executeGeneric(frame);
            }
            return body[body.length - 1].executeGeneric(frame);
        } finally {
            ref.setValue(oldValue);
        }
    }
}
