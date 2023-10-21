package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.exceptions.ISLISPReturnFromException;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `return-from` syntax. At parse time this form is given a blockId that matches
 * corresponding's `block` form's blockId. During execution a control flow exception is thrown
 * that unwindws the stack until the necessary block.
 */
public class ISLISPReturnFromNode extends ISLISPExpressionNode {

    private final int blockId;

    @Child
    private ISLISPExpressionNode resultForm;

    /**
     * Create return-from node.
     *
     * @param blockId unique id of the block to return to.
     * @param resultForm expression to evaluate to yield the result value.
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPReturnFromNode(int blockId, ISLISPExpressionNode resultForm, SourceSection sourceSection) {
        super(sourceSection);
        this.blockId = blockId;
        this.resultForm = resultForm;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        throw new ISLISPReturnFromException(blockId, resultForm.executeGeneric(frame));
    }
}
