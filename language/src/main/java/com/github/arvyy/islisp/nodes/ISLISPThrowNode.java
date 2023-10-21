package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.exceptions.ISLISPThrowException;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `throw` syntax for dynamic unwind.
 */
public class ISLISPThrowNode extends ISLISPExpressionNode {

    @Child
    ISLISPExpressionNode tagForm;

    @Child
    ISLISPExpressionNode resultForm;

    /**
     * Create throw node.
     *
     * @param tagForm expression corresponding to tag object
     * @param resultForm expression for value to be yielded by catch
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPThrowNode(ISLISPExpressionNode tagForm, ISLISPExpressionNode resultForm, SourceSection sourceSection) {
        super(sourceSection);
        this.tagForm = tagForm;
        this.resultForm = resultForm;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        throw new ISLISPThrowException(tagForm.executeGeneric(frame), resultForm.executeGeneric(frame));
    }
}
