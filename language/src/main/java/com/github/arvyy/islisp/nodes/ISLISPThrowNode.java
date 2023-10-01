package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.exceptions.ISLISPThrowException;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPThrowNode extends ISLISPExpressionNode {

    @Child
    ISLISPExpressionNode tagForm;

    @Child
    ISLISPExpressionNode resultForm;

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
