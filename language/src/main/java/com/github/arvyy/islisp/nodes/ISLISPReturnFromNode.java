package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.exceptions.ISLISPReturnFromException;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPReturnFromNode extends ISLISPExpressionNode {

    private final int blockId;

    @Child
    private ISLISPExpressionNode resultForm;

    public ISLISPReturnFromNode(int blockId, ISLISPExpressionNode resultForm, SourceSection sourceSection) {
        super(sourceSection);
        this.blockId = blockId;
        this.resultForm = resultForm;
    }

    @Override
    public Value executeGeneric(VirtualFrame frame) {
        throw new ISLISPReturnFromException(blockId, resultForm.executeGeneric(frame));
    }
}