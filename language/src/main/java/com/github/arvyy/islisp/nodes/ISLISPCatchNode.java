package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.builtins.BuiltinEq;
import com.github.arvyy.islisp.exceptions.ISLISPThrowException;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPCatchNode extends ISLISPExpressionNode {

    @Child
    ISLISPExpressionNode tagForm;

    @Children
    ISLISPExpressionNode[] body;

    public ISLISPCatchNode(ISLISPExpressionNode tagForm, ISLISPExpressionNode[] body, SourceSection sourceSection) {
        super(sourceSection);
        this.tagForm = tagForm;
        this.body = body;
    }

    @Override
    @ExplodeLoop
    public Object executeGeneric(VirtualFrame frame) {
        var tagObject = tagForm.executeGeneric(frame);
        try {
            if (body.length == 0) {
                return ISLISPContext.get(this).getNil();
            }
            for (int i = 0; i < body.length - 1; i++) {
                body[i].executeGeneric(frame);
            }
            return body[body.length - 1].executeGeneric(frame);
        } catch (ISLISPThrowException e) {
            if (BuiltinEq.isEq(tagObject, e.getCatchTag())) {
                return e.getResult();
            } else {
                throw e;
            }
        }
    }

}