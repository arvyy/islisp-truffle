package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPThrowException;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `catch` syntax. Catches raise calls for matching tag object.
 */
public class ISLISPCatchNode extends ISLISPExpressionNode {

    @Child
    ISLISPExpressionNode tagForm;

    @Children
    ISLISPExpressionNode[] body;

    @Child
    DirectCallNode eq;

    /**
     * Create catch node.
     *
     * @param tagForm expression for tag value
     * @param body body expressions
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPCatchNode(ISLISPExpressionNode tagForm, ISLISPExpressionNode[] body, SourceSection sourceSection) {
        super(sourceSection);
        this.tagForm = tagForm;
        this.body = body;
    }

    @Override
    @ExplodeLoop
    public Object executeGeneric(VirtualFrame frame) {
        var ctx = ISLISPContext.get(this);
        if (eq == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            eq = this.insert(DirectCallNode.create(
                ctx.lookupFunction("ROOT", ctx.namedSymbol("eq"))
                    .callTarget()));
        }
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
            if (eq.call(null, tagObject, e.getCatchTag()) != ctx.getNil()) {
                return e.getResult();
            } else {
                throw e;
            }
        }
    }

}
