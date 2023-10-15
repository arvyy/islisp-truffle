package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPWhileNode extends ISLISPExpressionNode {

    @Child
    ISLISPExpressionNode test;

    @Children
    ISLISPExpressionNode[] body;

    public ISLISPWhileNode(ISLISPExpressionNode test, ISLISPExpressionNode[] body, SourceSection sourceSection) {
        super(sourceSection);
        this.test = test;
        this.body = body;
    }

    @Override
    @ExplodeLoop
    public Object executeGeneric(VirtualFrame frame) {
        var nil = ISLISPContext.get(this).getNil();
        while (nil != test.executeGeneric(frame)) {
            for (var e: body) {
                e.executeGeneric(frame);
            }
        }
        return nil;
    }
}
