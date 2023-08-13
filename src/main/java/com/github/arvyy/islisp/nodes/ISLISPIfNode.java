package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.profiles.ConditionProfile;

public class ISLISPIfNode extends ISLISPExpressionNode {

    private final ConditionProfile conditionProfile;

    @Child
    private ISLISPExpressionNode testExpr;

    @Child
    private ISLISPExpressionNode truthyExpr;

    @Child
    private ISLISPExpressionNode falsyExpr;

    public ISLISPIfNode(ISLISPExpressionNode testExpr, ISLISPExpressionNode truthyExpr, ISLISPExpressionNode falsyExpr) {
        conditionProfile = ConditionProfile.createCountingProfile();
        this.testExpr = testExpr;
        this.truthyExpr = truthyExpr;
        this.falsyExpr = falsyExpr;
    }

    @Override
    public Value executeGeneric(VirtualFrame frame) {
        var test = testExpr.executeGeneric(frame);
        if (conditionProfile.profile(test == Symbol.NIL)) {
            return falsyExpr.executeGeneric(frame);
        } else {
            return truthyExpr.executeGeneric(frame);
        }
    }

}