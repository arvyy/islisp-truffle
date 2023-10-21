package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.profiles.ConditionProfile;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `if` syntax for logical branching.
 */
public class ISLISPIfNode extends ISLISPExpressionNode {

    private final ConditionProfile conditionProfile;

    @Child
    private ISLISPExpressionNode testExpr;

    @Child
    private ISLISPExpressionNode truthyExpr;

    @Child
    private ISLISPExpressionNode falsyExpr;

    /**
     * Create if node.
     *
     * @param testExpr expression used to determine the execution branch
     * @param truthyExpr expression to evaluate if testExpr yielded truthy value
     * @param falsyExpr expression to evaluate if testExpr yielded nil
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPIfNode(
            ISLISPExpressionNode testExpr,
            ISLISPExpressionNode truthyExpr,
            ISLISPExpressionNode falsyExpr,
            SourceSection sourceSection
    ) {
        super(sourceSection);
        conditionProfile = ConditionProfile.createCountingProfile();
        this.testExpr = testExpr;
        this.truthyExpr = truthyExpr;
        this.falsyExpr = falsyExpr;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        var test = testExpr.executeGeneric(frame);
        if (conditionProfile.profile(test == ISLISPContext.get(this).getNil())) {
            return falsyExpr.executeGeneric(frame);
        } else {
            return truthyExpr.executeGeneric(frame);
        }
    }

}
