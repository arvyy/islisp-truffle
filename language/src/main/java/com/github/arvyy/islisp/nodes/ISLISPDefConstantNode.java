package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `defconstant` syntax for creating global constants.
 */
public class ISLISPDefConstantNode extends ISLISPExpressionNode {

    private final Symbol name;
    @Child
    private ISLISPExpressionNode expression;

    /**
     * Create defconstant node.
     *
     * @param name costant name
     * @param expression constant value
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPDefConstantNode(Symbol name, ISLISPExpressionNode expression, SourceSection sourceSection) {
        super(true, sourceSection);
        this.name = name;
        this.expression = expression;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        ISLISPContext.get(this).registerGlobalVar(name.identityReference(), expression.executeGeneric(frame), true);
        return name;
    }
}
