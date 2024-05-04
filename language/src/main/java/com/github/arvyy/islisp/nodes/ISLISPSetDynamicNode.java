package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.ValueReference;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `set-dynamic` syntax for updating value of a dynamically scoped variable.
 */
public class ISLISPSetDynamicNode extends ISLISPExpressionNode {

    private final String module;
    private final Symbol symbol;

    @CompilerDirectives.CompilationFinal
    private ValueReference valueReference;

    @Child
    ISLISPExpressionNode expression;

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    /**
     * Create set-dynamic node.
     *
     * @param module module name whose source's this node is part of
     * @param symbol dynamic variable name
     * @param expression value expression
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPSetDynamicNode(
        String module,
        Symbol symbol,
        ISLISPExpressionNode expression,
        SourceSection sourceSection
    ) {
        super(sourceSection);
        this.module = module;
        this.symbol = symbol;
        this.expression = expression;
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        if (valueReference == null) {
            valueReference = ISLISPContext.get(this).lookupDynamicVar(module, symbol.identityReference());
            if (valueReference == null) {
                return errorSignalerNode.signalUnboundVariable(symbol);
            }
        }
        if (valueReference.getValue() == null) {
            return errorSignalerNode.signalUnboundVariable(symbol);
        }
        var value = expression.executeGeneric(frame);
        valueReference.setValue(value);
        return value;
    }
}
