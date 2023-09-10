package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.ISLISPError;
import com.github.arvyy.islisp.runtime.ValueReference;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPSetDynamicNode extends ISLISPExpressionNode {

    private final Symbol symbol;

    @CompilerDirectives.CompilationFinal
    private ValueReference valueReference;

    @Child
    ISLISPExpressionNode expression;

    public ISLISPSetDynamicNode(Symbol symbol, ISLISPExpressionNode expression, SourceSection sourceSection) {
        super(sourceSection);
        this.symbol = symbol;
        this.expression = expression;
    }

    @Override
    public Value executeGeneric(VirtualFrame frame) {
        if (valueReference == null) {
            valueReference = ISLISPContext.get(this).lookupDynamicVar(symbol.identityReference());
            if (valueReference == null) {
                throw new ISLISPError("Undefined dynamic variable", this);
            }
        }
        if (valueReference.getValue() == null) {
            throw new ISLISPError("Undefined dynamic variable", this);
        }
        var value = expression.executeGeneric(frame);
        valueReference.setValue(value);
        return value;
    }
}
