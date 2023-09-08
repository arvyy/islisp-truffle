package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.ISLISPError;
import com.github.arvyy.islisp.runtime.DynamicVar;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPSetDynamicNode extends ISLISPExpressionNode {

    private final Symbol symbol;

    @CompilerDirectives.CompilationFinal
    private DynamicVar dynamicVar;

    @Child
    ISLISPExpressionNode expression;

    public ISLISPSetDynamicNode(Symbol symbol, ISLISPExpressionNode expression, SourceSection sourceSection) {
        super(sourceSection);
        this.symbol = symbol;
        this.expression = expression;
    }

    @Override
    public Value executeGeneric(VirtualFrame frame) {
        if (dynamicVar == null) {
            dynamicVar = ISLISPContext.get(this).lookupDynamicVar(symbol.identityReference());
            if (dynamicVar == null) {
                throw new ISLISPError("Undefined dynamic variable", this);
            }
        }
        if (dynamicVar.getValue() == null) {
            throw new ISLISPError("Undefined dynamic variable", this);
        }
        var value = expression.executeGeneric(frame);
        dynamicVar.setValue(value);
        return value;
    }
}
