package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.runtime.ValueReference;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPDynamicLookupNode extends ISLISPExpressionNode {

    private final Symbol symbol;

    @CompilerDirectives.CompilationFinal
    private ValueReference valueReference;

    public ISLISPDynamicLookupNode(Symbol symbol, SourceSection sourceSection) {
        super(sourceSection);
        this.symbol = symbol;
    }

    @Override
    public Value executeGeneric(VirtualFrame frame) {
        if (valueReference == null) {
            valueReference = ISLISPContext.get(this).lookupDynamicVar(symbol.identityReference());
            if (valueReference == null) {
                throw new ISLISPError("Undefined dynamic variable", this);
            }
        }
        var value = valueReference.getValue();
        if (value == null) {
            throw new ISLISPError("Undefined dynamic variable", this);
        }
        return value;
    }
}
