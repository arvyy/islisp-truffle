package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.ValueReference;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPGlobalIdentifierNode extends ISLISPExpressionNode {

    @CompilerDirectives.CompilationFinal
    private ValueReference valueReference;

    private final Symbol name;

    public ISLISPGlobalIdentifierNode(Symbol name, SourceSection sourceSection) {
        super(sourceSection);
        this.name = name;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        if (valueReference == null) {
            valueReference = ISLISPContext.get(this).lookupGlobalVar(name.identityReference());
        }
        return valueReference.getValue();
    }
}
