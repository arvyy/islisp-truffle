package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPFunctionRefNode extends ISLISPExpressionNode {

    private final Symbol name;

    @CompilerDirectives.CompilationFinal
    private LispFunction function;

    public ISLISPFunctionRefNode(Symbol name, SourceSection sourceSection) {
        super(sourceSection);
        this.name = name;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        if (function == null) {
            function = ISLISPContext.get(this).lookupFunction(name.identityReference());
        }
        return function;
    }
}
