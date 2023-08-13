package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;

public class ISLISPFunctionRef extends ISLISPExpressionNode {

    private final String name;

    @CompilerDirectives.CompilationFinal
    private LispFunction function;

    public ISLISPFunctionRef(String name) {
        this.name = name;
    }

    @Override
    public Value executeGeneric(VirtualFrame frame) {
        if (function == null) {
            function = ISLISPContext.get(this).lookupFunction(name);
        }
        return function;
    }
}
