package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispClass;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPClassRef extends ISLISPExpressionNode {

    private final Symbol name;
    @CompilerDirectives.CompilationFinal
    private LispClass clazz;

    public ISLISPClassRef(Symbol name, SourceSection sourceSection) {
        super(sourceSection);
        this.name = name;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        if (clazz == null) {
            clazz = ISLISPContext.get(this).lookupClass(name.identityReference());
        }
        return clazz;
    }

}
