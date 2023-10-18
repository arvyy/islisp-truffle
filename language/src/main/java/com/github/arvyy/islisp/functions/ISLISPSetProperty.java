package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.ValueReference;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public abstract class ISLISPSetProperty extends RootNode {

    @Child
    private ISLISPErrorSignalerNode errorSignalerNode;

    public ISLISPSetProperty(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode();
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 4) {
            return errorSignalerNode.signalWrongArgumentCount(
                frame.getArguments().length,
                3,
                3);
        }
        return executeGeneric(frame.getArguments()[1], frame.getArguments()[2], frame.getArguments()[3]);
    }

    protected abstract Object executeGeneric(Object value, Object symbol, Object property);

    @Specialization(guards = {
        "symbol.identityReference().getId() == symbolLastId",
        "property.identityReference().getId() == propertyLastId"
    })
    Object doSymbols(
        Object value,
        Symbol symbol,
        Symbol property,
        @Cached("symbol.identityReference().getId()") int symbolLastId,
        @Cached("property.identityReference().getId()") int propertyLastId,
        @Cached("getPropertyReference(symbol, property)") ValueReference reference
    ) {
        reference.setValue(value);
        return value;
    }

    @Specialization(guards = {
        "notSymbol(symbol, property)"
    })
    Object doFallback(Object value, Object symbol, Object property) {
        var offender = symbol instanceof Symbol ? property : symbol;
        var ctx = ISLISPContext.get(this);
        return errorSignalerNode.signalWrongType(offender, ctx.lookupClass("<symbol>"));
    }

    ValueReference getPropertyReference(Symbol symbol, Symbol property) {
        var ctx = ISLISPContext.get(this);
        return ctx.lookupSymbolProperty(symbol.identityReference(), property.identityReference());
    }

    boolean notSymbol(Object symbol, Object property) {
        return !(symbol instanceof Symbol && property instanceof Symbol);
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPSetPropertyNodeGen.create(lang).getCallTarget());
    }

}
