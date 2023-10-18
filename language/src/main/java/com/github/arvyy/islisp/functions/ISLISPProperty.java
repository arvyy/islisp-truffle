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

public abstract class ISLISPProperty extends RootNode {

    @Child
    private ISLISPErrorSignalerNode errorSignalerNode;

    public ISLISPProperty(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode();
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 3 && frame.getArguments().length != 4) {
            return errorSignalerNode.signalWrongArgumentCount(
                frame.getArguments().length,
                2,
                3);
        }
        var defaultValue = frame.getArguments().length == 4
            ? frame.getArguments()[3]
            : ISLISPContext.get(this).getNil();
        return executeGeneric(frame.getArguments()[1], frame.getArguments()[2], defaultValue);
    }

    protected abstract Object executeGeneric(Object symbol, Object property, Object defaultValue);

    @Specialization(guards = {
        "symbol.identityReference().getId() == symbolLastId",
        "property.identityReference().getId() == propertyLastId"
    })
    Object doSymbols(
        Symbol symbol,
        Symbol property,
        Object defaultValue,
        @Cached("symbol.identityReference().getId()") int symbolLastId,
        @Cached("property.identityReference().getId()") int propertyLastId,
        @Cached("getPropertyReference(symbol, property)") ValueReference reference
    ) {
        if (reference.getValue() == null) {
            return defaultValue;
        } else {
            return reference.getValue();
        }
    }

    @Specialization(guards = {
        "notSymbol(symbol, property)"
    })
    Object doFallback(Object symbol, Object property, Object defaultValue) {
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
        return new LispFunction(ISLISPPropertyNodeGen.create(lang).getCallTarget());
    }

}
