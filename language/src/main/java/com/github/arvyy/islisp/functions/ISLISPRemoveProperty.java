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

/**
 * Implements `remove-property` function, which removes given property from a given symbol.
 */
public abstract class ISLISPRemoveProperty extends RootNode {

    @Child
    private ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPRemoveProperty(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 3) {
            return errorSignalerNode.signalWrongArgumentCount(
                frame.getArguments().length,
                2,
                2);
        }
        return executeGeneric(frame.getArguments()[1], frame.getArguments()[2]);
    }

    abstract Object executeGeneric(Object symbol, Object property);

    @Specialization(guards = {
        "symbol.identityReference().getId() == symbolLastId",
        "property.identityReference().getId() == propertyLastId"
    })
    Object doSymbols(
        Symbol symbol,
        Symbol property,
        @Cached("symbol.identityReference().getId()") int symbolLastId,
        @Cached("property.identityReference().getId()") int propertyLastId,
        @Cached("getPropertyReference(symbol, property)") ValueReference reference
    ) {
        var prev = reference.getValue();
        if (prev == null) {
            prev = ISLISPContext.get(this).getNil();
        }
        reference.setValue(null);
        return prev;
    }

    @Specialization(guards = {
        "notSymbol(symbol, property)"
    })
    Object doFallback(Object symbol, Object property) {
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

    /**
     * Construct LispFunction using this root node.
     *
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPRemovePropertyNodeGen.create(lang).getCallTarget());
    }

}
