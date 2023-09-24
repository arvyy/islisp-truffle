package com.github.arvyy.islisp.builtins;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.ISLISPError;
import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public abstract class BuiltinClassOf extends RootNode {

    public BuiltinClassOf(TruffleLanguage<?> language) {
        super(language);
    }

    protected abstract LispClass executeGeneric(Object value);

    @Override
    public final LispClass execute(VirtualFrame frame) {
        var value = (Value) frame.getArguments()[1];
        return executeGeneric(value);
    }

    @Specialization
    protected LispClass doInt(
            LispInteger integer,
            @Cached("loadIntegerClass()") LispClass lispClass) {
        return lispClass;
    }

    @Specialization
    protected LispClass doFunction(
            LispFunction fun,
            @Cached("loadFunctionClass()") LispClass lispClass) {
        return lispClass;
    }

    @Specialization
    protected LispClass doSymbol(
            Symbol symbol,
            @Cached("loadNullClass()") LispClass nullClass,
            @Cached("loadSymbolClass()") LispClass symbolClass) {
        return symbol.name().equals("NIL")? nullClass : symbolClass;
    }

    @Specialization
    protected LispClass doStandardClass(
            StandardClass clazz,
            @Cached("loadStandardClass()") LispClass builtinClass
    ) {
        return builtinClass;
    }

    @Specialization
    protected LispClass doStandardClassObject(StandardClassObject obj) {
        return obj.getLispClass();
    }

    @Fallback
    @CompilerDirectives.TruffleBoundary
    protected LispClass doFallback(Object value) {
        throw new ISLISPError("Unknown class for value: " + value, this);
    }

    LispClass loadIntegerClass() { return loadClass("<integer>"); }
    LispClass loadFunctionClass() { return loadClass("<function>"); }
    LispClass loadNullClass() { return loadClass("<null>"); }
    LispClass loadSymbolClass() { return loadClass("<symbol>"); }

    LispClass loadBuiltinClass() {
        return loadClass("<built-in-class>");
    }

    LispClass loadStandardClass() {
        return loadClass("<standard-class>");
    }

    LispClass loadClass(String name) {
        var ctx = ISLISPContext.get(this);
        var symbol = ctx.namedSymbol(name);
        return ctx.lookupClass(symbol.identityReference());
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(BuiltinClassOfNodeGen.create(lang).getCallTarget());
    }
}
