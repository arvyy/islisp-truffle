package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public abstract class ISLISPClassOf extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    public ISLISPClassOf(TruffleLanguage<?> language) {
        super(language);
    }

    protected abstract Object executeGeneric(Object value);

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 2) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length - 1, 1, 1);
        }
        var value = (Object) frame.getArguments()[1];
        return executeGeneric(value);
    }

    @Specialization
    protected LispClass doInt(
            int integer,
            @Cached("loadIntegerClass()") LispClass lispClass) {
        return lispClass;
    }

    @Specialization
    protected LispClass doFloat(
        double flt,
        @Cached("loadFloatClass()") LispClass lispClass) {
        return lispClass;
    }

    @Specialization
    protected LispClass doString(
        String str,
        @Cached("loadStringClass()") LispClass lispClass
    ) {
        return lispClass;
    }

    @Specialization
    protected LispClass doStringBuffer(
        StringBuffer str,
        @Cached("loadStringClass()") LispClass lispClass
    ) {
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
        return symbol.name().equals("NIL") ? nullClass : symbolClass;
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
        return obj.clazz();
    }

    @Fallback
    @CompilerDirectives.TruffleBoundary
    protected LispClass doFallback(Object value) {
        throw new ISLISPError("Unknown class for value: " + value, this);
    }

    LispClass loadIntegerClass() {
        return loadClass("<integer>");
    }
    LispClass loadFloatClass() {
        return loadClass("<float>");
    }
    LispClass loadFunctionClass() {
        return loadClass("<function>");
    }
    LispClass loadNullClass() {
        return loadClass("<null>");
    }
    LispClass loadSymbolClass() {
        return loadClass("<symbol>");
    }

    LispClass loadBuiltinClass() {
        return loadClass("<built-in-class>");
    }

    LispClass loadStandardClass() {
        return loadClass("<standard-class>");
    }

    LispClass loadStringClass() {
        return loadClass("<string>");
    }

    LispClass loadClass(String name) {
        var ctx = ISLISPContext.get(this);
        var symbol = ctx.namedSymbol(name);
        return ctx.lookupClass(symbol.identityReference());
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPClassOfNodeGen.create(lang).getCallTarget());
    }
}
