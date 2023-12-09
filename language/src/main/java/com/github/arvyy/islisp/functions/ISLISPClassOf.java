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

/**
 * Implements `class-of` function, returning given object's class.
 */
public abstract class ISLISPClassOf extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPClassOf(TruffleLanguage<?> language) {
        super(language);
    }

    abstract Object executeGeneric(Object value);

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 2) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length - 1, 1, 1);
        }
        var value = (Object) frame.getArguments()[1];
        return executeGeneric(value);
    }

    @Specialization
    LispClass doInt(
            int integer,
            @Cached("loadIntegerClass()") LispClass lispClass) {
        return lispClass;
    }

    @Specialization
    LispClass doBigInt(
        LispBigInteger integer,
        @Cached("loadIntegerClass()") LispClass lispClass) {
        return lispClass;
    }

    @Specialization
    LispClass doFloat(
        double flt,
        @Cached("loadFloatClass()") LispClass lispClass) {
        return lispClass;
    }

    @Specialization
    LispClass doString(
        String str,
        @Cached("loadStringClass()") LispClass lispClass
    ) {
        return lispClass;
    }

    @Specialization
    LispClass doStringBuffer(
        StringBuffer str,
        @Cached("loadStringClass()") LispClass lispClass
    ) {
        return lispClass;
    }

    @Specialization
    LispClass doFunction(
            LispFunction fun,
            @Cached("loadFunctionClass()") LispClass lispClass) {
        return lispClass;
    }

    @Specialization
    LispClass doSymbol(
            Symbol symbol,
            @Cached("loadNullClass()") LispClass nullClass,
            @Cached("loadSymbolClass()") LispClass symbolClass) {
        return symbol.name().equals("NIL") ? nullClass : symbolClass;
    }

    @Specialization
    LispClass doStandardClass(
            StandardClass clazz,
            @Cached("loadStandardClass()") LispClass builtinClass
    ) {
        return builtinClass;
    }

    @Specialization
    LispClass doStandardClassObject(StandardClassObject obj) {
        return obj.clazz();
    }

    @Specialization
    LispClass doVector(
        LispVector vec,
        @Cached("loadVectorClass()") LispClass vectorClass
    ) {
        return vectorClass;
    }

    @Specialization
    LispClass doOutputStream(
        LispStream stream,
        @Cached("loadStreamClass()") LispClass streamClass
    ) {
        return streamClass;
    }

    @Specialization
    LispClass doCharacter(
        LispChar character,
        @Cached("loadCharacterClass()") LispClass characterClass
    ) {
        return characterClass;
    }

    @Specialization
    LispClass doArray(
        LispArray character,
        @Cached("loadArrayClass()") LispClass arrayClass
    ) {
        return arrayClass;
    }

    @Fallback
    @CompilerDirectives.TruffleBoundary
    LispClass doFallback(Object value) {
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

    LispClass loadVectorClass() {
        return loadClass("<general-vector>");
    }

    LispClass loadStreamClass() {
        return loadClass("<stream>");
    }

    LispClass loadCharacterClass() {
        return loadClass("<character>");
    }

    LispClass loadArrayClass() {
        return loadClass("<general-array*>");
    }

    LispClass loadClass(String name) {
        var ctx = ISLISPContext.get(this);
        var symbol = ctx.namedSymbol(name);
        return ctx.lookupClass(symbol.identityReference());
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPClassOfNodeGen.create(lang).getCallTarget());
    }
}
