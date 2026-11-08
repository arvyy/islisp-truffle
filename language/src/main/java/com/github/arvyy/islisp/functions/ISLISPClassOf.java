package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;

/**
 * Implements `class-of` function, returning given object's class.
 */
public abstract class ISLISPClassOf extends ISLISPBuiltinFunctionRootNode {

    ISLISPClassOf(TruffleLanguage<?> language) {
        super("class-of", arg(1), language);
    }

    abstract Object executeGeneric(Object value);

    @Override
    public final Object executeImpl(VirtualFrame frame) {
        var value = (Object) frame.getArguments()[1];
        return executeGeneric(value);
    }

    @Specialization
    LispClass doInt(
        int integer,
        @Shared("ints") @Cached("loadIntegerClass()") LispClass lispClass
    ) {
        return lispClass;
    }

    @Specialization
    LispClass doBigInt(
        LispBigInteger integer,
        @Shared("ints") @Cached("loadIntegerClass()") LispClass lispClass
    ) {
        return lispClass;
    }

    @Specialization
    LispClass doFloat(
        double flt,
        @Cached("loadFloatClass()") LispClass lispClass
    ) {
        return lispClass;
    }

    @Specialization
    LispClass doString(
        String str,
        @Shared("strings") @Cached("loadStringClass()") LispClass lispClass
    ) {
        return lispClass;
    }

    @Specialization
    LispClass doStringBuffer(
        LispMutableString str,
        @Shared("strings") @Cached("loadStringClass()") LispClass lispClass
    ) {
        return lispClass;
    }

    @Specialization
    LispClass doFunction(
            LispFunction fun,
            @Cached("loadFunctionClass()") LispClass funClass,
            @Cached("loadStandardGenericFunctionClass()") LispClass genFunClass
    ) {
        return fun.isGeneric() ? genFunClass : funClass;
    }

    @Specialization
    LispClass doSymbol(
            Symbol symbol,
            @Cached("loadNullClass()") LispClass nullClass,
            @Cached("loadSymbolClass()") LispClass symbolClass) {
        return symbol.name().equals("NIL") ? nullClass : symbolClass;
    }

    @Specialization
    LispClass doSymbol(
        Pair pair,
        @Cached("loadPairClass()") LispClass pairClass
    ) {
        return pairClass;
    }

    @Specialization
    LispClass doStandardClass(
            StandardClass clazz,
            @Cached("loadStandardClass()") LispClass standardClass
    ) {
        return standardClass;
    }

    @Specialization
    LispClass doBuiltinClass(
            BuiltinClass clazz,
            @Cached("loadBuiltinClass()") LispClass builtinClass
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

    @Specialization
    LispClass doNativeLibrary(
        LispNativeLibrary library,
        @Cached("loadNativeLibraryClass()") LispClass libClass
    ) {
        return libClass;
    }

    @Specialization
    LispClass doNativeLibrary(
        ISLISPModule module,
        @Cached("loadModuleClass()") LispClass libClass
    ) {
        return libClass;
    }

    @Specialization(guards = {
        "interop.hasMembers(o)",
        "!interop.hasArrayElements(o)"
    }, limit = "3")
    LispClass doTruffleInteropObject(
        Object o,
        @CachedLibrary("o") InteropLibrary interop,
        @Cached("loadTruffleObjectClass()") LispClass truffleObjectClass
    ) {
        return truffleObjectClass;
    }

    @Specialization(guards = {
        "interop.hasArrayElements(o)"
    }, limit = "3")
    LispClass doTruffleInteropVector(
        Object o,
        @CachedLibrary("o") InteropLibrary interop,
        @Cached("loadTruffleVectorClass()") LispClass truffleObjectClass
    ) {
        return truffleObjectClass;
    }

    @Fallback
    @CompilerDirectives.TruffleBoundary
    LispClass doFallback(
        Object value,
        @Cached("loadObjectClass()") LispClass objClass
    ) {
        return objClass;
    }

    @NeverDefault
    LispClass loadIntegerClass() {
        return loadClass("<integer>");
    }

    @NeverDefault
    LispClass loadFloatClass() {
        return loadClass("<float>");
    }

    @NeverDefault
    LispClass loadFunctionClass() {
        return loadClass("<function>");
    }

    @NeverDefault
    LispClass loadStandardGenericFunctionClass() {
        return loadClass("<standard-generic-function>");
    }

    @NeverDefault
    LispClass loadNullClass() {
        return loadClass("<null>");
    }

    @NeverDefault
    LispClass loadSymbolClass() {
        return loadClass("<symbol>");
    }

    @NeverDefault
    LispClass loadPairClass() {
        return loadClass("<cons>");
    }

    @NeverDefault
    LispClass loadBuiltinClass() {
        return loadClass("<built-in-class>");
    }

    @NeverDefault
    LispClass loadStandardClass() {
        return loadClass("<standard-class>");
    }

    @NeverDefault
    LispClass loadStringClass() {
        return loadClass("<string>");
    }

    @NeverDefault
    LispClass loadVectorClass() {
        return loadClass("<general-vector>");
    }

    @NeverDefault
    LispClass loadStreamClass() {
        return loadClass("<stream>");
    }

    @NeverDefault
    LispClass loadCharacterClass() {
        return loadClass("<character>");
    }

    @NeverDefault
    LispClass loadArrayClass() {
        return loadClass("<general-array*>");
    }

    @NeverDefault
    LispClass loadTruffleObjectClass() {
        return loadClass("<truffle-object>");
    }

    @NeverDefault
    LispClass loadTruffleVectorClass() {
        return loadClass("<truffle-vector>");
    }

    @NeverDefault
    LispClass loadNativeLibraryClass() {
        return loadClass("<truffle-native-library>");
    }

    @NeverDefault
    LispClass loadModuleClass() {
        return loadClass("<module>");
    }

    @NeverDefault
    LispClass loadObjectClass() {
        return loadClass("<object>");
    }

    LispClass loadClass(String name) {
        var ctx = ISLISPContext.get(this);
        var clazz = ctx.lookupClass(name);
        if (clazz == null) {
            throw new ISLISPError(
                    "Unexpected failure to locate object's class: " + name,
                    this);
        }
        return clazz;
    }

}
