package com.github.arvyy.islisp;

import com.github.arvyy.islisp.builtins.*;
import com.github.arvyy.islisp.nodes.ISLISPDefGenericExecutionNodeGen;
import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.TruffleLanguage.Env;
import com.oracle.truffle.api.nodes.Node;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

public class ISLISPContext {

    private static final TruffleLanguage.ContextReference<ISLISPContext> CTX_REF
            = TruffleLanguage.ContextReference.create(ISLISPTruffleLanguage.class);
    public static ISLISPContext get(Node node) {
        return CTX_REF.get(node);
     }
    private final ISLISPTruffleLanguage language;
    private final Env env;
    private Symbol nil;
    private Symbol t;

    private final Map<SymbolReference, LispFunction> globalFunctions;
    private final Map<SymbolReference, GenericFunctionDescriptor> genericFunctions;
    private final Map<SymbolReference, LispFunction> macros;
    private final Map<SymbolReference, SetfTransformer> setfTransformers;
    private final Map<String, SymbolReference> symbols;
    private final Map<SymbolReference, LispClass> classes;
    private final Map<SymbolReference, ValueReference> dynamicVars;
    private final Map<SymbolReference, ValueReference> globalVars;

    public ISLISPContext(ISLISPTruffleLanguage language, Env env) {
        this.language = language;
        this.env = env;
        globalFunctions = new HashMap<>();
        genericFunctions = new HashMap<>();
        dynamicVars = new HashMap<>();
        macros = new HashMap<>();
        symbols = new HashMap<>();
        classes = new HashMap<>();
        setfTransformers = new HashMap<>();
        globalVars = new HashMap<>();
        initBuiltinClasses();
        initGlobalFunctions();
        initSetfExpanders();
    }

    void initGlobalFunction(String name, Function<TruffleLanguage<?>, LispFunction> f) {
        globalFunctions.put(namedSymbol(name).identityReference(), f.apply(language));
    }

    void initGlobalFunctions() {
        initGlobalFunction("+", BuiltinAdd::makeLispFunction);
        initGlobalFunction("eq", BuiltinEq::makeLispFunction);
        initGlobalFunction("-", BuiltinSubtract::makeLispFunction);
        initGlobalFunction("=", BuiltinNumericEqual::makeLispFunction);
        initGlobalFunction(">", BuiltinNumericGt::makeLispFunction);
        initGlobalFunction("car", BuiltinCar::makeLispFunction);
        initGlobalFunction("cdr", BuiltinCdr::makeLispFunction);
        initGlobalFunction("cons", BuiltinCons::makeLispFunction);
        initGlobalFunction("class-of", BuiltinClassOf::makeLispFunction);
        initGlobalFunction("gensym", BuiltinGensym::makeLispFunction);
        initGlobalFunction("format-integer", BuiltinFormatInteger::makeLispFunction);
        initGlobalFunction("format-char", BuiltinFormatChar::makeLispFunction);
        initGlobalFunction("set-car", BuiltinSetCar::makeLispFunction);
        initGlobalFunction("set-cdr", BuiltinSetCdr::makeLispFunction);
        initGlobalFunction("standard-output", BuiltinStandardOutputStream::makeLispFunction);

        var createDescriptor = new GenericFunctionDescriptor(1, true);
        createDescriptor.addPrimaryMethod(
                new LispClass[] {classes.get(namedSymbol("<standard-class>").identityReference())},
                BuiltinCreateStandardClassObject.makeLispFunction(language).callTarget(),
                null);
        genericFunctions.put(namedSymbol("create").identityReference(), createDescriptor);
        var executionNode = ISLISPDefGenericExecutionNodeGen.create(namedSymbol("create"), getLanguage(), null);
        globalFunctions.put(namedSymbol("create").identityReference(), new LispFunction(executionNode.getCallTarget()));
    }

    void initSetfExpanders() {
        setfTransformers.put(namedSymbol("car").identityReference(), (forms, value) -> {
            return Utils.listToValue(List.of(
                    namedSymbol("set-car"),
                    forms.get(1),
                    value
            ));
        });
        setfTransformers.put(namedSymbol("cdr").identityReference(), (forms, value) -> {
            return Utils.listToValue(List.of(
                    namedSymbol("set-cdr"),
                    forms.get(1),
                    value
            ));
        });
    }

    void initBuiltin(String name, String... parents) {
        var symbol = namedSymbol(name);
        var parentClasses = Arrays.stream(parents)
                .map(pname -> classes.get(namedSymbol(pname).identityReference()))
                .toList();
        classes.put(symbol.identityReference(), new BuiltinClass(parentClasses, symbol, false));
    }

    void initBuiltinClasses() {
        initBuiltin("<object>");
        initBuiltin("<standard-class>", "<object>");
        initBuiltin("<function>", "<object>");
        initBuiltin("<number>", "<object>");
        initBuiltin("<symbol>", "<object>");
        initBuiltin("<list>", "<object>");
        initBuiltin("<null>", "<symbol>", "<list>");
        initBuiltin("<integer>", "<number>");
        initBuiltin("<float>", "<number>");
        initBuiltin("<built-in-class>", "<object>");
    }

    public void reset() {
        globalFunctions.clear();
        genericFunctions.clear();
        macros.clear();
        globalVars.clear();
        setfTransformers.clear();
        classes.clear();
        initBuiltinClasses();
        initGlobalFunctions();
    }

    @CompilerDirectives.TruffleBoundary
    public void registerGlobalVar(SymbolReference symbolReference, Object init, boolean readonly) {
        var v = new ValueReference();
        v.setValue(init);
        v.setReadOnly(readonly);
        globalVars.put(symbolReference, v);
    }

    @CompilerDirectives.TruffleBoundary
    public ValueReference lookupGlobalVar(SymbolReference symbolReference) {
        return globalVars.get(symbolReference);
    }

    @CompilerDirectives.TruffleBoundary
    public void registerSetfTransformer(SymbolReference symbolReference, SetfTransformer transformer) {
        setfTransformers.put(symbolReference, transformer);
    }

    @CompilerDirectives.TruffleBoundary
    public SetfTransformer lookupSetfTransformer(SymbolReference symbolReference) {
        return setfTransformers.get(symbolReference);
    }

    @CompilerDirectives.TruffleBoundary
    public void registerDynamicVar(SymbolReference symbolReference, ValueReference v) {
        dynamicVars.put(symbolReference, v);
    }

    @CompilerDirectives.TruffleBoundary
    public ValueReference lookupDynamicVar(SymbolReference symbolReference) {
        return dynamicVars.get(symbolReference);
    }

    @CompilerDirectives.TruffleBoundary
    public void registerFunction(SymbolReference symbolReference, LispFunction function) {
        globalFunctions.put(symbolReference, function);
    }
    @CompilerDirectives.TruffleBoundary
    public LispFunction lookupFunction(SymbolReference symbolReference) {
        return globalFunctions.get(symbolReference);
    }

    @CompilerDirectives.TruffleBoundary
    public void registerGenericFunction(
            SymbolReference symbolReference,
            LispFunction function,
            GenericFunctionDescriptor descriptor
    ) {
        globalFunctions.put(symbolReference, function);
        genericFunctions.put(symbolReference, descriptor);
    }

    @CompilerDirectives.TruffleBoundary
    public GenericFunctionDescriptor lookupGenericFunctionDispatchTree(SymbolReference symbolReference) {
        return genericFunctions.get(symbolReference);
    }

    @CompilerDirectives.TruffleBoundary
    public void registerMacro(SymbolReference symbolReference, LispFunction function) {
        macros.put(symbolReference, function);
    }

    @CompilerDirectives.TruffleBoundary
    public LispFunction lookupMacro(SymbolReference symbolReference) {
        return macros.get(symbolReference);
    }

    @CompilerDirectives.TruffleBoundary
    public void registerClass(SymbolReference symbolReference, LispClass clazz) {
        classes.put(symbolReference, clazz);
    }

    @CompilerDirectives.TruffleBoundary
    public LispClass lookupClass(SymbolReference symbolReference) {
        return classes.get(symbolReference);
    }

    public ISLISPTruffleLanguage getLanguage() {
        return language;
    }

    public Env getEnv() {
        return env;
    }

    @CompilerDirectives.TruffleBoundary
    public Symbol namedSymbol(String name) {
        var v = symbols.computeIfAbsent(name, k -> new SymbolReference());
        return new Symbol(name, v);
    }

    public Symbol getNil() {
        if (nil == null) {
            nil = namedSymbol("nil");
        }
        return nil;
    }

    public Symbol getT() {
        if (t == null) {
            t = namedSymbol("t");
        }
        return t;
    }

    private int gensymIndex = 1;
    public int gensymIndex() {
        return gensymIndex++;
    }
}
