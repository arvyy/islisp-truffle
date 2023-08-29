package com.github.arvyy.islisp;

import com.github.arvyy.islisp.builtins.*;
import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.TruffleLanguage.Env;
import com.oracle.truffle.api.nodes.Node;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class ISLISPContext {

    private static final TruffleLanguage.ContextReference<ISLISPContext> ctxRef = TruffleLanguage.ContextReference.create(ISLISPTruffleLanguage.class);
    public static ISLISPContext get(Node node) {
        return ctxRef.get(node);
     }
    private final ISLISPTruffleLanguage language;
    private final Env env;
    private Symbol NIL, T;

    private final Map<SymbolReference, LispFunction> globalFunctions;
    private final Map<SymbolReference, GenericFunctionDescriptor> genericFunctions;
    private final Map<SymbolReference, LispFunction> macros;
    private final Map<String, SymbolReference> symbols;
    private final Map<SymbolReference, LispClass> classes;

    public ISLISPContext(ISLISPTruffleLanguage language, Env env) {
        this.language = language;
        this.env = env;
        globalFunctions = new HashMap<>();
        genericFunctions = new HashMap<>();
        macros = new HashMap<>();
        symbols = new HashMap<>();
        classes = new HashMap<>();
        initGlobalFunctions();
        initBuiltinClasses();
    }

    void initGlobalFunctions() {
        globalFunctions.put(namedSymbol("+").identityReference(), BuiltinAdd.makeLispFunction(language));
        globalFunctions.put(namedSymbol("eq").identityReference(), BuiltinEq.makeLispFunction(language));
        globalFunctions.put(namedSymbol("-").identityReference(), BuiltinSubtract.makeLispFunction(language));
        globalFunctions.put(namedSymbol("=").identityReference(), BuiltinNumericEqual.makeLispFunction(language));
        globalFunctions.put(namedSymbol(">").identityReference(), BuiltinNumericGt.makeLispFunction(language));
        globalFunctions.put(namedSymbol("print").identityReference(), BuiltinPrint.makeLispFunction(language));
        globalFunctions.put(namedSymbol("class-of").identityReference(), BuiltinClassOf.makeLispFunction(language));
        globalFunctions.put(namedSymbol("gensym").identityReference(), BuiltinGensym.makeLispFunction(language));
    }

    void initBuiltin(String name, String... parents) {
        var symbol = namedSymbol(name);
        var parentClasses = Arrays.stream(parents)
                .map(pname -> classes.get(namedSymbol(pname).identityReference()))
                .toList();
        classes.put(symbol.identityReference(), new BuiltinClass(parentClasses, symbol));
    }

    void initBuiltinClasses() {
        initBuiltin("<object>");
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
        initGlobalFunctions();
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
    public void registerGenericFunction(SymbolReference symbolReference, LispFunction function, GenericFunctionDescriptor descriptor) {
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
        return new Symbol(name, v, null);
    }

    public Symbol getNIL() {
        if (NIL == null) {
            NIL = namedSymbol("nil");
        }
        return NIL;
    }

    public Symbol getT() {
        if (T == null) {
            T = namedSymbol("t");
        }
        return T;
    }

    private int gensymIndex = 1;
    public int gensymIndex() {
        return gensymIndex++;
    }
}
