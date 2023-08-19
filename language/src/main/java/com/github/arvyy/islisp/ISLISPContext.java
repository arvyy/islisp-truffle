package com.github.arvyy.islisp;

import com.github.arvyy.islisp.builtins.*;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.SymbolReference;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.TruffleLanguage.Env;
import com.oracle.truffle.api.nodes.Node;

import java.util.HashMap;
import java.util.Map;

public class ISLISPContext {

    private static final TruffleLanguage.ContextReference<ISLISPContext> ctxRef = TruffleLanguage.ContextReference.create(ISLISPTruffleLanguage.class);
    public static ISLISPContext get(Node node) {
        return ctxRef.get(node);
     }
    private final ISLISPTruffleLanguage language;
    private final Env env;
    private Symbol NIL, T;

    private final Map<SymbolReference, LispFunction> globalFunctions;
    private final Map<SymbolReference, LispFunction> macros;
    private final Map<String, SymbolReference> symbols;

    public ISLISPContext(ISLISPTruffleLanguage language, Env env) {
        this.language = language;
        this.env = env;
        globalFunctions = new HashMap<>();
        macros = new HashMap<>();
        symbols = new HashMap<>();
        initGlobalFunctions();
    }

    void initGlobalFunctions() {
        globalFunctions.put(namedSymbol("+").identityReference(), BuiltinAdd.makeLispFunction(language));
        globalFunctions.put(namedSymbol("-").identityReference(), BuiltinSubtract.makeLispFunction(language));
        globalFunctions.put(namedSymbol("=").identityReference(), BuiltinNumericEqual.makeLispFunction(language));
        globalFunctions.put(namedSymbol(">").identityReference(), BuiltinNumericGt.makeLispFunction(language));
        globalFunctions.put(namedSymbol("print").identityReference(), BuiltinPrint.makeLispFunction(language));
    }

    public void reset() {
        globalFunctions.clear();
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
    public void registerMacro(SymbolReference symbolReference, LispFunction function) {
        macros.put(symbolReference, function);
    }

    @CompilerDirectives.TruffleBoundary
    public LispFunction lookupMacro(SymbolReference symbolReference) {
        return macros.get(symbolReference);
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
            NIL = namedSymbol("NIL");
        }
        return NIL;
    }

    public Symbol getT() {
        if (T == null) {
            T = namedSymbol("T");
        }
        return T;
    }

}
