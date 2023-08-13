package com.github.arvyy.islisp;

import com.github.arvyy.islisp.builtins.*;
import com.github.arvyy.islisp.runtime.LispFunction;
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

    private final Map<String, LispFunction> globalFunctions;
    private final Map<String, LispFunction> macros;

    public ISLISPContext(ISLISPTruffleLanguage language, Env env) {
        this.language = language;
        this.env = env;
        globalFunctions = new HashMap<>();
        macros = new HashMap<>();
        initGlobalFunctions();
    }

    void initGlobalFunctions() {
        globalFunctions.put("+", BuiltinAdd.makeLispFunction(language));
        globalFunctions.put("-", BuiltinSubtract.makeLispFunction(language));
        globalFunctions.put("=", BuiltinNumericEqual.makeLispFunction(language));
        globalFunctions.put(">", BuiltinNumericGt.makeLispFunction(language));
        globalFunctions.put("print", BuiltinPrint.makeLispFunction(language));
    }

    public void reset() {
        globalFunctions.clear();
        macros.clear();
        initGlobalFunctions();
    }

    public void registerFunction(String name, LispFunction function) {
        globalFunctions.put(name, function);
    }
    public LispFunction lookupFunction(String name) {
        return globalFunctions.get(name);
    }

    public void registerMacro(String name, LispFunction function) {
        macros.put(name, function);
    }

    public LispFunction lookupMacro(String name) {
        return macros.get(name);
    }

    public ISLISPTruffleLanguage getLanguage() {
        return language;
    }

    public Env getEnv() {
        return env;
    }
}
