package com.github.arvyy.islisp;

import com.github.arvyy.islisp.functions.*;
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

/**
 * Context class holding all active data for the execution.
 */
public class ISLISPContext {

    private static final TruffleLanguage.ContextReference<ISLISPContext> CTX_REF
            = TruffleLanguage.ContextReference.create(ISLISPTruffleLanguage.class);

    /**
     * Get current islisp context.
     *
     * @param node node where context used, can be null.
     * @return context
     */
    public static ISLISPContext get(Node node) {
        return CTX_REF.get(node);
     }
    private final ISLISPTruffleLanguage language;
    private final Env env;

    @CompilerDirectives.CompilationFinal
    private Symbol nil;

    @CompilerDirectives.CompilationFinal
    private Symbol t;

    private final Map<SymbolReference, LispFunction> globalFunctions;
    private final Map<SymbolReference, GenericFunctionDescriptor> genericFunctions;
    private final Map<SymbolReference, LispFunction> setfGlobalFunctions;
    private final Map<SymbolReference, GenericFunctionDescriptor> setfGenericFunctions;
    private final Map<SymbolReference, LispFunction> macros;
    private final Map<SymbolReference, SetfTransformer> setfTransformers;
    private final Map<String, SymbolReference> symbols;
    private final Map<SymbolReference, LispClass> classes;
    private final Map<SymbolReference, ValueReference> dynamicVars;
    private final Map<SymbolReference, ValueReference> globalVars;
    private final Map<SymbolReference, Map<SymbolReference, ValueReference>> symbolProperties;

    private HandlerChain handlerChain;

    /**
     * Create islisp context.
     *
     * @param language
     * @param env
     */
    public ISLISPContext(ISLISPTruffleLanguage language, Env env) {
        this.language = language;
        this.env = env;
        globalFunctions = new HashMap<>();
        genericFunctions = new HashMap<>();
        setfGlobalFunctions = new HashMap<>();
        setfGenericFunctions = new HashMap<>();
        symbolProperties = new HashMap<>();
        dynamicVars = new HashMap<>();
        macros = new HashMap<>();
        symbols = new HashMap<>();
        classes = new HashMap<>();
        setfTransformers = new HashMap<>();
        globalVars = new HashMap<>();
        initBuiltinVars();
        initBuiltinClasses();
        initGlobalFunctions();
        initSetfExpanders();
    }

    /**
     * Push handler into active handler stack, making it first handler to be hit by the condition signal.
     * @param f handler function
     */
    public void pushHandler(LispFunction f) {
        handlerChain = new HandlerChain(f, handlerChain);
    }

    /**
     * Pop a signal handler from the active handler stack.
     *
     * @return popped handler
     */
    public LispFunction popHandler() {
        var f = handlerChain.handler();
        handlerChain = handlerChain.rest();
        return f;
    }

    void initGlobalFunction(String name, Function<TruffleLanguage<?>, LispFunction> f) {
        globalFunctions.put(namedSymbol(name).identityReference(), f.apply(language));
    }

    /**
     * Initialize builtin functions into function namespace storage.
     */
    void initGlobalFunctions() {
        // standard
        initGlobalFunction("+", ISLISPAdd::makeLispFunction);
        initGlobalFunction("-", ISLISPSubtract::makeLispFunction);
        initGlobalFunction("=", ISLISPNumericEqual::makeLispFunction);
        initGlobalFunction(">", ISLISPNumericGt::makeLispFunction);
        initGlobalFunction("car", ISLISPCar::makeLispFunction);
        initGlobalFunction("cdr", ISLISPCdr::makeLispFunction);
        initGlobalFunction("char=", ISLISPCharEqual::makeLispFunction);
        initGlobalFunction("char<", ISLISPCharLt::makeLispFunction);
        initGlobalFunction("class-of", ISLISPClassOf::makeLispFunction);
        initGlobalFunction("cons", ISLISPCons::makeLispFunction);
        initGlobalFunction("continue-condition", ISLISPContinueCondition::makeLispFunction);
        initGlobalFunction("create-string-input-stream", ISLISPCreateStringInputStream::makeLispFunction);
        initGlobalFunction("create-string-output-stream", ISLISPCreateStringOutputStream::makeLispFunction);
        initGlobalFunction("create-vector", ISLISPCreateVector::makeLispFunction);
        initGlobalFunction("elt", ISLISPElt::makeLispFunction);
        initGlobalFunction("error-output", ISLISPErrorOutputStream::makeLispFunction);
        initGlobalFunction("eq", ISLISPEq::makeLispFunction);
        initGlobalFunction("equal", ISLISPEqual::makeLispFunction);
        initGlobalFunction("format-char", ISLISPFormatChar::makeLispFunction);
        initGlobalFunction("format-integer", ISLISPFormatInteger::makeLispFunction);
        initGlobalFunction("format-float", ISLISPFormatFloat::makeLispFunction);
        initGlobalFunction("format-object", ISLISPFormatObject::makeLispFunction);
        initGlobalFunction("gensym", ISLISPGensym::makeLispFunction);
        initGlobalFunction("get-output-stream-string", ISLISPGetOutputStreamString::makeLispFunction);
        initGlobalFunction("instancep", ISLISPInstancep::makeLispFunction);
        initGlobalFunction("length", ISLISPLength::makeLispFunction);
        initGlobalFunction("list", ISLISPList::makeLispFunction);
        initGlobalFunction("property", ISLISPProperty::makeLispFunction);
        initGlobalFunction("remove-property", ISLISPRemoveProperty::makeLispFunction);
        initGlobalFunction("set-car", ISLISPSetCar::makeLispFunction);
        initGlobalFunction("set-cdr", ISLISPSetCdr::makeLispFunction);
        initGlobalFunction("set-property", ISLISPSetProperty::makeLispFunction);
        initGlobalFunction("signal-condition", ISLISPSignalCondition::makeLispFunction);
        initGlobalFunction("standard-output", ISLISPStandardOutputStream::makeLispFunction);
        initGlobalFunction("subclassp", ISLISPSubclassp::makeLispFunction);
        initGlobalFunction("vector", ISLISPVector::makeLispFunction);
        initCreateMethod();
        initInitializeObjectMethod();

        //extension
        initGlobalFunction("current-stacktrace", ISLISPCurrentStacktrace::makeLispFunction);
        initGlobalFunction("exit", ISLISPExit::makeLispFunction);
    }

    private void initInitializeObjectMethod() {
        var initializeObjectDescriptor = new GenericFunctionDescriptor(1, true);
        initializeObjectDescriptor.addPrimaryMethod(
            new LispClass[] {classes.get(namedSymbol("<object>").identityReference())},
            ISLISPInitializeObject.makeLispFunction(language).callTarget(),
            null);
        var initializeObjectExecutionNode = ISLISPDefGenericExecutionNodeGen.create(
            namedSymbol("initialize-object"),
            false,
            getLanguage(),
            null);
        registerGenericFunction(
            namedSymbol("initialize-object").identityReference(),
            false,
            new LispFunction(initializeObjectExecutionNode.getCallTarget()),
            initializeObjectDescriptor);
    }

    private void initCreateMethod() {
        var createDescriptor = new GenericFunctionDescriptor(1, true);
        createDescriptor.addPrimaryMethod(
                new LispClass[] {classes.get(namedSymbol("<standard-class>").identityReference())},
                ISLISPCreateStandardClassObject.makeLispFunction(language).callTarget(),
                null);
        var createExecutionNode = ISLISPDefGenericExecutionNodeGen.create(
            namedSymbol("create"),
            false,
            getLanguage(),
            null);
        registerGenericFunction(
            namedSymbol("create").identityReference(),
            false,
            new LispFunction(createExecutionNode.getCallTarget()),
            createDescriptor);
    }

    void initSetfExpanders() {
        setfTransformers.put(namedSymbol("car").identityReference(), (forms, value) -> {
            return Utils.listToValue(List.of(
                namedSymbol("set-car"),
                value,
                forms.get(1)
            ));
        });
        setfTransformers.put(namedSymbol("cdr").identityReference(), (forms, value) -> {
            return Utils.listToValue(List.of(
                namedSymbol("set-cdr"),
                value,
                forms.get(1)
            ));
        });
        setfTransformers.put(namedSymbol("property").identityReference(), (forms, value) -> {
            return Utils.listToValue(List.of(
                namedSymbol("set-property"),
                value,
                forms.get(1),
                forms.get(2)
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

    /**
     * Initialize builtin class graph.
     */
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
        initBuiltin("<basic-array>", "<object>");
        initBuiltin("<basic-vector>", "<basic-array>");
        initBuiltin("<string>", "<basic-vector>");
        initBuiltin("<general-vector>", "<basic-vector>");
        initBuiltin("<stream>", "<object>");
        initBuiltin("<character>", "<object>");
    }

    /**
     * Initialize builtin constants.
     */
    void initBuiltinVars() {
        var nilref = new ValueReference();
        nilref.setValue(getNil());
        nilref.setReadOnly(true);
        globalVars.put(getNil().identityReference(), nilref);

        var tref = new ValueReference();
        tref.setValue(getT());
        tref.setReadOnly(true);
        globalVars.put(getT().identityReference(), tref);
    }

    /**
     * A soft reset to be done after macro expansion phase.
     */
    public void reset() {
        setfGlobalFunctions.clear();
        globalFunctions.clear();
        setfGenericFunctions.clear();
        genericFunctions.clear();
        macros.clear();
        globalVars.clear();
        setfTransformers.clear();
        classes.clear();
        symbolProperties.clear();
        initBuiltinVars();
        initBuiltinClasses();
        initGlobalFunctions();
    }

    /**
     * Register global variable (mutable or immutable).
     *
     * @param symbolReference variable name
     * @param init initialization value
     * @param readonly if the variable is a constant
     */
    @CompilerDirectives.TruffleBoundary
    public void registerGlobalVar(SymbolReference symbolReference, Object init, boolean readonly) {
        var v = new ValueReference();
        v.setValue(init);
        v.setReadOnly(readonly);
        globalVars.put(symbolReference, v);
    }

    /**
     * Find an associated property's value reference with a symbol. Creates if missing.
     *
     * @param symbol symbol
     * @param property property
     * @return property's value reference
     */
    @CompilerDirectives.TruffleBoundary
    public ValueReference lookupSymbolProperty(SymbolReference symbol, SymbolReference property) {
        symbolProperties.putIfAbsent(symbol, new HashMap<>());
        var properties = symbolProperties.get(symbol);
        properties.putIfAbsent(property, new ValueReference());
        return properties.get(property);
    }

    /**
     * Find global variable by name.
     *
     * @param symbolReference name
     * @return variable's value reference or null if not found
     */
    @CompilerDirectives.TruffleBoundary
    public ValueReference lookupGlobalVar(SymbolReference symbolReference) {
        return globalVars.get(symbolReference);
    }

    /**
     * Register setf transformer for a given name. When encountering `(setf (name form ...) value)`
     * the given transformer will be used to expand it into a non-setf expression.
     *
     * @param symbolReference transformer name.
     * @param transformer transformer.
     */
    @CompilerDirectives.TruffleBoundary
    public void registerSetfTransformer(SymbolReference symbolReference, SetfTransformer transformer) {
        setfTransformers.put(symbolReference, transformer);
    }

    /**
     * Find a setf transformer for a given name.
     *
     * @param symbolReference transformer name.
     * @return setf transformer or null if undefined
     */
    @CompilerDirectives.TruffleBoundary
    public SetfTransformer lookupSetfTransformer(SymbolReference symbolReference) {
        return setfTransformers.get(symbolReference);
    }

    /**
     * Register dynamic variable.
     *
     * @param symbolReference variable name.
     * @param v value reference.
     */
    @CompilerDirectives.TruffleBoundary
    public void registerDynamicVar(SymbolReference symbolReference, ValueReference v) {
        dynamicVars.put(symbolReference, v);
    }

    /**
     * Find dynamic variable value reference for a given name.
     *
     * @param symbolReference dynamic variable name
     * @return dynamic variable value reference
     */
    @CompilerDirectives.TruffleBoundary
    public ValueReference lookupDynamicVar(SymbolReference symbolReference) {
        return dynamicVars.get(symbolReference);
    }

    /**
     * Register function into function namespace.
     *
     * @param symbolReference function name
     * @param function function value
     */
    @CompilerDirectives.TruffleBoundary
    public void registerFunction(SymbolReference symbolReference, LispFunction function) {
        globalFunctions.put(symbolReference, function);
    }

    /**
     * Find function by name. Function can be a generic function or plain.
     *
     * @param symbolReference function name
     * @return function or null if undefined
     */
    @CompilerDirectives.TruffleBoundary
    public LispFunction lookupFunction(SymbolReference symbolReference) {
        return lookupFunction(symbolReference, false);
    }

    /**
     * Find function by name. Function can be a generic function or plain.
     *
     * @param symbolReference function name
     * @param setf whether function is of setf form in case it's generic.
     * @return function or null if undefined
     */
    @CompilerDirectives.TruffleBoundary
    public LispFunction lookupFunction(SymbolReference symbolReference, boolean setf) {
        if (setf) {
            return setfGlobalFunctions.get(symbolReference);
        } else {
            return globalFunctions.get(symbolReference);
        }
    }

    /**
     * Register a generic function.
     *
     * @param symbolReference function name
     * @param setf whether it's of setf form
     * @param function function call entrypoint function implementation
     * @param descriptor generic descriptor
     */
    @CompilerDirectives.TruffleBoundary
    public void registerGenericFunction(
            SymbolReference symbolReference,
            boolean setf,
            LispFunction function,
            GenericFunctionDescriptor descriptor
    ) {
        if (setf) {
            setfGlobalFunctions.put(symbolReference, function);
            setfGenericFunctions.put(symbolReference, descriptor);
        } else {
            globalFunctions.put(symbolReference, function);
            genericFunctions.put(symbolReference, descriptor);
        }
    }

    /**
     * Find generic descriptor for a given generic function name.
     *
     * @param symbolReference generic function name
     * @param setf if the function is of setf form
     * @return generic function descriptor or null if undefined
     */
    @CompilerDirectives.TruffleBoundary
    public GenericFunctionDescriptor lookupGenericFunctionDispatchTree(SymbolReference symbolReference, boolean setf) {
        if (setf) {
            return setfGenericFunctions.get(symbolReference);
        } else {
            return genericFunctions.get(symbolReference);
        }
    }

    /**
     * Register macro with a given name.
     *
     * @param symbolReference macro name
     * @param function macro implementation function
     */
    @CompilerDirectives.TruffleBoundary
    public void registerMacro(SymbolReference symbolReference, LispFunction function) {
        macros.put(symbolReference, function);
    }

    /**
     * Find macro by given symbol reference name.
     *
     * @param symbolReference reference
     * @return macro function or null if undefined
     */
    @CompilerDirectives.TruffleBoundary
    public LispFunction lookupMacro(SymbolReference symbolReference) {
        return macros.get(symbolReference);
    }

    /**
     * Add a new class to context.
     *
     * @param symbolReference class name's reference
     * @param clazz class instance
     */
    @CompilerDirectives.TruffleBoundary
    public void registerClass(SymbolReference symbolReference, LispClass clazz) {
        classes.put(symbolReference, clazz);
    }

    /**
     * Find a class by symbol reference.
     *
     * @param symbolReference class symbolic name's reference
     * @return class or null if doesn't exist
     */
    @CompilerDirectives.TruffleBoundary
    public LispClass lookupClass(SymbolReference symbolReference) {
        return classes.get(symbolReference);
    }

    /**
     * Find a class by string name.
     *
     * @param name class name
     * @return class or null if doesn't exit
     */
    @CompilerDirectives.TruffleBoundary
    public LispClass lookupClass(String name) {
        return lookupClass(namedSymbol(name).identityReference());
    }

    /**
     * Get associated language reference.
     *
     * @return islisp language object
     */
    public ISLISPTruffleLanguage getLanguage() {
        return language;
    }

    /**
     * Get execution environment.
     *
     * @return execution environment
     */
    public Env getEnv() {
        return env;
    }

    /**
     * Get a named symbol name. Creates if doesn't exist.
     * @param name symbol name
     * @return symbol
     */
    @CompilerDirectives.TruffleBoundary
    public Symbol namedSymbol(String name) {
        var v = symbols.computeIfAbsent(name, k -> new SymbolReference());
        return new Symbol(name, v);
    }

    /**
     * Get `nil` symbol.
     *
     * @return nil symbol
     */
    public Symbol getNil() {
        if (nil == null) {
            nil = namedSymbol("nil");
        }
        return nil;
    }

    /**
     * Get `t` symbol.
     *
     * @return t symbol
     */
    public Symbol getT() {
        if (t == null) {
            t = namedSymbol("t");
        }
        return t;
    }

    private int gensymIndex = 1;

    /**
     * Get next index to be used in gensym's autogenerated name.
     *
     * @return next gensym index.
     */
    public int gensymIndex() {
        return gensymIndex++;
    }
}
