package com.github.arvyy.islisp;

import com.github.arvyy.islisp.functions.*;
import com.github.arvyy.islisp.nodes.ISLISPDefGenericExecutionNodeGen;
import com.github.arvyy.islisp.parser.ParsingException;
import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.TruffleLanguage.Env;
import com.oracle.truffle.api.nodes.Node;

import java.util.*;
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

    private final Map<String, ISLISPModule> modules;

    private final Map<SymbolReference, Map<SymbolReference, ValueReference>> symbolProperties;
    private final Map<String, SymbolReference> symbols;
    private final ValueReference currentOutputStream;
    private final ValueReference currentInputStream;
    private final ValueReference currentErrorStream;

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
        modules = new HashMap<>();
        modules.put("ROOT", new ISLISPModule());
        symbolProperties = new HashMap<>();
        symbols = new HashMap<>();
        currentOutputStream = new ValueReference();
        currentOutputStream.setValue(new LispStream(null, env.out()));
        currentInputStream = new ValueReference();
        currentInputStream.setValue(new LispStream(env.in(), null));
        currentErrorStream = new ValueReference();
        currentErrorStream.setValue(new LispStream(null, env.err()));
        initBuiltinVars();
        initBuiltinClasses();
        initGlobalFunctions();
        initSetfExpanders();
    }

    /**
     * Get module by name.
     *
     * @param module name
     * @return module instance, or null if not defined
     */
    @CompilerDirectives.TruffleBoundary
    public ISLISPModule getModule(String module) {
        return modules.get(module);
    }

    /**
     * Create a module and include it in the context.
     * All necessary module's dependency should already be loaded.
     *
     * @param module module name
     * @param requiredModules list of required modules, must exist.
     * @param exports list of exports.
     */
    public void createModule(String module, List<String> requiredModules, List<SymbolReference> exports) {
        if (modules.containsKey(module)) {
            throw new ParsingException(null, "Module already defined: " + module);
        }
        var m = new ISLISPModule();
        m.addImport(modules.get("ROOT"));
        for (var req: requiredModules) {
            if (!modules.containsKey(req)) {
                throw new ParsingException(null, "No module found: " + req);
            }
            m.addImport(modules.get(req));
        }
        for (var export: exports) {
            m.addExport(export);
        }
        modules.put(module, m);
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
        if (handlerChain == null) {
            return null;
        }
        var f = handlerChain.handler();
        handlerChain = handlerChain.rest();
        return f;
    }

    void initGlobalFunction(String name, Function<TruffleLanguage<?>, LispFunction> f) {
        modules.get("ROOT").registerFunction(namedSymbol(name).identityReference(), f.apply(language));
    }

    /**
     * Initialize builtin functions into function namespace storage.
     */
    void initGlobalFunctions() {
        // standard
        initGlobalFunction("+", ISLISPAdd::makeLispFunction);
        initGlobalFunction("-", ISLISPSubtract::makeLispFunction);
        initGlobalFunction("*", ISLISPMul::makeLispFunction);
        initGlobalFunction("=", ISLISPNumericEqual::makeLispFunction);
        initGlobalFunction(">", ISLISPNumericGt::makeLispFunction);
        initGlobalFunction("aref", ISLISPAref::makeLispFunction);
        initGlobalFunction("car", ISLISPCar::makeLispFunction);
        initGlobalFunction("cdr", ISLISPCdr::makeLispFunction);
        initGlobalFunction("ceiling", ISLISPRoundingFunctions::makeLispFunctionCeiling);
        initGlobalFunction("char=", ISLISPCharEqual::makeLispFunction);
        initGlobalFunction("char<", ISLISPCharLt::makeLispFunction);
        initGlobalFunction("class-of", ISLISPClassOf::makeLispFunction);
        initGlobalFunction("close", ISLISPClose::makeLispFunction);
        initGlobalFunction("cons", ISLISPCons::makeLispFunction);
        initGlobalFunction("continue-condition", ISLISPContinueCondition::makeLispFunction);
        initGlobalFunction("cos", ISLISPTrigFunctions::makeLispFunctionCos);
        initGlobalFunction("create-array", ISLISPCreateArray::makeLispFunction);
        initGlobalFunction("create-string", ISLISPCreateString::makeLispFunction);
        initGlobalFunction("create-string-input-stream", ISLISPCreateStringInputStream::makeLispFunction);
        initGlobalFunction("create-string-output-stream", ISLISPCreateStringOutputStream::makeLispFunction);
        initGlobalFunction("create-vector", ISLISPCreateVector::makeLispFunction);
        initGlobalFunction("elt", ISLISPElt::makeLispFunction);
        initGlobalFunction("error-output", ISLISPErrorOutputStream::makeLispFunction);
        initGlobalFunction("eq", ISLISPEq::makeLispFunction);
        initGlobalFunction("equal", ISLISPEqual::makeLispFunction);
        initGlobalFunction("eval", ISLISPEval::makeLispFunction);
        initGlobalFunction("file-position", ISLISPFilePosition::makeLispFunction);
        initGlobalFunction("finish-output", ISLISPFinishOutput::makeLispFunction);
        initGlobalFunction("floor", ISLISPRoundingFunctions::makeLispFunctionFloor);
        initGlobalFunction("format", ISLISPFormat::makeLispFunction);
        initGlobalFunction("format-char", ISLISPFormatChar::makeLispFunction);
        initGlobalFunction("format-integer", ISLISPFormatInteger::makeLispFunction);
        initGlobalFunction("format-float", ISLISPFormatFloat::makeLispFunction);
        initGlobalFunction("format-object", ISLISPFormatObject::makeLispFunction);
        initGlobalFunction("format-fresh-line", ISLISPFormatFreshLine::makeLispFunction);
        initGlobalFunction("gensym", ISLISPGensym::makeLispFunction);
        initGlobalFunction("get-internal-real-time", ISLISPGetInternalRealTime::makeLispFunction);
        initGlobalFunction("get-universal-time", ISLISPGetUniversalTime::makeLispFunction);
        initGlobalFunction("get-output-stream-string", ISLISPGetOutputStreamString::makeLispFunction);
        initGlobalFunction("input-stream-p", ISLISPInputStreamp::makeLispFunction);
        initGlobalFunction("instancep", ISLISPInstancep::makeLispFunction);
        initGlobalFunction("length", ISLISPLength::makeLispFunction);
        initGlobalFunction("list", ISLISPList::makeLispFunction);
        initGlobalFunction("mapcar", ISLISPMapcar::makeLispFunction);
        initGlobalFunction("mapc", ISLISPMapc::makeLispFunction);
        initGlobalFunction("mapcan", ISLISPMapcan::makeLispFunction);
        initGlobalFunction("mapcon", ISLISPMapcon::makeLispFunction);
        initGlobalFunction("maplist", ISLISPMaplist::makeLispFunction);
        initGlobalFunction("mapl", ISLISPMapl::makeLispFunction);
        initGlobalFunction("open-input-file", ISLISPOpenInputFile::makeLispFunction);
        initGlobalFunction("open-io-file", ISLISPOpenIOFile::makeLispFunction);
        initGlobalFunction("open-output-file", ISLISPOpenOutputFile::makeLispFunction);
        initGlobalFunction("output-stream-p", ISLISPOutputStreamp::makeLispFunction);
        initGlobalFunction("preview-char", ISLISPPreviewChar::makeLispFunction);
        initGlobalFunction("probe-file", ISLISPProbeFile::makeLispFunction);
        initGlobalFunction("property", ISLISPProperty::makeLispFunction);
        initGlobalFunction("quotient", ISLISPQuotient::makeLispFunction);
        initGlobalFunction("read", ISLISPRead::makeLispFunction);
        initGlobalFunction("read-byte", ISLISPReadByte::makeLispFunction);
        initGlobalFunction("read-char", ISLISPReadChar::makeLispFunction);
        initGlobalFunction("read-line", ISLISPReadLine::makeLispFunction);
        initGlobalFunction("remove-property", ISLISPRemoveProperty::makeLispFunction);
        initGlobalFunction("round", ISLISPRoundingFunctions::makeLispFunctionRound);
        initGlobalFunction("set-aref", ISLISPSetAref::makeLispFunction);
        initGlobalFunction("set-car", ISLISPSetCar::makeLispFunction);
        initGlobalFunction("set-cdr", ISLISPSetCdr::makeLispFunction);
        initGlobalFunction("set-elt", ISLISPSetElt::makeLispFunction);
        initGlobalFunction("set-file-position", ISLISPSetFilePosition::makeLispFunction);
        initGlobalFunction("set-property", ISLISPSetProperty::makeLispFunction);
        initGlobalFunction("sin", ISLISPTrigFunctions::makeLispFunctionSin);
        initGlobalFunction("signal-condition", ISLISPSignalCondition::makeLispFunction);
        initGlobalFunction("standard-output", ISLISPStandardOutputStream::makeLispFunction);
        initGlobalFunction("standard-input", ISLISPStandardInputStream::makeLispFunction);
        initGlobalFunction("tan", ISLISPTrigFunctions::makeLispFunctionTan);
        initGlobalFunction("truncate", ISLISPRoundingFunctions::makeLispFunctionTruncate);
        initGlobalFunction("error-output", ISLISPErrorOutputStream::makeLispFunction);
        initGlobalFunction("subclassp", ISLISPSubclassp::makeLispFunction);
        initGlobalFunction("vector", ISLISPVector::makeLispFunction);
        initGlobalFunction("write-byte", ISLISPWriteByte::makeLispFunction);
        initCreateMethod();
        initInitializeObjectMethod();

        //extension
        initGlobalFunction("current-stacktrace", ISLISPCurrentStacktrace::makeLispFunction);
        initGlobalFunction("exit", ISLISPExit::makeLispFunction);
        initGlobalFunction("truffle-object-fields", ISLISPTruffleObjectFields::makeLispFunction);
        initGlobalFunction("truffle-object-field", ISLISPTruffleObjectField::makeLispFunction);
        initGlobalFunction("set-truffle-object-field", ISLISPSetTruffleObjectField::makeLispFunction);
        initGlobalFunction("load-native-library", ISLISPLoadNativeLibrary::makeLispFunction);
        initGlobalFunction("native-library-symbol", ISLISPNativeLibrarySymbol::makeLispFunction);
        initGlobalFunction("closed-p", ISLISPClosedp::makeLispFunction);
        initGlobalFunction("delete-file", ISLISPDeleteFile::makeLispFunction);
    }

    private void initInitializeObjectMethod() {
        var object = modules.get("ROOT").lookupClass(namedSymbol("<object>").identityReference());
        var initializeObjectDescriptor = new GenericFunctionDescriptor(1, true);
        initializeObjectDescriptor.addPrimaryMethod(
            new LispClass[] {object},
            ISLISPInitializeObject.makeLispFunction(language).callTarget(),
            null);
        var initializeObjectExecutionNode = ISLISPDefGenericExecutionNodeGen.create(
            "ROOT",
            namedSymbol("initialize-object"),
            false,
            getLanguage(),
            null);
        registerGenericFunction(
            "ROOT",
            namedSymbol("initialize-object").identityReference(),
            false,
            new LispFunction(initializeObjectExecutionNode.getCallTarget()),
            initializeObjectDescriptor);
    }

    private void initCreateMethod() {
        var stdClass = modules.get("ROOT").lookupClass(namedSymbol("<standard-class>").identityReference());
        var createDescriptor = new GenericFunctionDescriptor(1, true);
        createDescriptor.addPrimaryMethod(
                new LispClass[] {stdClass},
                ISLISPCreateStandardClassObject.makeLispFunction(language).callTarget(),
                null);
        var createExecutionNode = ISLISPDefGenericExecutionNodeGen.create(
            "ROOT",
            namedSymbol("create"),
            false,
            getLanguage(),
            null);
        registerGenericFunction(
            "ROOT",
            namedSymbol("create").identityReference(),
            false,
            new LispFunction(createExecutionNode.getCallTarget()),
            createDescriptor);
    }

    void initSetfExpanders() {
        // standard
        initBasicSetfExpander("car", "set-car");
        initBasicSetfExpander("cdr", "set-cdr");
        initBasicSetfExpander("property", "set-property");
        initBasicSetfExpander("aref", "set-aref");
        initBasicSetfExpander("garef", "set-garef");
        initBasicSetfExpander("dynamic", "set-dynamic");
        initBasicSetfExpander("elt", "set-elt");

        //extensions
        initBasicSetfExpander("truffle-object-field", "set-truffle-object-field");
    }

    void initBasicSetfExpander(String setfForm, String expandedForm) {
        SetfTransformer transformer = (forms, value) -> {
            var lst = new ArrayList<>();
            lst.add(namedSymbol(expandedForm));
            lst.add(value);
            lst.addAll(forms.subList(1, forms.size()));
            return Utils.listToValue(lst);
        };
        modules.get("ROOT").registerSetfTransformer(namedSymbol(setfForm).identityReference(), transformer);
    }

    void initBuiltin(String name, String... parents) {
        var root = modules.get("ROOT");
        var symbol = namedSymbol(name);
        var parentClasses = Arrays.stream(parents)
                .map(pname -> root.lookupClass(namedSymbol(pname).identityReference()))
                .toList();
        root.registerClass(symbol.identityReference(), new BuiltinClass(parentClasses, symbol, false));
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
        initBuiltin("<cons>", "<list>");
        initBuiltin("<integer>", "<number>");
        initBuiltin("<float>", "<number>");
        initBuiltin("<built-in-class>", "<object>");
        initBuiltin("<basic-array>", "<object>");
        initBuiltin("<basic-vector>", "<basic-array>");
        initBuiltin("<string>", "<basic-vector>");
        initBuiltin("<general-vector>", "<basic-vector>");
        initBuiltin("<basic-array*>", "<basic-array>");
        initBuiltin("<general-array*>", "<basic-array*>");
        initBuiltin("<stream>", "<object>");
        initBuiltin("<character>", "<object>");

        //truffle interop
        initBuiltin("<truffle-object>", "<object>");
        initBuiltin("<truffle-native-library>", "<object>");
        initBuiltin("<truffle-vector>", "<basic-vector>");
    }

    /**
     * Initialize builtin constants.
     */
    void initBuiltinVars() {
        modules.get("ROOT").registerGlobalVar(getNil().identityReference(), getNil(), true);
        modules.get("ROOT").registerGlobalVar(getT().identityReference(), getT(), true);
    }

    /**
     * Register global variable (mutable or immutable).
     *
     * @param module module name holding the binding
     * @param symbolReference variable name
     * @param init initialization value
     * @param readonly if the variable is a constant
     */
    @CompilerDirectives.TruffleBoundary
    public void registerGlobalVar(String module, SymbolReference symbolReference, Object init, boolean readonly) {
        modules.get(module).registerGlobalVar(symbolReference, init, readonly);
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
     * @param module module name holding the binding
     * @param symbolReference name
     * @return variable's value reference or null if not found
     */
    @CompilerDirectives.TruffleBoundary
    public ValueReference lookupGlobalVar(String module, SymbolReference symbolReference) {
        return modules.get(module).lookupGlobalVar(symbolReference);
    }

    /**
     * Register setf transformer for a given name. When encountering `(setf (name form ...) value)`
     * the given transformer will be used to expand it into a non-setf expression.
     *
     * @param module module name holding the binding
     * @param symbolReference transformer name.
     * @param transformer transformer.
     */
    @CompilerDirectives.TruffleBoundary
    public void registerSetfTransformer(String module, SymbolReference symbolReference, SetfTransformer transformer) {
        modules.get(module).registerSetfTransformer(symbolReference, transformer);
    }

    /**
     * Find a setf transformer for a given name.
     *
     * @param module module name holding the binding
     * @param symbolReference transformer name.
     * @return setf transformer or null if undefined
     */
    @CompilerDirectives.TruffleBoundary
    public SetfTransformer lookupSetfTransformer(String module, SymbolReference symbolReference) {
        return modules.get(module).lookupSetfTransformer(symbolReference);
    }

    /**
     * Register dynamic variable.
     *
     * @param module module name holding the binding
     * @param symbolReference variable name.
     * @param v value reference.
     */
    @CompilerDirectives.TruffleBoundary
    public void registerDynamicVar(String module, SymbolReference symbolReference, ValueReference v) {
        modules.get(module).registerDynamicVar(symbolReference, v);
    }

    /**
     * Find dynamic variable value reference for a given name.
     *
     * @param module module name holding the binding
     * @param symbolReference dynamic variable name
     * @return dynamic variable value reference
     */
    @CompilerDirectives.TruffleBoundary
    public ValueReference lookupDynamicVar(String module, SymbolReference symbolReference) {
        return modules.get(module).lookupDynamicVar(symbolReference);
    }

    /**
     * Register function into function namespace.
     *
     * @param module module name holding the binding
     * @param symbolReference function name
     * @param function function value
     */
    @CompilerDirectives.TruffleBoundary
    public void registerFunction(String module, SymbolReference symbolReference, LispFunction function) {
        modules.get(module).registerFunction(symbolReference, function);
    }

    /**
     * Find function by name. Function can be a generic function or plain.
     *
     * @param module module name holding the binding
     * @param symbolReference function name
     * @return function or null if undefined
     */
    @CompilerDirectives.TruffleBoundary
    public LispFunction lookupFunction(String module, SymbolReference symbolReference) {
        return lookupFunction(module, symbolReference, false);
    }

    /**
     * Find function by name. Function can be a generic function or plain.
     *
     * @param module module name holding the binding
     * @param symbolReference function name
     * @param setf whether function is of setf form in case it's generic.
     * @return function or null if undefined
     */
    @CompilerDirectives.TruffleBoundary
    public LispFunction lookupFunction(String module, SymbolReference symbolReference, boolean setf) {
        return modules.get(module).lookupFunction(symbolReference, setf);
    }

    /**
     * Register a generic function.
     *
     * @param module module name holding the binding
     * @param symbolReference function name
     * @param setf whether it's of setf form
     * @param function function call entrypoint function implementation
     * @param descriptor generic descriptor
     */
    @CompilerDirectives.TruffleBoundary
    public void registerGenericFunction(
            String module,
            SymbolReference symbolReference,
            boolean setf,
            LispFunction function,
            GenericFunctionDescriptor descriptor
    ) {
        modules.get(module).registerGenericFunction(symbolReference, setf, function, descriptor);
    }

    /**
     * Find generic descriptor for a given generic function name.
     *
     * @param module the module where to look for the binding
     * @param symbolReference generic function name
     * @param setf if the function is of setf form
     * @return generic function descriptor or null if undefined
     */
    @CompilerDirectives.TruffleBoundary
    public GenericFunctionDescriptor lookupGenericFunctionDispatchTree(
        String module,
        SymbolReference symbolReference,
        boolean setf
    ) {
        return modules.get(module).lookupGenericFunctionDispatchTree(symbolReference, setf);
    }

    /**
     * Register macro with a given name.
     *
     * @param module the module where to look for the binding
     * @param symbolReference macro name
     * @param function macro implementation function
     */
    @CompilerDirectives.TruffleBoundary
    public void registerMacro(String module, SymbolReference symbolReference, LispFunction function) {
        modules.get(module).registerMacro(symbolReference, function);
    }

    /**
     * Find macro by given symbol reference name.
     *
     * @param module the module where to look for the binding
     * @param symbolReference reference
     * @return macro function or null if undefined
     */
    @CompilerDirectives.TruffleBoundary
    public LispFunction lookupMacro(String module, SymbolReference symbolReference) {
        return modules.get(module).lookupMacro(symbolReference);
    }

    /**
     * Add a new class to context.
     *
     * @param module the module where to look for the binding
     * @param symbolReference class name's reference
     * @param clazz class instance
     */
    @CompilerDirectives.TruffleBoundary
    public void registerClass(String module, SymbolReference symbolReference, LispClass clazz) {
        modules.get(module).registerClass(symbolReference, clazz);
    }

    /**
     * Find a class by symbol reference.
     *
     * @param module the module where to look for the binding
     * @param symbolReference class symbolic name's reference
     * @return class or null if doesn't exist
     */
    @CompilerDirectives.TruffleBoundary
    public LispClass lookupClass(String module, SymbolReference symbolReference) {
        return modules.get(module).lookupClass(symbolReference);
    }

    /**
     * Find a class by string name.
     *
     * @param module module name holding the binding
     * @param name class name
     * @return class or null if doesn't exit
     */
    @CompilerDirectives.TruffleBoundary
    public LispClass lookupClass(String module, String name) {
        return lookupClass(module, namedSymbol(name).identityReference());
    }

    /**
     * Find root class by name.
     *
     * @param name class name
     * @return class instance or null
     */
    public LispClass lookupClass(String name) {
        return lookupClass("ROOT", name);
    }


    /**
     * Find root class by symbol.
     *
     * @param ref class name's symbol reference
     * @return class instance or null
     */
    public LispClass lookupClass(SymbolReference ref) {
        return lookupClass("ROOT", ref);
    }

    /**
     * @return currently active output stream reference.
     */
    public ValueReference currentOutputStreamReference() {
        return currentOutputStream;
    }

    /**
     * @return currently active input stream reference.
     */
    public ValueReference currentInputStreamReference() {
        return currentInputStream;
    }

    /**
     * @return currently active input stream reference.
     */
    public ValueReference currentErrorStreamReference() {
        return currentErrorStream;
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
            CompilerDirectives.transferToInterpreterAndInvalidate();
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
            CompilerDirectives.transferToInterpreterAndInvalidate();
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
