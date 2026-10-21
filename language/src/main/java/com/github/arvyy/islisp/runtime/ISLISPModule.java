package com.github.arvyy.islisp.runtime;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.ISLISPTruffleLanguage;
import com.github.arvyy.islisp.SetfTransformer;
import com.github.arvyy.islisp.parser.Declaration;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.api.strings.TruffleString;

import java.util.*;

/**
 * Encapsulate module's bindings.
 */
@ExportLibrary(InteropLibrary.class)
public class ISLISPModule implements TruffleObject {

    private final String name;
    private final List<ISLISPModule> importedModules;
    private final Set<Symbol> exports;
    private final Map<Symbol, LispFunction> globalFunctions;
    private final Map<Symbol, GenericFunctionDescriptor> genericFunctions;
    private final Map<Symbol, LispFunction> setfGlobalFunctions;
    private final Map<Symbol, GenericFunctionDescriptor> setfGenericFunctions;
    private final Map<Symbol, LispFunction> macros;
    private final Map<Symbol, LispClass> classes;
    private final Map<Symbol, ValueReference> dynamicVars;
    private final Map<Symbol, ValueReference> globalVars;
    private final Map<Symbol, SetfTransformer> setfTransformers;
    private final Map<Symbol, Map<Symbol, Object>> documentation;
    private final List<Declaration> declarations;


    private final Map<String, Object> visibleMembers;
    /**
     * Create empty module.
     *
     * @param name module name
     */
    public ISLISPModule(String name) {
        this.name = name;
        exports = new HashSet<>();
        importedModules = new ArrayList<>();
        globalFunctions = new HashMap<>();
        genericFunctions = new HashMap<>();
        setfGlobalFunctions = new HashMap<>();
        setfGenericFunctions = new HashMap<>();
        dynamicVars = new HashMap<>();
        macros = new HashMap<>();
        classes = new HashMap<>();
        setfTransformers = new HashMap<>();
        globalVars = new HashMap<>();
        declarations = new ArrayList<>();
        visibleMembers = new HashMap<>();
        documentation = new HashMap<>();
    }

    /**
     * Append import.
     *
     * @param m imported module.
     */
    public void addImport(ISLISPModule m) {
        importedModules.add(m);
        visibleMembers.put("module " + m.name, m);
    }

    /**
     * Append export.
     *
     * @param symbolReference exported symbol reference.
     */
    public void addExport(Symbol symbolReference) {
        exports.add(symbolReference);
    }

    /**
     * Add all bindings to exported set.
     */
    public void exportAll() {
        exports.addAll(globalFunctions.keySet());
        exports.addAll(genericFunctions.keySet());
        exports.addAll(setfGlobalFunctions.keySet());
        exports.addAll(setfGenericFunctions.keySet());
        exports.addAll(dynamicVars.keySet());
        exports.addAll(macros.keySet());
        exports.addAll(classes.keySet());
        exports.addAll(setfTransformers.keySet());
        exports.addAll(globalVars.keySet());
    }

    /**
     * Register global variable (mutable or immutable).
     *
     * @param symbolReference variable name
     * @param init initialization value
     * @param readonly if the variable is a constant
     * @param sourceSection associated source section
     */
    @CompilerDirectives.TruffleBoundary
    public void registerGlobalVar(Symbol symbolReference, Object init, boolean readonly, SourceSection sourceSection) {
        var v = new ValueReference(sourceSection);
        v.setValue(init);
        v.setReadOnly(readonly);
        globalVars.put(symbolReference, v);
        visibleMembers.put("var " + symbolReference.name(), v);
    }

    /**
     * Find global variable by name.
     *
     * @param symbolReference name
     * @return variable's value reference or null if not found
     */
    @CompilerDirectives.TruffleBoundary
    public ValueReference lookupGlobalVar(Symbol symbolReference) {
        if (globalVars.containsKey(symbolReference)) {
            return globalVars.get(symbolReference);
        }
        for (var module: importedModules) {
            if (module.globalVars.containsKey(symbolReference) && module.exports.contains(symbolReference)) {
                return module.globalVars.get(symbolReference);
            }
        }
        return null;
    }

    /**
     * Register setf transformer for a given name. When encountering `(setf (name form ...) value)`
     * the given transformer will be used to expand it into a non-setf expression.
     *
     * @param symbolReference transformer name.
     * @param transformer transformer.
     */
    @CompilerDirectives.TruffleBoundary
    public void registerSetfTransformer(Symbol symbolReference, SetfTransformer transformer) {
        setfTransformers.put(symbolReference, transformer);
    }

    /**
     * Find a setf transformer for a given name.
     *
     * @param symbolReference transformer name.
     * @return setf transformer or null if undefined
     */
    @CompilerDirectives.TruffleBoundary
    public SetfTransformer lookupSetfTransformer(Symbol symbolReference) {
        if (setfTransformers.containsKey(symbolReference)) {
            return setfTransformers.get(symbolReference);
        }
        for (var module: importedModules) {
            if (module.setfTransformers.containsKey(symbolReference) && module.exports.contains(symbolReference)) {
                return module.setfTransformers.get(symbolReference);
            }
        }
        return null;
    }

    /**
     * Register dynamic variable.
     *
     * @param symbolReference variable name.
     * @param v value reference.
     */
    @CompilerDirectives.TruffleBoundary
    public void registerDynamicVar(Symbol symbolReference, ValueReference v) {
        dynamicVars.put(symbolReference, v);
        visibleMembers.put("dynvar " + symbolReference.name(), v);
    }

    /**
     * Find dynamic variable value reference for a given name.
     *
     * @param symbolReference dynamic variable name
     * @return dynamic variable value reference
     */
    @CompilerDirectives.TruffleBoundary
    public ValueReference lookupDynamicVar(Symbol symbolReference) {
        if (dynamicVars.containsKey(symbolReference)) {
            return dynamicVars.get(symbolReference);
        }
        for (var module: importedModules) {
            if (module.dynamicVars.containsKey(symbolReference) && module.exports.contains(symbolReference)) {
                return module.dynamicVars.get(symbolReference);
            }
        }
        return null;
    }

    /**
     * Register function into function namespace.
     *
     * @param symbolReference function name
     * @param function function value
     */
    @CompilerDirectives.TruffleBoundary
    public void registerFunction(Symbol symbolReference, LispFunction function) {
        globalFunctions.put(symbolReference, function);
        visibleMembers.put("fun " + symbolReference.name(), function);
    }

    /**
     * Find function by name. Function can be a generic function or plain.
     *
     * @param symbolReference function name
     * @return function or null if undefined
     */
    @CompilerDirectives.TruffleBoundary
    public LispFunction lookupFunction(Symbol symbolReference) {
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
    public LispFunction lookupFunction(Symbol symbolReference, boolean setf) {
        if (setf) {
            if (setfGlobalFunctions.containsKey(symbolReference)) {
                return setfGlobalFunctions.get(symbolReference);
            }
            for (var module: importedModules) {
                if (module.setfGlobalFunctions.containsKey(symbolReference)
                    && module.exports.contains(symbolReference)
                ) {
                    return module.setfGlobalFunctions.get(symbolReference);
                }
            }
            return null;
        } else {
            if (globalFunctions.containsKey(symbolReference)) {
                return globalFunctions.get(symbolReference);
            }
            for (var module: importedModules) {
                if (module.globalFunctions.containsKey(symbolReference) && module.exports.contains(symbolReference)) {
                    return module.globalFunctions.get(symbolReference);
                }
            }
            return null;
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
        Symbol symbolReference,
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
        visibleMembers.put("genfun " + symbolReference.name(), function);
    }

    /**
     * Find generic descriptor for a given generic function name.
     *
     * @param symbolReference generic function name
     * @param setf if the function is of setf form
     * @return generic function descriptor or null if undefined
     */
    @CompilerDirectives.TruffleBoundary
    public GenericFunctionDescriptor lookupGenericFunctionDispatchTree(Symbol symbolReference, boolean setf) {
        if (setf) {
            if (setfGenericFunctions.containsKey(symbolReference)) {
                return setfGenericFunctions.get(symbolReference);
            }
            for (var module: importedModules) {
                if (module.setfGenericFunctions.containsKey(symbolReference)
                    && module.exports.contains(symbolReference)
                ) {
                    return module.setfGenericFunctions.get(symbolReference);
                }
            }
        } else {
            if (genericFunctions.containsKey(symbolReference)) {
                return genericFunctions.get(symbolReference);
            }
            for (var module: importedModules) {
                if (module.genericFunctions.containsKey(symbolReference) && module.exports.contains(symbolReference)) {
                    return module.genericFunctions.get(symbolReference);
                }
            }
        }
        return null;
    }

    /**
     * Register macro with a given name.
     *
     * @param symbolReference macro name
     * @param function macro implementation function
     */
    @CompilerDirectives.TruffleBoundary
    public void registerMacro(Symbol symbolReference, LispFunction function) {
        macros.put(symbolReference, function);
        visibleMembers.put("macro " + symbolReference.name(), function);
    }

    /**
     * Find macro by given symbol reference name.
     *
     * @param symbolReference reference
     * @return macro function or null if undefined
     */
    @CompilerDirectives.TruffleBoundary
    public LispFunction lookupMacro(Symbol symbolReference) {
        if (macros.containsKey(symbolReference)) {
            return macros.get(symbolReference);
        }
        for (var module: importedModules) {
            if (module.macros.containsKey(symbolReference) && module.exports.contains(symbolReference)) {
                return module.macros.get(symbolReference);
            }
        }
        return null;
    }

    /**
     * Add a new class to context.
     *
     * @param symbolReference class name's reference
     * @param clazz class instance
     */
    @CompilerDirectives.TruffleBoundary
    public void registerClass(Symbol symbolReference, LispClass clazz) {
        classes.put(symbolReference, clazz);
        visibleMembers.put("class " + symbolReference.name(), clazz);
    }

    /**
     * Find a class by symbol reference.
     *
     * @param symbolReference class symbolic name's reference
     * @return class or null if doesn't exist
     */
    @CompilerDirectives.TruffleBoundary
    public LispClass lookupClass(Symbol symbolReference) {
        if (classes.containsKey(symbolReference)) {
            return classes.get(symbolReference);
        }
        for (var module: importedModules) {
            if (module.classes.containsKey(symbolReference) && module.exports.contains(symbolReference)) {
                return module.classes.get(symbolReference);
            }
        }
        return null;
    }

    /**
    * Sets documentation obj for given symbol and namespace.
    * @param namespace namespace
    * @param s symbol
    * @param doc documentation object
    * @return previously stored documentation object
    */
    @CompilerDirectives.TruffleBoundary
    public Object setDocumentation(Symbol namespace, Symbol s, Object doc) {
        Map<Symbol, Object> namespaceDocumentation;
        if (!documentation.containsKey(namespace)) {
            namespaceDocumentation = new HashMap<>();
            documentation.put(namespace, namespaceDocumentation);
        } else {
            namespaceDocumentation = documentation.get(namespace);
        }
        var prev = namespaceDocumentation.put(s, doc);
        if (prev == null) {
            return ISLISPContext.get(null).getNil();
        } else {
            return prev;
        }
    }

    /**
    * Finds documentation obj for given symbol and namespace.
    * @param namespace namespace
    * @param s symbol
    * @return documentation object or nil
    */
    @CompilerDirectives.TruffleBoundary
    public Object getDocumentation(Symbol namespace, Symbol s) {
        var ns = documentation.get(namespace);
        if (ns == null) {
            return ISLISPContext.get(null).getNil();
        }
        var doc = ns.get(s);
        if (doc == null) {
            return ISLISPContext.get(null).getNil();
        }
        return doc;
    }

    /**
    * Finds documentation obj for given symbol from any namespace.
    * @param s symbol
    * @return documentation object or nil
    */
    @CompilerDirectives.TruffleBoundary
    public Object getDocumentation(Symbol s) {
        for (var e: documentation.entrySet()) {
            var ns = e.getValue();
            var doc = ns.get(s);
            if (doc != null) {
                return doc;
            }
        }
        return ISLISPContext.get(null).getNil();
    }

    /**
    * Finds documentation obj for given symbol and namespace.
    * If not found searches in included modules.
    * @param namespace namespace
    * @param s symbol
    * @return documentation object or nil
    */
    @CompilerDirectives.TruffleBoundary
    public Object getDocumentationIncludeImports(Symbol namespace, Symbol s) {
        var nil = ISLISPContext.get(null).getNil();
        var mine = getDocumentation(namespace, s);
        if (mine != nil) {
            return mine;
        }
        for (var m: importedModules) {
            var doc = m.getDocumentation(namespace, s);
            if (doc != nil) {
                return doc;
            }
        }
        return nil;
    }

    /**
    * @return List of all documentation entries in this module.
    */
    @CompilerDirectives.TruffleBoundary
    public List<DocumentationEntry> getModuleDocumentations() {
        var lst = new ArrayList<DocumentationEntry>();
        for (var e1: documentation.entrySet()) {
            for (var e2: e1.getValue().entrySet()) {
                var e = new DocumentationEntry(e1.getKey(), e2.getKey(), e2.getValue());
                lst.add(e);
            }
        }
        Comparator<DocumentationEntry> c = (d1, d2) -> d1.symbol().name().compareTo(d2.symbol().name());
        c = c.thenComparing((d1, d2) -> d1.namespace().name().compareTo(d2.namespace().name()));
        lst.sort(c);
        return lst;
    }

    /**
    * Finds documentation obj for given symbol, from any namespace.
    * If not found searches in included modules.
    * @param s symbol
    * @return documentation object or nil
    */
    @CompilerDirectives.TruffleBoundary
    public Object getDocumentationIncludeImports(Symbol s) {
        var nil = ISLISPContext.get(null).getNil();
        var mine = getDocumentation(s);
        if (mine != nil) {
            return mine;
        }
        for (var m: importedModules) {
            var doc = m.getDocumentation(s);
            if (doc != nil) {
                return doc;
            }
        }
        return nil;
    }

    /**
     * Add list of declarations.
     *
     * @param decls list of declarations
     */
    @CompilerDirectives.TruffleBoundary
    public void addDeclarations(List<Declaration> decls) {
        declarations.addAll(decls);
    }

    /**
     * Check if given symbol had declarations for being inlined.
     *
     * @param symbolReference symbol for a function name
     * @return if definition for the function should mark it as inlined.
     */
    @CompilerDirectives.TruffleBoundary
    public boolean shouldInline(Symbol symbolReference) {
        for (var decl: declarations) {
            if (decl instanceof Declaration.Inline inline) {
                if (inline.name().equals(symbolReference)) {
                    return true;
                }
            }
        }
        return false;
    }

    @ExportMessage
    boolean isScope() {
        return true;
    }

    @ExportMessage
    boolean hasMembers() {
        return true;
    }

    @ExportMessage
    boolean hasLanguage() {
        return true;
    }

    @ExportMessage
    Class<ISLISPTruffleLanguage> getLanguage() {
        return ISLISPTruffleLanguage.class;
    }

    @CompilerDirectives.TruffleBoundary
    @ExportMessage
    String toDisplayString(boolean ignored) {
        return "Module " + name;
    }

    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    LispVector getMembers(boolean ignored) {
        var lst = new ArrayList<ISLISPModuleMemberString>(getMembersInternal(false));
        /*
        for (var m: importedModules) {
            lst.addAll(m.getMembersInternal(true));
        }
        */
        return new LispVector(lst.toArray(new ISLISPModuleMemberString[0]));
    }

    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    boolean isMemberReadable(String memberName) {
        return visibleMembers.containsKey(memberName);
    }

    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    Object readMember(String memberName) {
        var value = visibleMembers.get(memberName);
        if (value instanceof ValueReference r) {
            return r.getValue();
        }
        return value;
    }

    @CompilerDirectives.TruffleBoundary
    private List<ISLISPModuleMemberString> getMembersInternal(boolean exportedOnly) {
        var lst = new ArrayList<ISLISPModuleMemberString>();
        for (var e: visibleMembers.entrySet()) {
            var v = e.getValue();
            SourceSection src = null;
            if (v instanceof LispFunction f) {
                src = f.getSourceLocation();
            }
            if (v instanceof ValueReference r) {
                src = r.getSourceLocation();
            }
            if (v instanceof LispClass c) {
                src = c.getSourceLocation();
            }
            // TODO: module source section
            /*
            if (v instanceof ISLISPModule m) {
            }
            */
            lst.add(new ISLISPModuleMemberString(e.getKey(), src));
        }
        return lst;
        /*
        Predicate<Symbol> include = symbolName -> !exportedOnly || exports.contains(symbolName);
        var lst = new ArrayList<ISLISPModuleMemberString>();
        for (var e: globalFunctions.keySet()) {
            if (!include.test(e)) {
                continue;
            }
            lst.add(new ISLISPModuleMemberString(e.name(), globalFunctions.get(e).getSourceLocation()));
        }
        for (var e: macros.keySet()) {
            if (!include.test(e)) {
                continue;
            }
            lst.add(new ISLISPModuleMemberString(e.name(), macros.get(e).getSourceLocation()));
        }
        for (var e: setfGlobalFunctions.keySet()) {
            if (!include.test(e)) {
                continue;
            }
            lst.add(new ISLISPModuleMemberString(e.name(), setfGlobalFunctions.get(e).getSourceLocation()));
        }
        for (var e: classes.keySet()) {
            if (!include.test(e)) {
                continue;
            }
            lst.add(new ISLISPModuleMemberString(e.name(), classes.get(e).getSourceLocation()));
        }
        for (var e: dynamicVars.keySet()) {
            if (!include.test(e)) {
                continue;
            }
            lst.add(new ISLISPModuleMemberString(e.name(), dynamicVars.get(e).getSourceLocation()));
        }
        for (var e: globalVars.keySet()) {
            if (!include.test(e)) {
                continue;
            }
            lst.add(new ISLISPModuleMemberString(e.name(), globalVars.get(e).getSourceLocation()));
        }
        return lst;
        */
    }

    /**
    *  Data-bag for documentation entry.
    * @param namespace namespace
    * @param symbol name
    * @param doc documentation object
    */
    public static record DocumentationEntry(Symbol namespace, Symbol symbol, Object doc) { }

}

@ExportLibrary(InteropLibrary.class)
class ISLISPModuleMemberString implements TruffleObject {

    private final String name;
    private final SourceSection source;
    ISLISPModuleMemberString(String name, SourceSection source) {
        this.name = name;
        this.source = source;
    }

    @ExportMessage
    public boolean isString() {
        return true;
    }

    @ExportMessage
    public TruffleString asTruffleString() {
        return TruffleString.fromJavaStringUncached(name, TruffleString.Encoding.BYTES);
    }

    @ExportMessage
    public String asString() {
        return name;
    }

    @ExportMessage
    public boolean hasSourceLocation() {
        return source != null;
    }
    @ExportMessage
    public SourceSection getSourceLocation() {
        return source;
    }

}
