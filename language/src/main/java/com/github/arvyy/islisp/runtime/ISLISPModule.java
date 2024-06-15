package com.github.arvyy.islisp.runtime;

import com.github.arvyy.islisp.SetfTransformer;
import com.github.arvyy.islisp.parser.Declaration;
import com.oracle.truffle.api.CompilerDirectives;

import java.util.*;

/**
 * Encapsulate module's bindings.
 */
public class ISLISPModule {

    private final List<ISLISPModule> importedModules;
    private final Set<SymbolReference> exports;
    private final Map<SymbolReference, LispFunction> globalFunctions;
    private final Map<SymbolReference, GenericFunctionDescriptor> genericFunctions;
    private final Map<SymbolReference, LispFunction> setfGlobalFunctions;
    private final Map<SymbolReference, GenericFunctionDescriptor> setfGenericFunctions;
    private final Map<SymbolReference, LispFunction> macros;
    private final Map<SymbolReference, LispClass> classes;
    private final Map<SymbolReference, ValueReference> dynamicVars;
    private final Map<SymbolReference, ValueReference> globalVars;
    private final Map<SymbolReference, SetfTransformer> setfTransformers;
    private final List<Declaration> declarations;

    /**
     * Create empty module.
     */
    public ISLISPModule() {
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
    }

    /**
     * Append import.
     *
     * @param m imported module.
     */
    public void addImport(ISLISPModule m) {
        importedModules.add(m);
    }

    /**
     * Append export.
     *
     * @param symbolReference exported symbol reference.
     */
    public void addExport(SymbolReference symbolReference) {
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
     */
    @CompilerDirectives.TruffleBoundary
    public void registerGlobalVar(SymbolReference symbolReference, Object init, boolean readonly) {
        var v = new ValueReference();
        v.setValue(init);
        v.setReadOnly(readonly);
        globalVars.put(symbolReference, v);
    }

    /**
     * Find global variable by name.
     *
     * @param symbolReference name
     * @return variable's value reference or null if not found
     */
    @CompilerDirectives.TruffleBoundary
    public ValueReference lookupGlobalVar(SymbolReference symbolReference) {
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
    public boolean shouldInline(SymbolReference symbolReference) {
        for (var decl: declarations) {
            if (decl instanceof Declaration.Inline inline) {
                if (inline.name().equals(symbolReference)) {
                    return true;
                }
            }
        }
        return false;
    }
}
