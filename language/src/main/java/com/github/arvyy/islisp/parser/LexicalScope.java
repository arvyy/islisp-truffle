package com.github.arvyy.islisp.parser;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

/**
 * A hierarchical Map, wherein if value is missing in a leaf node, it tries recursively look
 * up in parent. Useful for lexical scope implementation concerns. The class is immutable, therefore
 * pushing is done through creating new value that references parent instance, and pop'ing the leaf Map is done
 * by letting it go out of scope and continuing on using parent reference.
 *
 * @param <Key> Map key type
 * @param <Value> Map value type
 */
public class LexicalScope<Key, Value> {

    private final Map<Key, Value> myValues;
    private final LexicalScope<Key, Value> parent;

    /**
     * Create new lexical scope chain.
     *
     * @param parent parent scope
     * @param entries new leaf's scope entries
     */
    public LexicalScope(LexicalScope<Key, Value> parent, Map<Key, Value> entries) {
        this.parent = parent;
        myValues = new HashMap<>(entries);
    }

    /**
     * Create root empty scope.
     */
    public LexicalScope() {
        this(null, Map.of());
    }

    /**
     * Find value, starting from this scope and recursively searching in parent scope.
     *
     * @param key mapping key value
     * @return current active value for the given key
     */
    public Optional<Value> get(Key key) {
        if (myValues.containsKey(key)) {
            return Optional.ofNullable(myValues.get(key));
        }
        if (parent != null) {
            return parent.get(key);
        }
        return Optional.empty();
    }

}
