package com.github.arvyy.islisp.parser;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

public class LexicalScope<Key, Value> {

    private final Map<Key, Value> myValues;
    private final LexicalScope<Key, Value> parent;

    public LexicalScope(LexicalScope<Key, Value> parent, Map<Key, Value> entries) {
        this.parent = parent;
        myValues = new HashMap<>(entries);
    }

    public LexicalScope() {
        this(null, Map.of());
    }

    public Optional<Value> get(Key key) {
        if (myValues.containsKey(key))
            return Optional.ofNullable(myValues.get(key));
        if (parent != null)
            return parent.get(key);
        return Optional.empty();
    }

}
