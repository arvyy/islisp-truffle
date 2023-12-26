package com.github.arvyy.islisp.parser;

import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.SymbolReference;

import java.util.List;

public record ModuleSource(
    String name,
    List<String> requires,
    List<SymbolReference> provides,
    List<Object> content
) { }
