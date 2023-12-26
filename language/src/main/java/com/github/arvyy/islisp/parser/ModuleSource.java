package com.github.arvyy.islisp.parser;

import com.github.arvyy.islisp.runtime.SymbolReference;

import java.util.List;

/**
 * Module with unparsed code and necessary import/export metadata.
 *
 * @param name name of the module
 * @param requires list of required modules
 * @param provides list of exports
 * @param content unparsed sexprs of module's content
 */
public record ModuleSource(
    String name,
    List<String> requires,
    List<SymbolReference> provides,
    List<Object> content
) { }
