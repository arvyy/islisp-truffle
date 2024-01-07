package com.github.arvyy.islisp.parser;

import com.github.arvyy.islisp.runtime.SymbolReference;
import com.oracle.truffle.api.source.SourceSection;

import java.util.List;

/**
 * Module with unparsed code and necessary import/export metadata.
 *
 * @param name name of the module
 * @param sourceSection associated source section
 * @param requires list of required modules
 * @param provides list of exports
 * @param content unparsed sexprs of module's content
 */
public record ModuleSource(
    String name,
    SourceSection sourceSection,
    List<String> requires,
    List<SymbolReference> provides,
    List<Object> content
) { }
