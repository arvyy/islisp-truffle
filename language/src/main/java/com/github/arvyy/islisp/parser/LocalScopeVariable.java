package com.github.arvyy.islisp.parser;

/**
 * Variable definition to be used when display debugger scope to user.
 *
 * @param name visual var representation
 * @param frameIndex how many frames to loop back for variable's slot.
 * @param slot slot index in the appropriate frame
 */
public record LocalScopeVariable(String name, int frameIndex, int slot) { }
