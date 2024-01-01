package com.github.arvyy.islisp.parser;

/**
 * Combines token with its source information.
 *
 * @param token
 * @param startLine
 * @param startColumn
 * @param endLine
 * @param endColumn
 */
public record TokenWithSource(
    Token token,
    int startLine,
    int startColumn,
    int endLine,
    int endColumn
) { }
