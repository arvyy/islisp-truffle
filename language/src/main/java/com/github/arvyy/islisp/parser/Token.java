package com.github.arvyy.islisp.parser;

/**
 * Possible values to be returned from Lexer.
 */
public sealed interface Token {

    /**
     * Exact number literal.
     * @param value
     */
    record ExactNumberToken(int value) implements Token { }

    /**
     * Inexact number literal.
     * @param value
     */
    record InexactNumberToken(double value) implements Token { }

    /**
     *  '# token.
     */
    record FunctionRefToken() implements Token { }

    /**
     * String literal.
     * @param value
     */
    record StringToken(String value) implements Token { }

    /**
     * Char literal.
     * @param value
     */
    record CharToken(Integer value) implements Token { }

    /**
     * Identifier.
     * @param identifier
     */
    record IdentifierToken(String identifier) implements Token { }

    /**
     * Quote (').
     */
    record QuoteToken() implements Token { }

    /**
     * Vector opener #(.
     */
    record VectorBracketOpenToken() implements Token { }

    /**
     * Simple bracket open (.
     */
    record BracketOpenToken() implements Token { }

    /**
     * Bracket close ).
     */
    record BracketCloseToken() implements Token { }

    /**
     * Quasiquote `.
     */
    record QuasiquoteToken() implements Token { }

    /**
     * Unquote ,.
     */
    record UnquoteToken() implements Token { }

    /**
     * Unquote splicitn ,@.
     */
    record UnquoteSpliceToken() implements Token { }

    /**
     * Period ..
     */
    record PeriodToken() implements Token { }

}
