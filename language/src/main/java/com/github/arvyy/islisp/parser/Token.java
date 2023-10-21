package com.github.arvyy.islisp.parser;

/**
 * Possible values to be returned from Lexer.
 */
public sealed interface Token {

    record ExactNumberToken(int value) implements Token { }
    record InexactNumberToken(double value) implements Token { }

    record FunctionRefToken() implements Token { }
    record StringToken(String value) implements Token { }
    record CharToken(Integer value) implements Token { }
    record BooleanToken(Boolean value) implements Token { }
    record IdentifierToken(String identifier) implements Token { }
    record QuoteToken() implements Token { }
    record VectorBracketOpenToken() implements Token { }
    record ByteVectorBracketOpenToken() implements Token { }
    record BracketOpenToken() implements Token { }
    record BracketCloseToken() implements Token { }
    record QuasiquoteToken() implements Token { }
    record UnquoteToken() implements Token { }
    record UnquoteSpliceToken() implements Token { }
    record PeriodToken() implements Token { }
    record StatementCommentBracketOpenToken() implements Token { }

}
