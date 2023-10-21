package com.github.arvyy.islisp.parser;

import java.util.Optional;

/**
 * ISLISP lexer, returns a token one by one. Implementation is done through
 * jflex code generation.
 */
public interface Lexer {

    /**
     * Lookup following token without consuming it.
     *
     * @return lexer token
     */
    Optional<Token> peekToken();

    /**
     * Lookup following token, consuming it.
     *
     * @return lexer token
     */
    Optional<Token> getToken();

    /**
     * Length of text in source input corresponding to recently return token.
     *
     * @return length of text in source
     */
    int getLength();

    /**
     * Get line information about recently returned token.
     *
     * @return line number
     */
    int getLine();

    /**
     * Get column information about recently return token.
     *
     * @return column number
     */
    int getColumn();
}
