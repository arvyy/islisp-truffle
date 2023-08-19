package com.github.arvyy.islisp.parser;

import java.util.Optional;

public interface Lexer {
    Optional<Token> peekToken();
    Optional<Token> getToken();

    int getLength();
    int getLine();
    int getColumn();
}