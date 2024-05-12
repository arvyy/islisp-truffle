package com.github.arvyy.islisp.test;

import com.github.arvyy.islisp.parser.Lexer;
import com.github.arvyy.islisp.parser.LexerSourceFromReader;
import com.github.arvyy.islisp.parser.Token;
import com.github.arvyy.islisp.parser.TokenWithSource;
import org.junit.jupiter.api.Test;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.math.BigInteger;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class LexerTest {

    @Test
    public void testLexemes() throws IOException {
        var input = """
            ( ) ab . "str" 12 #( #' ' ` , ,@
            """;
        var lexer = new Lexer(new LexerSourceFromReader(new BufferedReader(new StringReader(input)), null));
        assertEquals(
            lexer.readToken(),
            Optional.of(new TokenWithSource(new Token.BracketOpenToken(), 1, 1, 1, 1))
        );
        assertEquals(
            lexer.readToken(),
            Optional.of(new TokenWithSource(new Token.BracketCloseToken(), 1, 3, 1, 3))
        );
        assertEquals(
            lexer.readToken(),
            Optional.of(new TokenWithSource(new Token.IdentifierToken("ab"), 1, 5, 1, 6))
        );
        assertEquals(
            lexer.readToken(),
            Optional.of(new TokenWithSource(new Token.PeriodToken(), 1, 8, 1, 8))
        );
        assertEquals(
            lexer.readToken(),
            Optional.of(new TokenWithSource(new Token.StringToken("str"), 1, 10, 1, 14))
        );
        assertEquals(
            lexer.readToken(),
            Optional.of(new TokenWithSource(new Token.ExactNumberToken(BigInteger.valueOf(12)), 1, 16, 1, 17))
        );
        assertEquals(
            lexer.readToken(),
            Optional.of(new TokenWithSource(new Token.VectorBracketOpenToken(), 1, 19, 1, 20))
        );
        assertEquals(
            lexer.readToken(),
            Optional.of(new TokenWithSource(new Token.FunctionRefToken(), 1, 22, 1, 23))
        );
        assertEquals(
            lexer.readToken(),
            Optional.of(new TokenWithSource(new Token.QuoteToken(), 1, 25, 1, 25))
        );
        assertEquals(
            lexer.readToken(),
            Optional.of(new TokenWithSource(new Token.QuasiquoteToken(), 1, 27, 1, 27))
        );
        assertEquals(
            lexer.readToken(),
            Optional.of(new TokenWithSource(new Token.UnquoteToken(), 1, 29, 1, 29))
        );
        assertEquals(
            lexer.readToken(),
            Optional.of(new TokenWithSource(new Token.UnquoteSpliceToken(), 1, 31, 1, 32))
        );
    }

    @Test
    public void testWhitespace() throws IOException {

        var input = """
            ;; ignore1 #|
            1 #|
            ;; #|
            ignore2
            |# ignore3 
            |# 2
            """;
        var lexer = new Lexer(new LexerSourceFromReader(new BufferedReader(new StringReader(input)), null));
        assertEquals(
            lexer.readToken(),
            Optional.of(new TokenWithSource(new Token.ExactNumberToken(BigInteger.valueOf(1)), 2, 1, 2, 1))
        );
        assertEquals(
            lexer.readToken(),
            Optional.of(new TokenWithSource(new Token.ExactNumberToken(BigInteger.valueOf(2)), 6, 4, 6, 4))
        );
    }

}
