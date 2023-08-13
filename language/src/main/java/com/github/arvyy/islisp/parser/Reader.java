package com.github.arvyy.islisp.parser;

import com.github.arvyy.islisp.runtime.LispInteger;
import com.github.arvyy.islisp.runtime.Pair;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.Value;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class Reader {

    private final Lexer lexer;

    public Reader(java.io.Reader reader) {
        lexer = new ISLISPLexer(reader);
    }

    public List<Value> readAll() {
        var lst = new ArrayList<Value>();
        Optional<Value> maybeValue = readSingle();
        while (maybeValue.isPresent()) {
            lst.add(maybeValue.get());
            maybeValue = readSingle();
        }
        return lst;
    }

    public Optional<Value> readSingle() {
        Optional<Token> maybeT = lexer.getToken();
        if (maybeT.isEmpty())
            return Optional.empty();
        var t = maybeT.get();
        if (t instanceof Token.IdentifierToken)
            return Optional.of(new Symbol(((Token.IdentifierToken) t).identifier()));
        if (t instanceof Token.ExactNumberToken) {
            return Optional.of(new LispInteger(((Token.ExactNumberToken) t).value()));
        }
        /*
        if (t instanceof Token.VectorBracketOpenToken) {
            Optional<Token> next;
            var lst = new ArrayList<Value>();
            while (true) {
                next = lexer.peekToken();
                if (next.isEmpty())
                    throw new RuntimeException("Premature end of file");
                var token = next.get();
                if (token instanceof Token.BracketCloseToken) {
                    lexer.getToken();
                    return Optional.of(new Value.ImmutableVector(lst));
                }
                lst.add(readSingle().orElseThrow()); //TODO
            }
        }
         */
        if (t instanceof Token.BracketOpenToken) {
            Optional<Token> next;
            var lst = new ArrayList<Value>();
            while (true) {
                next = lexer.peekToken();
                if (next.isEmpty())
                    throw new RuntimeException("Premature end of file");
                var token = next.get();
                if (token instanceof Token.BracketCloseToken) {
                    lexer.getToken();
                    if (lst.isEmpty()) {
                        return Optional.of(Symbol.NIL);
                    } else {
                        Value tail = Symbol.NIL;
                        for (var i = lst.size() - 1; i >= 0; i--) {
                            tail = new Pair(lst.get(i), tail);
                        }
                        return Optional.of(tail);
                    }
                }
                lst.add(readSingle().orElseThrow()); //TODO
            }
        }
        return Optional.empty();
    }
}
