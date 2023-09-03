package com.github.arvyy.islisp.parser;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class Reader {

    private final Lexer lexer;
    private final Source source;

    public Reader(Source source) {
        this.source = source;
        lexer = new ISLISPLexer(source.getReader());
    }

    SourceSection section() {
        return source.createSection(lexer.getLine(), lexer.getColumn(), lexer.getLength());
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
        if (t instanceof Token.IdentifierToken) {
            var identifier = ((Token.IdentifierToken) t).identifier();
            var symbol = ISLISPContext.get(null).namedSymbol(identifier);
            var symbolWithSource = new Symbol(symbol.name(), symbol.identityReference(), section());
            return Optional.of(symbolWithSource);
        }
        if (t instanceof Token.ExactNumberToken) {
            var value = ((Token.ExactNumberToken) t).value();
            var lispInt = new LispInteger(value, section());
            return Optional.of(lispInt);
        }
        if (t instanceof Token.QuasiquoteToken ||
                t instanceof Token.QuoteToken ||
                t instanceof Token.UnquoteSpliceToken ||
                t instanceof Token.UnquoteToken ||
                t instanceof Token.FunctionRefToken
        ) {
            String symbolName;
            if (t instanceof Token.QuasiquoteToken) {
                symbolName = "quasiquote";
            } else if (t instanceof Token.QuoteToken) {
                symbolName = "quote";
            } else if (t instanceof Token.UnquoteSpliceToken) {
                symbolName = "unquote-splicing";
            } else if (t instanceof Token.FunctionRefToken) {
                symbolName = "function";
            } else {
                symbolName = "unquote";
            }
            var quoteSection = section();
            var value = readSingle().orElseThrow(); //TODO
            var normalizedSyntaxSymbol = ISLISPContext.get(null).namedSymbol(symbolName);
            var nil = ISLISPContext.get(null).getNIL();
            var fullSection = source.createSection(quoteSection.getStartLine(), quoteSection.getStartColumn(), value.sourceSection().getEndLine(), value.sourceSection().getEndColumn());
            return Optional.of(new Pair(normalizedSyntaxSymbol, new Pair(value, nil, value.sourceSection()), fullSection));
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
            var startLine = lexer.getLine();
            var startColumn = lexer.getColumn();
            Optional<Token> next;
            var lst = new ArrayList<Value>();
            while (true) {
                next = lexer.peekToken();
                if (next.isEmpty())
                    throw new RuntimeException("Premature end of file");
                var token = next.get();
                if (token instanceof Token.BracketCloseToken) {
                    lexer.getToken();
                    var endLine = lexer.getLine();
                    var endColumn = lexer.getColumn();
                    var section = source.createSection(startLine, startColumn, endLine, endColumn);
                    if (lst.isEmpty()) {
                        var nil = ISLISPContext.get(null).getNIL();
                        var nilWithPos = new Symbol(nil.name(), nil.identityReference(), section);
                        return Optional.of(nilWithPos);
                    } else {
                        Value tail = ISLISPContext.get(null).getNIL();
                        for (var i = lst.size() - 1; i >= 0; i--) {
                            tail = new Pair(lst.get(i), tail, null);
                        }
                        var parsedTail = (Pair) tail;
                        var finalList = new Pair(parsedTail.car(), parsedTail.cdr(), section);
                        return Optional.of(finalList);
                    }
                }
                lst.add(readSingle().orElseThrow()); //TODO
            }
        }
        if (t instanceof Token.CharToken c) {
            return Optional.of(new LispChar(c.value(), section()));
        }
        return Optional.empty();
    }
}
