package com.github.arvyy.islisp.parser;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;

import java.io.BufferedReader;
import java.io.EOFException;
import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * Reader using lexer's token input and forms
 * sexprs to be parsed from.
 */
//TODO rename to not clash with java.io.reader
public class Reader {

    private final Lexer lexer;
    private final Source source;
    private TokenWithSource peekedToken;

    private TokenWithSource lastToken;

    /**
     * Create reader from given source.
     *
     * @param source source to read from
     */
    @CompilerDirectives.TruffleBoundary
    public Reader(Source source) {
        this.source = source;
        BufferedReader bufferedReader;
        if (source.getReader() instanceof BufferedReader br) {
            bufferedReader = br;
        } else {
            bufferedReader = new BufferedReader(source.getReader());
        }
        lexer = new Lexer(new LexerSourceFromReader(bufferedReader, source));
    }

    /**
     * Create reader from given LispStream.
     *
     * @param stream source in a lispstream shape
     */
    @CompilerDirectives.TruffleBoundary
    public Reader(LispStream stream) {
        this.source = null;
        lexer = new Lexer(new LexerSourceFromLispStream(stream));
    }

    @CompilerDirectives.TruffleBoundary
    Optional<Token> peekToken() throws IOException {
        if (peekedToken != null) {
            return Optional.of(peekedToken.token());
        }
        var t = lexer.readToken();
        if (t.isPresent()) {
            peekedToken = t.get();
            return Optional.of(peekedToken.token());
        }
        return Optional.empty();
    }

    @CompilerDirectives.TruffleBoundary
    Optional<Token> getToken() throws IOException {
        if (peekedToken != null) {
            lastToken = peekedToken;
            peekedToken = null;
            return Optional.of(lastToken.token());
        }
        var t = lexer.readToken();
        if (t.isPresent()) {
            lastToken = t.get();
        } else {
            return Optional.empty();
        }
        return Optional.of(lastToken.token());
    }

    int getLine() {
        if (lastToken == null) {
            return 1;
        }
        return lastToken.startLine();
    }

    int getColumn() {
        if (lastToken == null) {
            return 1;
        }
        return lastToken.startColumn();
    }

    int getEndLine() {
        if (lastToken == null) {
            return 1;
        }
        return lastToken.endLine();
    }

    int getEndColumn() {
        if (lastToken == null) {
            return 1;
        }
        return lastToken.endColumn();
    }

    SourceSection section() {
        if (source == null) {
            return null;
        }
        return source.createSection(getLine(), getColumn(), getEndLine(), getEndColumn());
    }

    /**
     * @return list of all top level expressions in given source.
     */
    @CompilerDirectives.TruffleBoundary
    public List<SyntaxObject> readAll() {
        try {
            var lst = new ArrayList<SyntaxObject>();
            Optional<SyntaxObject> maybeValue = readSingle();
            while (maybeValue.isPresent()) {
                lst.add(maybeValue.get());
                maybeValue = readSingle();
            }
            return lst;
        } catch (NumberFormatException nfe) {
            throw new ParsingException(section(), "Failed to read number");
        } catch (EOFException eof) {
            throw new ParsingException(section(), "Unexpected end of file");
        } catch (IOException e) {
            throw new ParsingException(section(), "Failed to read; " + e.getMessage());
        }
    }

    /**
     * Read single sexpr expression.
     *
     * @return sexpr expression
     */
    @CompilerDirectives.TruffleBoundary
    public Optional<SyntaxObject> readSingle() throws IOException {
        Optional<Token> maybeT = getToken();
        if (maybeT.isEmpty()) {
            return Optional.empty();
        }
        var t = maybeT.get();
        if (t instanceof Token.IdentifierToken) {
            var identifier = ((Token.IdentifierToken) t).identifier();
            var symbol = ISLISPContext.get(null).namedSymbol(identifier);
            return Optional.of(new SyntaxObject(symbol, section()));
        }
        if (t instanceof Token.ExactNumberToken) {
            var value = ((Token.ExactNumberToken) t).value();
            if (value.compareTo(BigInteger.valueOf(Integer.MAX_VALUE)) < 0) {
                return Optional.of(new SyntaxObject(value.intValueExact(), section()));
            } else {
                return Optional.of(new SyntaxObject(value, section()));
            }
        }
        if (t instanceof Token.InexactNumberToken inexact) {
            return Optional.of(new SyntaxObject(inexact.value(), section()));
        }
        if (t instanceof Token.QuasiquoteToken
                || t instanceof Token.QuoteToken
                || t instanceof Token.UnquoteSpliceToken
                || t instanceof Token.UnquoteToken
                || t instanceof Token.FunctionRefToken
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
            var value = readSingle().orElseThrow(() ->
                    new ParsingException(section(), "Unexpected end of file"));
            var normalizedSyntaxSymbol = ISLISPContext.get(null).namedSymbol(symbolName);
            var nil = ISLISPContext.get(null).getNil();
            SourceSection fullSection = null;
            if (source != null) {
                var endSection = section();
                if (quoteSection != null && endSection != null) {
                    fullSection = source.createSection(
                        quoteSection.getStartLine(),
                        quoteSection.getStartColumn(),
                        endSection.getEndLine(),
                        endSection.getEndColumn());
                }
            }
            var result = new Pair(
                new SyntaxObject(normalizedSyntaxSymbol, quoteSection),
                new Pair(value, nil));
            return Optional.of(new SyntaxObject(result, fullSection));
        }
        if (t instanceof Token.ArrayBracketOpenToken arr && arr.dimensions() > 1) {
            var startSection = section();
            var content = readArrayContent(arr.dimensions());
            var endSection = section();
            var fullSection = source.createSection(
                startSection.getStartLine(),
                startSection.getStartColumn(),
                endSection.getEndLine(),
                endSection.getEndColumn());
            var array = new LispArray(content, arr.dimensions());
            return Optional.of(new SyntaxObject(array, fullSection));
        }
        if (t instanceof Token.VectorBracketOpenToken
            || (t instanceof Token.ArrayBracketOpenToken arr && arr.dimensions() == 1)
        ) {
            var startSection = section();
            var vec = new LispVector(readUntilClosingBracket());
            var endSection = section();
            SourceSection fullSection = null;
            if (startSection != null && endSection != null) {
                fullSection = source.createSection(
                    startSection.getStartLine(),
                    startSection.getStartColumn(),
                    endSection.getEndLine(),
                    endSection.getEndColumn());
            }
            return Optional.of(new SyntaxObject(vec, fullSection));
        }
        if (t instanceof Token.BracketOpenToken) {
            var startLine = getLine();
            var startColumn = getColumn();
            Optional<Token> next;
            var lst = new ArrayList<Object>();
            var periodSeen = false;
            Object tail = null;
            while (true) {
                next = peekToken();
                if (next.isEmpty()) {
                    throw new ParsingException(section(), "Unexpected end of file");
                }
                var token = next.get();
                if (token instanceof Token.PeriodToken) {
                    if (periodSeen) {
                        throw new ParsingException(section(), "Malformed dotted list");
                    }
                    periodSeen = true;
                    getToken();
                    continue;
                }
                if (token instanceof Token.BracketCloseToken) {
                    if (periodSeen && tail == null) {
                        throw new ParsingException(section(), "Malformed dotted list");
                    }
                    getToken();
                    var endLine = getLine();
                    var endColumn = getColumn();
                    if (lst.isEmpty()) {
                        var nil = ISLISPContext.get(null).getNil();
                        var section = source == null
                            ? null
                            : source.createSection(startLine, startColumn, endLine, endColumn);
                        return Optional.of(new SyntaxObject(nil, section));
                    } else {
                        tail = tail == null ? ISLISPContext.get(null).getNil() : tail;
                        for (var i = lst.size() - 1; i >= 0; i--) {
                            tail = new Pair(lst.get(i), tail);
                        }
                        var parsedTail = (Pair) tail;
                        var section = source == null
                            ? null
                            : source.createSection(startLine, startColumn, endLine, endColumn);
                        return Optional.of(new SyntaxObject(parsedTail, section));
                    }
                }
                if (periodSeen && tail == null) {
                    // orElseThrow shouldn't ever be invoked at this point.
                    tail = readSingle().orElseThrow();
                } else if (periodSeen && tail != null) {
                    throw new ParsingException(section(), "Malformed dotted list");
                } else {
                    // orElseThrow shouldn't ever be invoked at this point.
                    lst.add(readSingle().orElseThrow());
                }
            }
        }
        if (t instanceof Token.CharToken c) {
            var lispChar = new LispChar(c.value());
            return Optional.of(new SyntaxObject(lispChar, section()));
        }
        if (t instanceof Token.StringToken str) {
            return Optional.of(new SyntaxObject(str.value(), section()));
        }
        return Optional.empty();
    }

    Object[] readArrayContent(int dimensions) throws IOException {
        if (dimensions == 1) {
            return readUntilClosingBracket();
        }
        var content = new ArrayList<>();
        while (true) {
            var next = getToken();
            if (next.isEmpty()) {
                throw new ParsingException(section(), "Unexpected end of file");
            }
            var token = next.get();
            if (token instanceof Token.BracketCloseToken) {
                return content.toArray();
            } else if (token instanceof Token.BracketOpenToken) {
                content.add(readArrayContent(dimensions - 1));
            } else {
                throw new ParsingException(section(), "Unexpected token: " + token.toString());
            }
        }
    }

    SyntaxObject[] readUntilClosingBracket() throws IOException {
        Optional<Token> next;
        var lst = new ArrayList<SyntaxObject>();
        while (true) {
            next = peekToken();
            if (next.isEmpty()) {
                throw new ParsingException(section(), "Unexpected end of file");
            }
            var token = next.get();
            if (token instanceof Token.BracketCloseToken) {
                getToken();
                return lst.toArray(SyntaxObject[]::new);
            }
            readSingle().ifPresent(lst::add);
        }
    }
}
