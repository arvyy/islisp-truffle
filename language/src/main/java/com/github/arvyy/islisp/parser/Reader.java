package com.github.arvyy.islisp.parser;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;

import java.io.BufferedReader;
import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Reader using lexer's token input and forms
 * sexprs to be parsed from.
 */
//TODO rename to not clash with java.io.reader
public class Reader {

    private final Lexer lexer;
    private final Source source;
    private final Map<EqWrapper, SourceSection> sourceSectionMap;
    private TokenWithSource peekedToken;

    private TokenWithSource lastToken;

    /**
     * Create reader from given source.
     *
     * @param source source to read from
     * @param sourceSectionMap map to populate with source location information while reading
     */
    @CompilerDirectives.TruffleBoundary
    public Reader(Source source, Map<EqWrapper, SourceSection> sourceSectionMap) {
        this.source = source;
        BufferedReader bufferedReader;
        if (source.getReader() instanceof BufferedReader br) {
            bufferedReader = br;
        } else {
            bufferedReader = new BufferedReader(source.getReader());
        }
        lexer = new Lexer(new LexerSourceFromReader(bufferedReader));
        this.sourceSectionMap = sourceSectionMap;
    }

    /**
     * Create reader from given LispStream.
     *
     * @param stream source in a lispstream shape
     */
    @CompilerDirectives.TruffleBoundary
    public Reader(LispStream stream) {
        this.source = null;
        this.sourceSectionMap = null;
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
        return lastToken.startLine();
    }

    int getColumn() {
        return lastToken.startColumn();
    }

    int getEndLine() {
        return lastToken.endLine();
    }

    int getEndColumn() {
        return lastToken.endColumn();
    }

    SourceSection section() {
        return source.createSection(getLine(), getColumn(), getEndLine(), getEndColumn());
    }

    /**
     * @return list of all top level expressions in given source.
     */
    @CompilerDirectives.TruffleBoundary
    public List<Object> readAll() {
        try {
            var lst = new ArrayList<Object>();
            Optional<Object> maybeValue = readSingle();
            while (maybeValue.isPresent()) {
                lst.add(maybeValue.get());
                maybeValue = readSingle();
            }
            return lst;
        } catch (IOException e) {
            throw new RuntimeException("Failed to read", e); //TODO
        }
    }

    /**
     * Read single sexpr expression.
     *
     * @return sexpr expression
     */
    @CompilerDirectives.TruffleBoundary
    public Optional<Object> readSingle() throws IOException {
        Optional<Token> maybeT = getToken();
        if (maybeT.isEmpty()) {
            return Optional.empty();
        }
        var t = maybeT.get();
        if (t instanceof Token.IdentifierToken) {
            var identifier = ((Token.IdentifierToken) t).identifier();
            var symbol = ISLISPContext.get(null).namedSymbol(identifier);
            var symbolWithSource = new Symbol(symbol.name(), symbol.identityReference());
            if (source != null) {
                sourceSectionMap.put(new EqWrapper(symbolWithSource), section());
            }
            return Optional.of(symbolWithSource);
        }
        if (t instanceof Token.ExactNumberToken) {
            var value = ((Token.ExactNumberToken) t).value();
            if (value.compareTo(BigInteger.valueOf(Integer.MAX_VALUE)) < 0) {
                return Optional.of(value.intValueExact());
            } else {
                return Optional.of(value);
            }
        }
        if (t instanceof Token.InexactNumberToken inexact) {
            return Optional.of(inexact.value());
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
            var value = readSingle().orElseThrow();
            var normalizedSyntaxSymbol = ISLISPContext.get(null).namedSymbol(symbolName);
            var nil = ISLISPContext.get(null).getNil();
            SourceSection fullSection = null;
            if (source != null) {
                var endSection = sourceSectionMap.get(new EqWrapper(value));
                if (quoteSection != null && endSection != null) {
                    fullSection = source.createSection(
                        quoteSection.getStartLine(),
                        quoteSection.getStartColumn(),
                        endSection.getEndLine(),
                        endSection.getEndColumn());
                }
            }
            var result = new Pair(
                normalizedSyntaxSymbol,
                new Pair(
                    value,
                    nil));
            sourceSectionMap.put(new EqWrapper(result), fullSection);
            return Optional.of(result);
        }
        if (t instanceof Token.ArrayBracketOpenToken arr && arr.dimensions() > 1) {
            var startLine = getLine();
            var startColumn = getColumn();
            var content = readArrayContent(arr.dimensions());
            var array = new LispArray(content, arr.dimensions());
            var endLine = getLine();
            var endColumn = getColumn();
            if (source != null) {
                var section = source.createSection(startLine, startColumn, endLine, endColumn);
                sourceSectionMap.put(new EqWrapper(array), section);
            }
            return Optional.of(array);
        }
        if (t instanceof Token.VectorBracketOpenToken
            || (t instanceof Token.ArrayBracketOpenToken arr && arr.dimensions() == 1)
        ) {
            var startLine = getLine();
            var startColumn = getColumn();
            var vec = new LispVector(readUntilClosingBracket());
            var endLine = getLine();
            var endColumn = getColumn();
            if (source != null) {
                var section = source.createSection(startLine, startColumn, endLine, endColumn);
                sourceSectionMap.put(new EqWrapper(vec), section);
            }
            return Optional.of(vec);
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
                    throw new RuntimeException("Premature end of file"); // TODO
                }
                var token = next.get();
                if (token instanceof Token.PeriodToken) {
                    if (periodSeen) {
                        throw new RuntimeException("Bad dotted list"); //TODO
                    }
                    periodSeen = true;
                    getToken();
                    continue;
                }
                if (token instanceof Token.BracketCloseToken) {
                    if (periodSeen && tail == null) {
                        throw new RuntimeException("Bad dotted list"); // TODO
                    }
                    getToken();
                    var endLine = getLine();
                    var endColumn = getColumn();
                    if (lst.isEmpty()) {
                        var nil = ISLISPContext.get(null).getNil();
                        var nilWithPos = new Symbol(nil.name(), nil.identityReference());
                        if (source != null) {
                            var section = source.createSection(startLine, startColumn, endLine, endColumn);
                            sourceSectionMap.put(new EqWrapper(nilWithPos), section);
                        }
                        return Optional.of(nilWithPos);
                    } else {
                        tail = tail == null ? ISLISPContext.get(null).getNil() : tail;
                        for (var i = lst.size() - 1; i >= 0; i--) {
                            tail = new Pair(lst.get(i), tail);
                        }
                        var parsedTail = (Pair) tail;
                        if (source != null) {
                            var section = source.createSection(startLine, startColumn, endLine, endColumn);
                            sourceSectionMap.put(new EqWrapper(parsedTail), section);
                        }
                        return Optional.of(parsedTail);
                    }
                }
                if (periodSeen && tail == null) {
                    tail = readSingle().orElseThrow(); // TODO
                } else if (periodSeen && tail != null) {
                    throw new RuntimeException("Bad dotted list"); //TODO
                } else {
                    lst.add(readSingle().orElseThrow()); //TODO
                }
            }
        }
        if (t instanceof Token.CharToken c) {
            var lispChar = new LispChar(c.value());
            if (source != null) {
                sourceSectionMap.put(new EqWrapper(lispChar), section());
            }
            return Optional.of(lispChar);
        }
        if (t instanceof Token.StringToken str) {
            return Optional.of(str.value());
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
                throw new RuntimeException("Premature end of file");
            }
            var token = next.get();
            if (token instanceof Token.BracketCloseToken) {
                return content.toArray();
            } else if (token instanceof Token.BracketOpenToken) {
                content.add(readArrayContent(dimensions - 1));
            } else {
                throw new RuntimeException("Unexpected token " + token);
            }
        }
    }

    Object[] readUntilClosingBracket() throws IOException {
        Optional<Token> next;
        var lst = new ArrayList<Object>();
        while (true) {
            next = peekToken();
            if (next.isEmpty()) {
                throw new RuntimeException("Premature end of file");
            }
            var token = next.get();
            if (token instanceof Token.BracketCloseToken) {
                getToken();
                return lst.toArray();
            }
            lst.add(readSingle().orElseThrow()); //TODO
        }
    }
}
