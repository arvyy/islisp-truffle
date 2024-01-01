package com.github.arvyy.islisp.parser;

import com.oracle.truffle.api.CompilerDirectives;

import java.io.BufferedReader;
import java.io.EOFException;
import java.io.IOException;
import java.math.BigInteger;
import java.util.Optional;
import java.util.regex.Pattern;

/**
 * ISLISP lexer.
 */
public class Lexer {

    private final BufferedReader reader;
    private int col;
    private int line;
    private int markedCol;
    private int markedLine;
    private boolean eol;
    private boolean markedEol;

    /**
     * Create lexer from given buffered reader.
     *
     * @param reader buffered reader
     */
    public Lexer(BufferedReader reader) {
        this.reader = reader;
        col = 0;
        line = 1;
        eol = false;
    }

    /**
     * Read next token.
     *
     * @return optional token
     * @throws IOException
     */
    @CompilerDirectives.TruffleBoundary
    public Optional<TokenWithSource> readToken() throws IOException {
        skipWhitespace();
        int c;
        try {
            mark(10);
            c = readCodepoint();
        } catch (EOFException ignored) {
            return Optional.empty();
        }
        int startLine = line;
        int startCol = col;
        if (c == '.') {
            return Optional.of(new TokenWithSource(new Token.PeriodToken(), startLine, startCol, line, col));
        }
        if (c == '(') {
            return Optional.of(new TokenWithSource(new Token.BracketOpenToken(), startLine, startCol, line, col));
        }
        if (c == ')') {
            return Optional.of(new TokenWithSource(new Token.BracketCloseToken(), startLine, startCol, line, col));
        }
        if (c == '`') {
            return Optional.of(new TokenWithSource(new Token.QuasiquoteToken(), startLine, startCol, line, col));
        }
        if (c == '\'') {
            return Optional.of(new TokenWithSource(new Token.QuoteToken(), startLine, startCol, line, col));
        }
        if (c == ',') {
            mark(1);
            c = readCodepoint(false);
            if (c == '@') {
                return Optional.of(new TokenWithSource(new Token.UnquoteSpliceToken(), startLine, startCol, line, col));
            } else {
                reset();
                return Optional.of(new TokenWithSource(new Token.UnquoteToken(), startLine, startCol, line, col));
            }
        }
        if (c == '#') {
            mark(10);
            c = readCodepoint();
            if (c == '(') {
                return Optional.of(new TokenWithSource(
                    new Token.VectorBracketOpenToken(), startLine, startCol, line, col));
            }
            if (Character.isDigit(c)) {
                reset();
                var sb = new StringBuilder();
                while (true) {
                    mark(1);
                    c = readCodepoint();
                    if (Character.isDigit(c)) {
                        sb.appendCodePoint(c);
                    } else {
                        reset();
                        break;
                    }
                }
                int dimensions = Integer.parseInt(sb.toString());
                if (Character.toLowerCase(readCodepoint()) == 'a'
                    && readCodepoint() == '('
                ) {
                    return Optional.of(new TokenWithSource(
                        new Token.ArrayBracketOpenToken(dimensions), startLine, startCol, line, col));
                } else {
                    throw new RuntimeException("Bad array definition"); //TODO
                }
            }
            if (c == '\\') {
                return Optional.of(new TokenWithSource(
                    new Token.CharToken(readCharLiteral()), startLine, startCol, line, col));
            }
            if (c == 'b' || c == 'B') {
                var numberToken = readNumberToken(2);
                return Optional.of(new TokenWithSource(numberToken, startLine, startCol, line, col));
            }
            if (c == 'o' || c == 'O') {
                var numberToken = readNumberToken(8);
                return Optional.of(new TokenWithSource(numberToken, startLine, startCol, line, col));
            }
            if (c == 'x' || c == 'X') {
                var numberToken = readNumberToken(16);
                return Optional.of(new TokenWithSource(numberToken, startLine, startCol, line, col));
            }
            if (c == '\'') {
                return Optional.of(new TokenWithSource(new Token.FunctionRefToken(), startLine, startCol, line, col));
            }
        }
        if (c == '-' || c == '+') {
            var follow = readCodepoint(false);
            if (Character.isDigit(follow)) {
                reset();
                var numberToken = readNumberToken(10);
                return Optional.of(new TokenWithSource(numberToken, startLine, startCol, line, col));
            } else {
                reset();
            }
        }
        if (c == '"') {
            var strToken = readStringLiteral();
            return Optional.of(new TokenWithSource(strToken, startLine, startCol, line, col));
        }
        if (Character.isDigit(c)) {
            reset();
            var numberToken = readNumberToken(10);
            return Optional.of(new TokenWithSource(numberToken, startLine, startCol, line, col));
        }
        if (isSymbolStarterChar(c)) {
            reset();
            var identifier = readIdentifierToken();
            return Optional.of(new TokenWithSource(identifier, startLine, startCol, line, col));
        }
        throw new RuntimeException("Unrecognized lexeme: " + Character.toString(c)); //TODO
    }

    private Token.StringToken readStringLiteral() throws IOException {
        var sb = new StringBuilder();
        while (true) {
            mark(2);
            var c = readCodepoint();
            if (c == '"') {
                return new Token.StringToken(sb.toString());
            }
            if (c == '\\') {
                c = readCodepoint();
                switch (c) {
                    case '"':
                        sb.appendCodePoint('"');
                        continue;
                    case '\\':
                        sb.appendCodePoint('\\');
                        continue;
                    default:
                }
                throw new RuntimeException("Unknown escape"); //TODO
            }
            sb.appendCodePoint(c);
        }
    }

    Token readIdentifierToken() throws IOException {
        StringBuilder sb = new StringBuilder();
        while (true) {
            mark(1);
            var c = readCodepoint(false);
            if (c == -1) {
                break;
            }
            if (!isSymbolContChar(c)) {
                reset();
                break;
            }
            sb.appendCodePoint(c);
        }
        return new Token.IdentifierToken(sb.toString());
    }

    int readCharLiteral() throws IOException {
        String[] special = {
            "newline",
            "space"
        };
        char[] specialValue = {
            '\n', ' '
        };
        outter:
        for (var i = 0; i < special.length; i++) {
            var name = special[i];
            mark(name.length());
            for (int j = 0; j < name.length(); j++) {
                if (readCodepoint() != name.codePointAt(j)) {
                    reset();
                    continue outter;
                }
            }
            return specialValue[i];
        }
        return readCodepoint();
    }

    final char[] starterSpecialChar = {
        '<', '>', '/', '*',
        '=', '?', '_', '!',
        '$', '%', '[', ']',
        '^', '{', '}', '~',
        ':', '&', '+', '-'
    };
    boolean isSymbolStarterChar(int codepoint) {
        if (Character.isAlphabetic(codepoint)) {
            return true;
        }
        for (var c: starterSpecialChar) {
            if (codepoint == c) {
                return true;
            }
        }
        return false;
    }

    boolean isSymbolContChar(int codepoint) {
        if (isSymbolStarterChar(codepoint)) {
            return true;
        }
        if (Character.isDigit(codepoint)) {
            return true;
        }
        return switch (codepoint) {
            case '-', '+' -> true;
            default -> false;
        };
    }

    final Pattern numberCharPattern = Pattern.compile("[0-9a-fA-F.+\\-]");
    Token readNumberToken(int radix) throws IOException {
        // super lazy and slow implementation
        var sb = new StringBuilder();
        mark(1);
        while (true) {
            mark(1);
            var c = readCodepoint(false);
            if (c == -1) {
                break;
            }
            if (!numberCharPattern.matcher(Character.toString(c)).matches()) {
                reset();
                break;
            }
            sb.appendCodePoint(c);
        }
        var numberString = sb.toString();
        if (radix == 10 && (numberString.contains(".") || numberString.contains("e") || numberString.contains("E"))) {
            return new Token.InexactNumberToken(Double.parseDouble(numberString));
        } else {
            return new Token.ExactNumberToken(new BigInteger(numberString, radix));
        }
    }

    int readCodepoint() throws IOException {
        return readCodepoint(true);
    }

    int readCodepoint(boolean throwEOF) throws IOException {
        int c = reader.read();
        if (c == -1) {
            if (throwEOF) {
                throw new EOFException();
            } else {
                return -1;
            }
        }
        if (eol) {
            col = 1;
            line++;
            eol = false;
        } else {
            col++;
        }
        if (c == '\n') {
            eol = true;
        }
        return c;
    }

    void skipWhitespace() throws IOException {
        while (true) {
            mark(2);
            int c;
            try {
                c = readCodepoint();
            } catch (EOFException ignored) {
                return;
            }
            if (c == ' ' || c == '\n' || c == '\r') {
                continue;
            }
            if (c == ';') {
                skipUntilEOL();
                continue;
            }
            if (c == '#') {
                if (readCodepoint() == '|') {
                    skipUntilBlockCommentEnd();
                    continue;
                }
            }
            reset();
            break;
        }
    }

    void skipUntilEOL() throws IOException {
        try {
            var c = readCodepoint();
            while (c != '\n') {
                c = readCodepoint();
            }
        } catch (EOFException ignored) {
            // input can end before EOL character, it's ok
        }
    }

    void skipUntilBlockCommentEnd() throws IOException {
        int level = 1;
        while (true) {
            var c = readCodepoint();
            if (c == '#') {
                if (readCodepoint() == '|') {
                    level++;
                }
            }
            if (c == '|') {
                if (readCodepoint() == '#') {
                    level--;
                    if (level == 0) {
                        break;
                    }
                }
            }
        }
    }

    void mark(int size) throws IOException {
        reader.mark(size);
        markedCol = col;
        markedLine = line;
        markedEol = eol;
    }

    void reset() throws IOException {
        reader.reset();
        col = markedCol;
        line = markedLine;
        eol = markedEol;
    }

}
