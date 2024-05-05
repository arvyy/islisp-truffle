package com.github.arvyy.islisp.parser;

import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.Objects;
import java.util.Optional;

/**
 * Lexer source wrapping reader.
 */
public class LexerSourceFromReader implements LexerSource {

    private final BufferedReader reader;
    private final Source source;

    /**
     * Create lexer source from reader.
     * @param reader reader containing source.
     * @param source source the reader was taken from, if possible. Can be null.
     */
    public LexerSourceFromReader(BufferedReader reader, Source source) {
        this.reader = Objects.requireNonNull(reader);
        this.source = source;
    }

    @Override
    public int read() throws IOException {
        return reader.read();
    }

    @Override
    public void reset() throws IOException {
        reader.reset();
    }

    @Override
    public void mark(int size) throws IOException {
        reader.mark(size);
    }

    @Override
    public Optional<SourceSection> lexemeSourceSection(int line, int col) {
        if (source == null) {
            return Optional.empty();
        }
        return Optional.of(source.createSection(line, col, 1));
    }
}
