package com.github.arvyy.islisp.parser;

import java.io.BufferedReader;
import java.io.IOException;

/**
 * Lexer source wrapping reader.
 */
public class LexerSourceFromReader implements LexerSource {

    private final BufferedReader reader;

    /**
     * Create lexer source from reader.
     * @param reader reader containing source.
     */
    public LexerSourceFromReader(BufferedReader reader) {
        this.reader = reader;
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
}
