package com.github.arvyy.islisp.parser;

import com.github.arvyy.islisp.runtime.LispStream;

import java.io.IOException;

/**
 * Lexer source wrapping lisp stream.
 */
public class LexerSourceFromLispStream implements LexerSource {

    private final LispStream stream;

    /**
     * Create lexer source from lisp stream.
     *
     * @param stream lisp stream containing source.
     */
    public LexerSourceFromLispStream(LispStream stream) {
        this.stream = stream;
    }

    @Override
    public int read() throws IOException {
        return stream.readCodepoint();
    }

    @Override
    public void reset() throws IOException {
        stream.reset();
    }

    @Override
    public void mark(int size) throws IOException {
        stream.mark(size);
    }
}
