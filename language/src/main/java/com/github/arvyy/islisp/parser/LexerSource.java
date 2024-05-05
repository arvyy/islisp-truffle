package com.github.arvyy.islisp.parser;

import com.oracle.truffle.api.source.SourceSection;

import java.io.IOException;
import java.util.Optional;

/**
 * Abstracts away data stream used by lexer.
 */
public interface LexerSource {


    /**
     * @return codepoint, or -1 if EOF.
     * @throws IOException
     */
    int read() throws IOException;

    /**
     * Marks position, which can be gotten back to through reset.
     *
     * @param size ensured amount of data buffer
     * @throws IOException
     */
    void mark(int size) throws IOException;

    /**
     * Reset source position to what was during last mark call.
     * @throws IOException
     */
    void reset() throws IOException;

    /**
     * Create source section, if possible, for a given position.
     *
     * @param line line number
     * @param col column number
     * @return source section, if possible.
     */
    default Optional<SourceSection> lexemeSourceSection(int line, int col) {
        return Optional.empty();
    }

}
