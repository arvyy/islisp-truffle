package com.github.arvyy.islisp.parser;

import java.io.IOException;

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

}
