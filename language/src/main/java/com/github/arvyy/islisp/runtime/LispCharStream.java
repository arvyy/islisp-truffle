package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.TruffleObject;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Writer;

/**
 * ISLISP stream object.
 */
public class LispCharStream implements TruffleObject {
    private Writer output;
    private BufferedReader input;
    private boolean closed;

    /**
     * Construct the character stream; output or input can be null but not both.
     *
     * @param output
     * @param input
     */
    public LispCharStream(Writer output, BufferedReader input) {
        this.output = output;
        this.input = input;
        closed = false;
    }


    /**
     * @return output writer
     */
    public Writer getOutput() {
        return output;
    }

    /**
     * @return input reader
     */
    public BufferedReader getInput() {
        return input;
    }

    /**
     * @return is stream closed
     */
    public boolean isClosed() {
        return closed;
    }

    /**
     * Close the stream.
     */
    @CompilerDirectives.TruffleBoundary
    public void close() {
        closed = true;
        if (output != null) {
            try {
                output.close();
                output = null;
            } catch (IOException ignored) {
            }
        }
        if (input != null) {
            try {
                input.close();
                input = null;
            } catch (IOException ignored) {
            }
        }
    }
}
