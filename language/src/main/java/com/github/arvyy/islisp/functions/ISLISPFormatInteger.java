package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispStream;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;

/**
 * Implements `format-integer` function, that writes a given integer to output stream.
 */
public abstract class ISLISPFormatInteger extends RootNode {

    ISLISPFormatInteger(TruffleLanguage<?> language) {
        super(language);
    }

    abstract void executeGeneric(Object stream, Object integer, Object radix);

    @Override
    public final Object execute(VirtualFrame frame) {
        executeGeneric(frame.getArguments()[1], frame.getArguments()[2], frame.getArguments()[3]);
        return ISLISPContext.get(this).getNil();
    }

    @Specialization
    void doProper(LispStream stream, int integer, int radix) {
        doPrint(stream.outputStream(), integer, radix);
    }

    @Fallback
    void doFallback(Object stream, Object integer, Object radix) {
        throw new ISLISPError("Bad arguments", this);
    }


    @CompilerDirectives.TruffleBoundary
    void doPrint(OutputStream os, int value, int radix) {
        try {
            os.write(Integer.toString(value, radix).toUpperCase().getBytes(StandardCharsets.UTF_8));
        } catch (IOException e) {
            throw new ISLISPError(e.getMessage(), this);
        }
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPFormatIntegerNodeGen.create(lang).getCallTarget());
    }
}
