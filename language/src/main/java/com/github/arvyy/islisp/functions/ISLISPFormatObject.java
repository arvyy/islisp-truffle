package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispOutputStream;
import com.github.arvyy.islisp.runtime.Symbol;
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
 * Implements `format-object` function, that writes a given object to output stream.
 */
public abstract class ISLISPFormatObject extends RootNode {

    ISLISPFormatObject(TruffleLanguage<?> language) {
        super(language);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        executeGeneric(frame.getArguments()[1], frame.getArguments()[2], frame.getArguments()[3]);
        return ISLISPContext.get(this).getNil();
    }

    abstract void executeGeneric(Object stream, Object obj, Object escape);

    @Specialization
    void doString(LispOutputStream stream, String obj, Object escape) {
        if (escape instanceof Symbol s && s.name().equals("nil")) {
            doPrint(stream.outputStream(), obj);
        } else {
            doPrint(stream.outputStream(), obj);
        }
    }

    @Fallback
    void doFallback(Object stgream, Object obj, Object escape) {
        throw new ISLISPError("Bad arguments", this);
    }

    @CompilerDirectives.TruffleBoundary
    void doPrint(OutputStream os, String value) {
        try {
            os.write(value.getBytes(StandardCharsets.UTF_8));
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
        return new LispFunction(ISLISPFormatObjectNodeGen.create(lang).getCallTarget());
    }
}
