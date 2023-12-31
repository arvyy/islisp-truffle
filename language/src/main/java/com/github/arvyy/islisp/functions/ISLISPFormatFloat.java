package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.runtime.LispCharStream;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

import java.io.IOException;
import java.io.Writer;

/**
 * Implements `format-float` function.
 */
public abstract class ISLISPFormatFloat extends RootNode {

    ISLISPFormatFloat(TruffleLanguage<?> language) {
        super(language);
    }

    abstract void executeGeneric(Object stream, Object f);

    @Override
    public final Object execute(VirtualFrame frame) {
        executeGeneric(frame.getArguments()[1], frame.getArguments()[2]);
        return ISLISPContext.get(this).getNil();
    }

    @Specialization
    void doProper(LispCharStream stream, double f) {
        doPrint(stream.getOutput(), f);
    }

    @Fallback
    void doFallback(Object stream, Object f) {
        throw new ISLISPError("Bad arguments", this);
    }


    @CompilerDirectives.TruffleBoundary
    void doPrint(Writer writer, double f) {
        try {
            writer.write(Double.toString(f));
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
        return new LispFunction(ISLISPFormatFloatNodeGen.create(lang).getCallTarget());
    }
}
