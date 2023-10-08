package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispOutputStream;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;

public abstract class ISLISPFormatInteger extends RootNode {

    public ISLISPFormatInteger(TruffleLanguage<?> language) {
        super(language);
    }

    abstract void executeGeneric(Object stream, Object integer, Object radix);

    @Override
    public final Object execute(VirtualFrame frame) {
        executeGeneric(frame.getArguments()[1], frame.getArguments()[2], frame.getArguments()[3]);
        return ISLISPContext.get(this).getNil();
    }

    @Specialization
    public void doProper(LispOutputStream stream, int integer, int radix) {
        doPrint(stream.outputStream(), integer, radix);
    }

    @Fallback
    public void doFallback(Object stream, Object integer, Object radix) {
        throw new ISLISPError("Bad arguments", this);
    }


    @CompilerDirectives.TruffleBoundary
    void doPrint(OutputStream os, int value, int radix) {
        try {
            os.write(Integer.toString(value, radix).getBytes(StandardCharsets.UTF_8));
        } catch (IOException e) {
            throw new ISLISPError(e.getMessage(), this);
        }
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPFormatIntegerNodeGen.create(lang).getCallTarget());
    }
}
