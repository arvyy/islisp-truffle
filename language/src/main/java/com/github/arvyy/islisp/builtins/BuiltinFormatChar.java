package com.github.arvyy.islisp.builtins;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.ISLISPError;
import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;

public abstract class BuiltinFormatChar extends RootNode {

    public BuiltinFormatChar(TruffleLanguage<?> language) {
        super(language);
    }

    abstract void executeGeneric(Object stream, Object ch);

    @Override
    public final Value execute(VirtualFrame frame) {
        executeGeneric(frame.getArguments()[1], frame.getArguments()[2]);
        return ISLISPContext.get(this).getNil();
    }

    @Specialization
    public void executeProper(LispOutputStream stream, LispChar ch) {
        doPrint(stream.outputStream(), ch.codepoint());
    }

    @Fallback
    public void executeFallback(Object stream, Object c) {
        throw new ISLISPError("Bad arguments", this);
    }


    @CompilerDirectives.TruffleBoundary
    void doPrint(OutputStream os, int codepoint) {
        try {
            os.write(new String(new int[] {codepoint}, 0, 1).getBytes(StandardCharsets.UTF_8));
        } catch (IOException e) {
            throw new ISLISPError(e.getMessage(), this);
        }
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(BuiltinFormatCharNodeGen.create(lang).getCallTarget());
    }
}
