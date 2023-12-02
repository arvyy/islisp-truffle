package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispOutputStream;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.RootNode;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;

/**
 * Implements `format` function.
 */
public class ISLISPFormat extends RootNode {

    @Child
    DirectCallNode formatChar;

    @Child
    DirectCallNode formatFloat;

    @Child
    DirectCallNode formatInt;

    @Child
    DirectCallNode formatObject;

    @Child
    DirectCallNode formatFreshLine;

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPFormat(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        var ctx = ISLISPContext.get(this);
        var argsLen = frame.getArguments().length;
        String formatString;
        if (frame.getArguments()[2] instanceof String s) {
            formatString = s;
        } else {
            return errorSignalerNode.signalWrongType(frame.getArguments()[2], ctx.lookupClass("<string>"));
        }
        LispOutputStream os;
        if (frame.getArguments()[1] instanceof LispOutputStream out) {
            os = out;
        } else {
            return errorSignalerNode.signalWrongType(frame.getArguments()[1], ctx.lookupClass("<stream>"));
        }
        var args = new Object[frame.getArguments().length];
        System.arraycopy(frame.getArguments(), 0, args, 0, frame.getArguments().length);
        return executeBoundary(os, formatString, args);
    }

    //TODO granualize snippets that need boundary
    @CompilerDirectives.TruffleBoundary
    Object executeBoundary(LispOutputStream os, String formatString, Object[] args) {
        var ctx = ISLISPContext.get(this);
        try {
            int argIndex = 3;
            var writer = getWriter(os);
            for (int i = 0; i < formatString.length(); i++) {
                if (formatString.charAt(i) == '~') {
                    i++;
                    if (i >= formatString.length()) {
                        throw new ISLISPError("Unexpected end of format string", this);
                    }
                    var c = formatString.charAt(i);
                    switch (c) {
                        case '~' -> {
                            writeCodepoint(writer, "~".codePointAt(0));
                        }
                        case 'A' ->
                            formatObject().call(null, os, args[argIndex++], ctx.getNil());
                        case 'B' ->
                            formatInt().call(null, os, args[argIndex++], 2);
                        case 'C' ->
                            formatChar().call(null, os, args[argIndex++]);
                        case 'D' ->
                            formatInt().call(null, os, args[argIndex++], 10);
                        case 'G' ->
                            formatFloat().call(null, os, args[argIndex++]);
                        case 'O' ->
                            formatInt().call(null, os, args[argIndex++], 8);
                        case 'S' ->
                            formatObject().call(null, os, args[argIndex++], ctx.getT());
                        case 'X' ->
                            formatInt().call(null, os, args[argIndex++], 16);
                        case '%' ->
                            writeCodepoint(writer, "\n".codePointAt(0));
                        case '&' ->
                            formatFreshLine().call(null, os);
                        default -> { }
                    }
                } else {
                    writeCodepoint(writer, formatString.codePointAt(i));
                }
            }
        } catch (Exception e) {
            throw new ISLISPError(e.getMessage(), this);
        }

        return ctx.getNil();
    }

    @CompilerDirectives.TruffleBoundary
    Writer getWriter(LispOutputStream os) {
        return new OutputStreamWriter(os.outputStream());
    }

    @CompilerDirectives.TruffleBoundary
    void writeCodepoint(Writer writer, int codePoint) throws IOException {
        writer.write(codePoint);
        writer.flush();
    }

    DirectCallNode formatChar() {
        if (formatChar == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            var ctx = ISLISPContext.get(this);
            var fun = ctx.lookupFunction(ctx.namedSymbol("format-char").identityReference());
            formatChar = this.insert(DirectCallNode.create(fun.callTarget()));
        }
        return formatChar;
    }

    DirectCallNode formatInt() {
        if (formatInt == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            var ctx = ISLISPContext.get(this);
            var fun = ctx.lookupFunction(ctx.namedSymbol("format-integer").identityReference());
            formatInt = this.insert(DirectCallNode.create(fun.callTarget()));
        }
        return formatInt;
    }

    DirectCallNode formatFloat() {
        if (formatFloat == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            var ctx = ISLISPContext.get(this);
            var fun = ctx.lookupFunction(ctx.namedSymbol("format-float").identityReference());
            formatFloat = this.insert(DirectCallNode.create(fun.callTarget()));
        }
        return formatFloat;
    }

    DirectCallNode formatObject() {
        if (formatObject == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            var ctx = ISLISPContext.get(this);
            var fun = ctx.lookupFunction(ctx.namedSymbol("format-object").identityReference());
            formatObject = this.insert(DirectCallNode.create(fun.callTarget()));
        }
        return formatObject;
    }

    DirectCallNode formatFreshLine() {
        if (formatFreshLine == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            var ctx = ISLISPContext.get(this);
            var fun = ctx.lookupFunction(ctx.namedSymbol("format-fresh-line").identityReference());
            formatFreshLine = this.insert(DirectCallNode.create(fun.callTarget()));
        }
        return formatFreshLine;
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPFormat(lang).getCallTarget());
    }

}
