package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispStream;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;

import java.io.IOException;

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
    public String getName() {
        return "format";
    }

    @Override
    @CompilerDirectives.TruffleBoundary
    public SourceSection getSourceSection() {
        return Source.newBuilder("islisp", "", ISLISPFormat.class.getSimpleName())
            .internal(true)
            .build()
            .createSection(1);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        var ctx = ISLISPContext.get(this);
        var argsLen = frame.getArguments().length;
        if (argsLen < 3) {
            return errorSignalerNode.signalWrongArgumentCount(argsLen - 1, 2, -1);
        }
        String formatString;
        if (frame.getArguments()[2] instanceof String s) {
            formatString = s;
        } else {
            return errorSignalerNode.signalWrongType(frame.getArguments()[2], ctx.lookupClass("<string>"));
        }
        LispStream os;
        if (frame.getArguments()[1] instanceof LispStream out) {
            os = out;
        } else {
            return errorSignalerNode.signalWrongType(frame.getArguments()[1], ctx.lookupClass("<stream>"));
        }
        var args = new Object[frame.getArguments().length];
        System.arraycopy(frame.getArguments(), 0, args, 0, frame.getArguments().length);
        return executeBoundary(os, formatString, args);
    }

    @CompilerDirectives.TruffleBoundary
    Object executeBoundary(LispStream os, String formatString, Object[] args) {
        var ctx = ISLISPContext.get(this);
        try {
            int argIndex = 3;
            for (int i = 0; i < formatString.length(); i++) {
                if (formatString.charAt(i) == '~') {
                    i++;
                    if (i >= formatString.length()) {
                        writeCodepoint(os, "~".codePointAt(0));
                        continue;
                    }
                    var c = formatString.charAt(i);
                    switch (c) {
                        case '~' -> {
                            writeCodepoint(os, "~".codePointAt(0));
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
                            writeCodepoint(os, "\n".codePointAt(0));
                        case '&' ->
                            formatFreshLine().call(null, os);
                        default -> { }
                    }
                } else {
                    writeCodepoint(os, formatString.codePointAt(i));
                }
            }
        } catch (IOException e) {
            return errorSignalerNode.signalIOError(e);
        }

        return ctx.getNil();
    }

    @CompilerDirectives.TruffleBoundary
    void writeCodepoint(LispStream stream, int codePoint) throws IOException {
        stream.writeCodepoint(codePoint);
        if (codePoint == '\n') {
            stream.flush();
        }
    }

    DirectCallNode formatChar() {
        if (formatChar == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            var ctx = ISLISPContext.get(this);
            var fun = ctx.lookupFunction("ROOT", ctx.namedSymbol("format-char"));
            formatChar = this.insert(DirectCallNode.create(fun.callTarget()));
        }
        return formatChar;
    }

    DirectCallNode formatInt() {
        if (formatInt == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            var ctx = ISLISPContext.get(this);
            var fun = ctx.lookupFunction("ROOT", ctx.namedSymbol("format-integer"));
            formatInt = this.insert(DirectCallNode.create(fun.callTarget()));
        }
        return formatInt;
    }

    DirectCallNode formatFloat() {
        if (formatFloat == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            var ctx = ISLISPContext.get(this);
            var fun = ctx.lookupFunction("ROOT", ctx.namedSymbol("format-float"));
            formatFloat = this.insert(DirectCallNode.create(fun.callTarget()));
        }
        return formatFloat;
    }

    DirectCallNode formatObject() {
        if (formatObject == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            var ctx = ISLISPContext.get(this);
            var fun = ctx.lookupFunction("ROOT", ctx.namedSymbol("format-object"));
            formatObject = this.insert(DirectCallNode.create(fun.callTarget()));
        }
        return formatObject;
    }

    DirectCallNode formatFreshLine() {
        if (formatFreshLine == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            var ctx = ISLISPContext.get(this);
            var fun = ctx.lookupFunction("ROOT", ctx.namedSymbol("format-fresh-line"));
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
