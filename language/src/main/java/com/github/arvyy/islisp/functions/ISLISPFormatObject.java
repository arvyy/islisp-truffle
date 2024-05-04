package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.Utils;
import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

/**
 * Implements `format-object` function, that writes a given object to output stream.
 */
public abstract class ISLISPFormatObject extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPFormatObject(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        return executeGeneric(frame.getArguments()[1], frame.getArguments()[2], frame.getArguments()[3]);
    }

    abstract Object executeGeneric(Object stream, Object obj, Object escape);

    @Specialization
    Object doProper(LispStream stream, Object obj, Object escape) {
        var nil = ISLISPContext.get(this).getNil();
        doPrint(stream, obj, escape != nil);
        return nil;
    }

    @Fallback
    Object doFallback(Object stream, Object obj, Object escape) {
        return errorSignalerNode.signalWrongType(stream, ISLISPContext.get(this).lookupClass("<output-stream>"));
    }

    /**
     * Util java procedure to give same string representation as what format-object produces.
     *
     * @param o value
     * @return display string
     */
    @CompilerDirectives.TruffleBoundary
    public static String format(Object o) {
        try {
            var stream = new LispStream(null, new ByteArrayOutputStream());
            doPrint(stream, o, false);
            return stream.getOutputString().get();
        } catch (Exception e) {
            return "";
        }
    }

    @CompilerDirectives.TruffleBoundary
    static void doPrint(LispStream stream, Object value, boolean escape) {
        try {
            if (value instanceof LispChar c) {
                if (escape) {
                    stream.write("#\\");
                }
                stream.writeCodepoint(c.codepoint());
                return;
            }
            if (value instanceof String s) {
                if (escape) {
                    stream.write("\"");
                }
                stream.write(s);
                if (escape) {
                    stream.write("\"");
                }
                return;
            }
            if (value instanceof LispMutableString s) {
                if (escape) {
                    stream.write("\"");
                }
                for (var c: s.chars()) {
                    stream.writeCodepoint(c.codepoint());
                }
                if (escape) {
                    stream.write("\"");
                }
                return;
            }
            if (value instanceof Integer i) {
                stream.write(i.toString());
                return;
            }
            if (value instanceof Double d) {
                stream.write(d.toString());
                return;
            }
            if (value instanceof LispBigInteger b) {
                stream.write(b.data().toString());
                return;
            }
            if (value instanceof Symbol s) {
                stream.write(s.name());
                return;
            }
            if (value instanceof Pair p) {
                stream.write("(");
                var first = true;
                for (var e: Utils.readList(p)) {
                    if (!first) {
                        stream.write(" ");
                    } else {
                        first = false;
                    }
                    doPrint(stream, e, escape);
                }
                stream.write(")");
                return;
            }
            if (value instanceof LispVector v) {
                stream.write("#(");
                var first = true;
                for (var e: v.values()) {
                    if (!first) {
                        stream.write(" ");
                    } else {
                        first = false;
                    }
                    doPrint(stream, e, escape);
                }
                stream.write(")");
                return;
            }
            if (value instanceof StandardClass c) {
                stream.write("#<class ");
                stream.write(c.name());
                stream.write(">");
                return;
            }
            if (value instanceof BuiltinClass c) {
                stream.write("#<class ");
                stream.write(c.name());
                stream.write(">");
                return;
            }
            if (value instanceof StandardClassObject o) {
                stream.write("#<object ");
                stream.write(o.clazz().name());
                stream.write(" ");
                stream.write(o.hashCode() + "");
                stream.write(">");
                return;
            }
            if (value instanceof LispStream s) {
                stream.write("#<stream ");
                stream.write(s.hashCode() + "");
                stream.write(">");
                return;
            }
            if (value instanceof LispFunction f) {
                if (f.isGeneric()) {
                    stream.write("#<generic-function ");
                } else {
                    stream.write("#<function ");
                }
                stream.write(f.hashCode() + "");
                stream.write(">");
                return;
            }
        } catch (IOException e) {
            throw new ISLISPError(e.getMessage(), null);
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
