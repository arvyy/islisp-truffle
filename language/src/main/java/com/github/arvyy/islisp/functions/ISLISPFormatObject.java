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

import java.io.*;

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
        doPrint(stream.outputStream(), obj, escape != nil);
        return nil;
    }

    @Fallback
    Object doFallback(Object stream, Object obj, Object escape) {
        return errorSignalerNode.signalWrongType(stream, ISLISPContext.get(this).lookupClass("<output-stream>"));
    }

    @CompilerDirectives.TruffleBoundary
    void doPrint(OutputStream os, Object value, boolean escape) {
        //TODO implement escape
        var writer = new OutputStreamWriter(os);
        doPrint(writer, value, escape);
        try {
            writer.flush();
        } catch (IOException e) {
            throw new ISLISPError(e.getMessage(), this);
        }
    }

    /**
     * Util java procedure to give same string representation as what format-object produces.
     *
     * @param o value
     * @return display string
     */
    @CompilerDirectives.TruffleBoundary
    public static String format(Object o) {
        var sw = new StringWriter();
        doPrint(sw, o, false);
        return sw.toString();
    }

    static void doPrint(Writer writer, Object value, boolean escape) {
        try {
            if (value instanceof LispChar c) {
                if (escape) {
                    writer.write("#\\");
                }
                writer.write(c.codepoint());
                return;
            }
            if (value instanceof String s) {
                if (escape) {
                    writer.write("\"");
                }
                writer.write(s);
                if (escape) {
                    writer.write("\"");
                }
                return;
            }
            if (value instanceof Integer i) {
                writer.write(i.toString());
                return;
            }
            if (value instanceof Double d) {
                writer.write(d.toString());
                return;
            }
            if (value instanceof LispBigInteger b) {
                writer.write(b.data().toString());
                return;
            }
            if (value instanceof Symbol s) {
                writer.write(s.name());
                return;
            }
            if (value instanceof Pair p) {
                writer.write("(");
                var first = true;
                for (var e: Utils.readList(p)) {
                    if (!first) {
                        writer.write(" ");
                    } else {
                        first = false;
                    }
                    doPrint(writer, e, escape);
                }
                writer.write(")");
                return;
            }
            if (value instanceof LispVector v) {
                writer.write("#(");
                var first = true;
                for (var e: v.values()) {
                    if (!first) {
                        writer.write(" ");
                    } else {
                        first = false;
                    }
                    doPrint(writer, e, escape);
                }
                writer.write(")");
                return;
            }
            if (value instanceof StandardClass c) {
                writer.write("#<class ");
                writer.write(c.name());
                writer.write(">");
                return;
            }
            if (value instanceof BuiltinClass c) {
                writer.write("#<class ");
                writer.write(c.name());
                writer.write(">");
                return;
            }
            if (value instanceof StandardClassObject o) {
                writer.write("#<object ");
                writer.write(o.clazz().name());
                writer.write(" ");
                writer.write(o.hashCode() + "");
                writer.write(">");
                return;
            }
            if (value instanceof LispStream s) {
                writer.write("#<stream ");
                writer.write(s.hashCode() + "");
                writer.write(">");
                return;
            }
            if (value instanceof LispFunction f) {
                if (f.isGeneric()) {
                    writer.write("#<generic-function ");
                } else {
                    writer.write("#<function ");
                }
                writer.write(f.hashCode() + "");
                writer.write(">");
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
