package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.Utils;
import com.github.arvyy.islisp.runtime.LispChar;
import com.github.arvyy.islisp.runtime.LispVector;
import com.github.arvyy.islisp.runtime.Pair;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `convert` syntax.
 */
public class ISLISPConvertNode extends ISLISPExpressionNode {

    @Child
    ISLISPExpressionNode valueExpression;

    @Child
    AbstractConverterNode converterNode;

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    final String module;
    final Symbol className;

    /**
     * Create convert node.
     *
     * @param module module in which the node resides
     * @param valueExpression expression yielding object to be converted
     * @param className symbol denoting class name
     * @param sourceSection matching source section
     */
    public ISLISPConvertNode(
        String module,
        ISLISPExpressionNode valueExpression,
        Symbol className,
        SourceSection sourceSection
    ) {
        super(sourceSection);
        this.className = className;
        this.module = module;
        this.valueExpression = valueExpression;
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
        switch (className.name().toLowerCase()) {
            case "<character>":
                converterNode = ISLISPConvertNodeFactory.ToCharConverterNodeGen.create();
                break;
            case "<integer>":
                converterNode = ISLISPConvertNodeFactory.ToIntConverterNodeGen.create();
                break;
            case "<float>":
                converterNode = ISLISPConvertNodeFactory.ToFloatConverterNodeGen.create();
                break;
            case "<symbol>":
                converterNode = ISLISPConvertNodeFactory.ToSymbolConverterNodeGen.create();
                break;
            case "<string>":
                converterNode = ISLISPConvertNodeFactory.ToStringConverterNodeGen.create();
                break;
            case "<general-vector>":
                converterNode = ISLISPConvertNodeFactory.ToVectorConverterNodeGen.create();
                break;
            case "<list>":
                converterNode = ISLISPConvertNodeFactory.ToListConverterNodeGen.create();
                break;
            default:
        }
    }

    @Override
    public final Object executeGeneric(VirtualFrame frame) {
        var value = valueExpression.executeGeneric(frame);
        if (converterNode == null) {
            return errorSignalerNode.signalUnknownConversion(
                value,
                ISLISPContext.get(this).lookupClass(module, className.name()));
        }
        return converterNode.execute(value);
    }

    abstract static class AbstractConverterNode extends Node {

        @Child
        ISLISPErrorSignalerNode errorSignalerNode;

        AbstractConverterNode() {
            errorSignalerNode = new ISLISPErrorSignalerNode(this);
        }

        abstract Object execute(Object value);

    }

    abstract static class ToFloatConverterNode extends AbstractConverterNode {

        @Specialization
        Object convertFloat(double value) {
            return value;
        }

        @Specialization
        Object convertInt(int value) {
            return (double) value;
        }

        @Specialization
        @CompilerDirectives.TruffleBoundary
        Object convertString(String value) {
            return Double.valueOf(value);
        }

        @Fallback
        Object fallback(Object value) {
            return errorSignalerNode.signalUnknownConversion(
                value,
                ISLISPContext.get(this).lookupClass("<float>"));
        }
    }

    abstract static class ToListConverterNode extends AbstractConverterNode {

        @Specialization
        Object convertList(Pair p) {
            return p;
        }

        @Specialization
        Object convertList(Symbol s) {
            if (Utils.isNil(s)) {
                return s;
            }
            return fallback(s);
        }

        @Specialization
        Object convertString(String s) {
            Object tail = ISLISPContext.get(this).getNil();
            for (int i = s.length() - 1; i >= 0; i--) {
                tail = new Pair(new LispChar(s.codePointAt(i)), tail);
            }
            return tail;
        }

        @Specialization
        Object convertVector(LispVector v) {
            Object tail = ISLISPContext.get(this).getNil();
            for (int i = v.values().length - 1; i >= 0; i--) {
                tail = new Pair(v.values()[i], tail);
            }
            return tail;
        }

        @Fallback
        Object fallback(Object value) {
            return errorSignalerNode.signalUnknownConversion(
                value,
                ISLISPContext.get(this).lookupClass("<list>"));
        }
    }

    abstract static class ToCharConverterNode extends AbstractConverterNode {

        @Specialization
        Object convertChar(LispChar c) {
            return c;
        }

        @Specialization
        Object convertInt(int i) {
            return new LispChar(i);
        }

        @Fallback
        Object fallback(Object value) {
            return errorSignalerNode.signalUnknownConversion(
                value,
                ISLISPContext.get(this).lookupClass("<character>"));
        }

    }

    abstract static class ToSymbolConverterNode extends AbstractConverterNode {

        @Specialization
        Object convertSymbol(Symbol s) {
            return s;
        }

        @Specialization
        @CompilerDirectives.TruffleBoundary
        Object convertChar(LispChar c) {
            return ISLISPContext.get(this).namedSymbol(new String(new int[]{c.codepoint()}, 0, 1));
        }

        @Specialization
        Object convertString(String s) {
            return ISLISPContext.get(this).namedSymbol(s);
        }

        @Fallback
        Object fallback(Object value) {
            return errorSignalerNode.signalUnknownConversion(
                value,
                ISLISPContext.get(this).lookupClass("<symbol>"));
        }

    }

    abstract static class ToVectorConverterNode extends AbstractConverterNode {

        @Specialization
        Object convertVector(LispVector v) {
            return v;
        }

        @Specialization
        Object convertList(Pair p) {
            return new LispVector(Utils.readListAsArray(p));
        }

        @Specialization
        Object convertList(Symbol s) {
            return new LispVector(Utils.readListAsArray(s));
        }

        @Specialization
        Object convertString(String s) {
            Object[] data = new Object[s.length()];
            for (int i = 0; i < data.length; i++) {
                data[i] = new LispChar(s.codePointAt(i));
            }
            return new LispVector(data);
        }

        @Fallback
        Object fallback(Object value) {
            return errorSignalerNode.signalUnknownConversion(
                value,
                ISLISPContext.get(this).lookupClass("<general-vector>"));
        }

    }

    abstract static class ToStringConverterNode extends AbstractConverterNode {

        @Specialization
        Object convertString(String s) {
            return s;
        }

        @Specialization
        @CompilerDirectives.TruffleBoundary
        Object convertFloat(double d) {
            return Double.toString(d);
        }

        @Specialization
        @CompilerDirectives.TruffleBoundary
        Object convertInt(int i) {
            return Integer.toString(i);
        }

        @Specialization
        Object convertSymbol(Symbol s) {
            return s.name();
        }

        @Fallback
        Object fallback(Object value) {
            return errorSignalerNode.signalUnknownConversion(
                value,
                ISLISPContext.get(this).lookupClass("<string>"));
        }

    }

    abstract static class ToIntConverterNode extends AbstractConverterNode {

        @Specialization
        Object convertInt(int i) {
            return i;
        }

        @Specialization
        Object convertChar(LispChar c) {
            return c.codepoint();
        }

        @Specialization
        @CompilerDirectives.TruffleBoundary
        Object convertString(String s) {
            return Integer.parseInt(s);
        }

        @Fallback
        Object fallback(Object value) {
            return errorSignalerNode.signalUnknownConversion(
                value,
                ISLISPContext.get(this).lookupClass("<integer>"));
        }

    }
}
