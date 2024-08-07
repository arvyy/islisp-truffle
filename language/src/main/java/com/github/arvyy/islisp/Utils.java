package com.github.arvyy.islisp;

import com.github.arvyy.islisp.runtime.Pair;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.nodes.ControlFlowException;

import java.util.ArrayList;
import java.util.List;

/**
 * Misc static utilities.
 */
public final class Utils {

    private Utils() { }

    /**
     * Exception signalling given object wasn't a list.
     */
    public static class NotAList extends ControlFlowException {
    }

    /**
     * Parse sexpr (Pair or nil) to a java list.
     *
     * @param v sesxpr
     * @return java list
     */
    public static List<Object> readList(Object v) throws NotAList {
        if (v instanceof Pair p) {
            var lst = new ArrayList<Object>();
            for (var el : p) {
                lst.add(el);
            }
            return lst;
        } else if (v instanceof Symbol s) {
            if (s.identityReference() == ISLISPContext.get(null).getNil().identityReference()) {
                return List.of();
            }
        }
        throw new NotAList();
    }

    /**
     * Parse sexpr (Pair or nil) to a java array.
     *
     * @param v sesxpr
     * @return java array
     */
    public static Object[] readListAsArray(Object v) throws NotAList {
        int size = 0;
        Object i = v;
        while (true) {
            if (i instanceof Pair p) {
                size++;
                i = p.cdr();
            } else if (i instanceof Symbol s) {
                if (s.identityReference() == ISLISPContext.get(null).getNil().identityReference()) {
                    break;
                } else {
                    throw new NotAList();
                }
            } else {
                throw new NotAList();
            }
        }

        var data = new Object[size];
        i = v;
        for (int j = 0; j < data.length; j++) {
            data[j] = ((Pair) i).car();
            i = ((Pair) i).cdr();
        }
        return data;
    }

    /**
     * Convert java list to sexpr equivalent.
     *
     * @param lst java list
     * @return sexpr
     */
    public static Object listToValue(List<Object> lst) {
        Object val = ISLISPContext.get(null).getNil();
        for (int i = lst.size() - 1; i >= 0; i--) {
            val = new Pair(lst.get(i), val);
        }
        if (val instanceof Pair p) {
            return new Pair(p.car(), p.cdr());
        }
        return val;
    }

    /**
     * Is given object a nil symbol.
     *
     * @param o any object
     * @return true if o is nil
     */
    public static boolean isNil(Object o) {
        var ctx = ISLISPContext.get(null);
        var nil = ctx.getNil();
        return o instanceof Symbol s && s.identityReference() == nil.identityReference();
    }

    /**
     * Convert java's boolean to islisp t and nil.
     *
     * @param b boolean to convert
     * @return t if b was true, nil otherwise
     */
    public static Object booleanToSymbol(boolean b) {
        var ctx = ISLISPContext.get(null);
        var nil = ctx.getNil();
        var t = ctx.getT();
        return b ? t : nil;
    }
}
