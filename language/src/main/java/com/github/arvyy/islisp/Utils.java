package com.github.arvyy.islisp;

import com.github.arvyy.islisp.runtime.Pair;
import com.github.arvyy.islisp.runtime.Symbol;

import java.util.ArrayList;
import java.util.List;

public final class Utils {

    private Utils() { }

    public static List<Object> readList(Object v) {
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
        throw new RuntimeException();
    }

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
}
