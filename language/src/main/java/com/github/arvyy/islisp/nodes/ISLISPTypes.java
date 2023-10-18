package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.dsl.ImplicitCast;
import com.oracle.truffle.api.dsl.TypeSystem;

// class cannot be final
//CHECKSTYLE:OFF
@TypeSystem({
    LispClass.class,
    LispFunction.class,
    Pair.class,
    StandardClassObject.class,
    Symbol.class,
    LispOutputStream.class,
    LispChar.class,
    String.class,
    StringBuffer.class,
    LispVector.class,
    int.class,
    double.class
})
public class ISLISPTypes {
//CHECKSTYLE:ON

    protected ISLISPTypes() { }

    @ImplicitCast
    public static double intToDouble(int v) {
        return v;
    }

}
