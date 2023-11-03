package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.dsl.ImplicitCast;
import com.oracle.truffle.api.dsl.TypeSystem;

import java.math.BigInteger;

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

    /**
     * Implicitly convert int to double.
     *
     * @param v int value
     * @return double value
     */
    @ImplicitCast
    public static BigInteger intToBigInt(int v) {
        return BigInteger.valueOf(v);
    }

    /**
     * Implicitly convert int to double.
     *
     * @param v int value
     * @return double value
     */
    @ImplicitCast
    public static double intToDouble(int v) {
        return v;
    }

    /**
     * Implicitly convert float to double.
     *
     * @param f float value
     * @return double value
     */
    @ImplicitCast
    public static double floatToDouble(float f) {
        return f;
    }

}
