package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.CompilerDirectives;
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
    LispStream.class,
    LispChar.class,
    String.class,
    LispMutableString.class,
    LispVector.class,
    LispArray.class,
    LispBigInteger.class,
    int.class,
    double.class
})
public class ISLISPTypes {
//CHECKSTYLE:ON

    protected ISLISPTypes() { }

    /**
     * Implicitly convert int to big int.
     *
     * @param v big int value
     * @return int value
     */
    @ImplicitCast
    @CompilerDirectives.TruffleBoundary
    public static LispBigInteger intToBigInt(int v) {
        return LispBigInteger.valueOf(v);
    }

    /**
     * Implicitly convert long to big int.
     *
     * @param v big int value
     * @return int value
     */
    @ImplicitCast
    @CompilerDirectives.TruffleBoundary
    public static LispBigInteger longToBigInt(long v) {
        return LispBigInteger.valueOf(v);
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
