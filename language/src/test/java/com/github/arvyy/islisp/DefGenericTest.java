package com.github.arvyy.islisp;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class DefGenericTest extends AbstractTestSuite {

    @Test
    public void testDefGenericDispatch() {
        var result = ctx.eval("islisp", """
                (defgeneric foo (bar))
                (defmethod foo (bar)
                    'default)
                (defmethod foo ((bar <number>))
                    'number)
                (foo 1)
                """);
        assertEquals("number", result.asString());

        result = ctx.eval("islisp", """
                (defgeneric foo (bar))
                (defmethod foo (bar)
                    'default)
                (defmethod foo ((bar <number>))
                    'number)
                (foo 'bar)
                """);
        assertEquals("default", result.asString());
    }

    @Test
    public void testDefGenericHasNextMethod() {
        var result = ctx.eval("islisp", """
                (defgeneric foo (bar))
                (defmethod foo (bar)
                    (next-method-p))
                (defmethod foo ((bar <number>))
                    (next-method-p))
                (foo 1)
                """);
        assertEquals("t", result.asString());

        result = ctx.eval("islisp", """
                (defgeneric foo (bar))
                (defmethod foo (bar)
                    (next-method-p))
                (defmethod foo ((bar <number>))
                    (next-method-p))
                (foo 'bar)
                """);
        assertEquals("nil", result.asString());
    }

}
