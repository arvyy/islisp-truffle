package com.github.arvyy.islisp;

import com.github.arvyy.islisp.runtime.LispInteger;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.TypeLiteral;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class IfNodeTest {

    Context ctx;

    @BeforeEach
    void init() {
        ctx = Context.create();
    }

    @AfterEach
    void cleanup() {
        if (ctx != null) {
            ctx.close();
            ctx = null;
        }
    }

    @Test
    public void testTruthy() {
        var result = ctx.eval("islisp", """
                (if (> 3 2) 1 0)
                """);
        assertEquals(1, result.asInt());
    }

    @Test
    public void testFalsy() {
        var result = ctx.eval("islisp", """
                (if (> 2 3) 1 0)
                """);
        assertEquals(0, result.asInt());
    }

    @Test
    public void testImplicitFalsy() {
        var result = ctx.eval("islisp", """
                (if (> 2 3) 1)
                """);
        assertEquals("nil", result.asString());
    }

}
