package com.github.arvyy.islisp;

import org.graalvm.polyglot.Context;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class DirectLambdaCallTest {

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
    public void testLambdaCall() {
        var res = ctx.eval("islisp", """
                ((lambda (a) (+ 1 a)) 2)
                """);
        assertEquals(res.asInt(), 3);
    }

}
