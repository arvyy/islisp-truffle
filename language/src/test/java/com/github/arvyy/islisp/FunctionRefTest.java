package com.github.arvyy.islisp;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;

public class FunctionRefTest extends AbstractTestSuite {

    @Test
    public void testFullSyntax() {
        assertEquals(3, ctx.eval("islisp", """
                (funcall (function +) 1 2)
                """).asInt());
    }

    @Test
    public void testShortSyntax() {
        assertEquals(3, ctx.eval("islisp", """
                (funcall #'+ 1 2)
                """).asInt());
    }

}
