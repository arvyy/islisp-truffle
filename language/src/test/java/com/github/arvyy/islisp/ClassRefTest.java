package com.github.arvyy.islisp;

import com.github.arvyy.islisp.nodes.ISLISPClassRef;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class ClassRefTest extends AbstractTestSuite {

    @Test
    public void testClassRef() {
        assertEquals("t", ctx.eval("islisp", """
                (eq (class <integer>) (class-of 1))
                """).asString());
    }

}
