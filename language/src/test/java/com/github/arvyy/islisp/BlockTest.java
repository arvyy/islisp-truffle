package com.github.arvyy.islisp;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class BlockTest extends AbstractTestSuite {

    @Test
    public void testBlock() {
        assertEquals(6, ctx.eval("islisp", """
                (block x
                    (+ 10 (return-from x 6) 22)) ;;; Bad programming style
                """).asInt());
    }

}
