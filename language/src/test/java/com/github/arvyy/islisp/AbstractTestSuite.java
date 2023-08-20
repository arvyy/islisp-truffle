package com.github.arvyy.islisp;

import org.graalvm.polyglot.Context;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;

public abstract class AbstractTestSuite {

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

}
