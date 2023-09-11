package com.github.arvyy.islisp;

import com.oracle.truffle.api.debug.Breakpoint;
import com.oracle.truffle.api.debug.Debugger;
import com.oracle.truffle.api.debug.SuspendedEvent;
import com.oracle.truffle.api.source.Source;
import org.graalvm.polyglot.Context;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

public class DebuggerTest {

    @Test
    @Disabled
    public void testInstalledDebugger() {
        var context = Context.newBuilder()
                .build();
        var debugger = context.getEngine().getInstruments().get("debugger").lookup(Debugger.class);
        var suspendedEvent = new SuspendedEvent[1];
        var session = debugger.startSession((event) -> {
            suspendedEvent[0] = event;
            assertEquals("(+ a 2)", event.getSourceSection().getCharacters());
        });
        var source = Source.newBuilder("islisp", """
                        (defun foo (a)
                            (+ a 2))
                        (foo 3)""", "test.lisp")
                .build();
        session.install(Breakpoint.newBuilder(source).lineIs(2).build());
        context.eval("islisp", source.getCharacters());
        assertNotNull(suspendedEvent[0]);
    }

    @Test
    @Disabled
    public void testCodeDebugger() {
        var context = Context.newBuilder()
                .build();
        var debugger = context.getEngine().getInstruments().get("debugger").lookup(Debugger.class);
        var suspendedEvent = new SuspendedEvent[1];
        var session = debugger.startSession((event) -> {
            suspendedEvent[0] = event;
            assertEquals("(debugger)", event.getSourceSection().getCharacters());
        });
        var source = Source.newBuilder("islisp", """
                        (defun foo (a)
                            (debugger)
                            (+ a 2))
                        (foo 3)""", "test.lisp")
                .build();
        context.eval("islisp", source.getCharacters());
        assertNotNull(suspendedEvent[0]);
    }

}
