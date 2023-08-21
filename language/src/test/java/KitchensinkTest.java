import com.oracle.truffle.api.debug.Breakpoint;
import com.oracle.truffle.api.debug.Debugger;
import com.oracle.truffle.api.debug.SuspendedEvent;
import com.oracle.truffle.api.source.Source;
import org.graalvm.polyglot.Context;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class KitchensinkTest {

    Context context;

    @BeforeEach
    public void setUp() {
        this.context = Context.newBuilder()
                .build();
    }

    @Test
    public void test() {
        var sourceCode = """
                (block a
                    (return-from b 1))
                (print (foo '(2)))
                """;
        context.eval("islisp", sourceCode);
    }

    SuspendedEvent suspendedEvent;

    @Test
    public void testDebugger() {
        var debugger = context.getEngine().getInstruments().get("debugger").lookup(Debugger.class);
        var session = debugger.startSession((event) -> {
            suspendedEvent = event;
            System.out.println(event);
            suspendedEvent = null;
        });
        var source = Source.newBuilder("islisp", """
                        (defun foo (a) 
                            (debugger)
                            (+ a 2))
                        (foo 3)""", "test.lisp")
                        .build();
        session.install(Breakpoint.newBuilder(source).lineIs(1).build());
        session.suspendNextExecution();
        System.out.println(context.eval("islisp", source.getCharacters()));
    }

}
