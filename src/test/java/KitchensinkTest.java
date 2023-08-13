import com.github.arvyy.islisp.ISLISPTruffleLanguage;
import com.oracle.truffle.api.TruffleLanguage;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.junit.Before;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class KitchensinkTest {

    Context context;

    @BeforeEach
    public void setUp() {
        this.context = Context.create();
    }

    @Test
    public void test() {
        var sourceCode = """
                (defun fib (n)
                    (if (> 2 n)
                        1
                        (+ (fib (- n 1)) (fib (- n 2)))))
                (print (fib 6))
                """;
        context.eval("islisp", sourceCode);
    }

}
