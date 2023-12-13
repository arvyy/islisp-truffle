package com.github.arvyy.islisp;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotAccess;
import org.graalvm.polyglot.Source;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class ExternalTest {

    @TestFactory
    public Stream<DynamicTest> externalTestsPortable() throws IOException {
        return Files.list(Path.of("../tests"))
                .filter(p -> p.getFileName().toString().endsWith(".lisp"))
                .map(lispFile -> {
                    return DynamicTest.dynamicTest("Test " + lispFile.getFileName(), () -> executeTest(lispFile));
                });
    }

    @TestFactory
    public Stream<DynamicTest> externalTestsUnportable() throws IOException {
        return Files.list(Path.of("../tests2"))
            .filter(p -> p.getFileName().toString().endsWith(".lisp"))
            .map(lispFile -> {
                return DynamicTest.dynamicTest("Unportable test " + lispFile.getFileName(), () -> executeTest(lispFile));
            });
    }

    void executeTest(Path lispFile) throws Throwable {
        var srcName = lispFile.getFileName().toString();
        var resultName = srcName.substring(0, srcName.length() - ".lisp".length()) + ".expect.txt";
        var resultFile = lispFile.getParent().resolve(resultName);
        if (!Files.exists(resultFile)) {
            throw new RuntimeException("Result file does not exist: " + resultName);
        }
        var output = new ByteArrayOutputStream();
        var ctxBuilder = Context.newBuilder()
            .in(new ByteArrayInputStream(new byte[0]))
            .out(output)
            .allowPolyglotAccess(PolyglotAccess.ALL)
            .allowNativeAccess(true);
        try (var ctx = ctxBuilder.build()) {
            ctx.eval(Source.newBuilder("islisp", lispFile.toFile()).build());
            var expected = Files.readString(resultFile);
            var actual = output.toString(StandardCharsets.UTF_8);
            assertEquals(expected, actual);
        }
    }

}
