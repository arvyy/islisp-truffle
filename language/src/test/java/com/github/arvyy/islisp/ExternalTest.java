package com.github.arvyy.islisp;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotAccess;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.io.IOAccess;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestFactory;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class ExternalTest {

    @TestFactory
    public Stream<DynamicTest> externalTestsPortable() throws IOException {
        return Files.list(Path.of("../tests/portable"))
                .filter(p -> p.getFileName().toString().endsWith(".lisp"))
                .map(lispFile -> {
                    return DynamicTest.dynamicTest("Test " + lispFile.getFileName(), () -> executeTest(lispFile));
                });
    }

    @TestFactory
    public Stream<DynamicTest> externalTestsUnportable() throws IOException {
        return Files.list(Path.of("../tests/nonportable"))
            .filter(p -> p.getFileName().toString().endsWith(".lisp"))
            .map(lispFile -> {
                return DynamicTest.dynamicTest("Nonportable test " + lispFile.getFileName(), () -> executeTest(lispFile));
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
            .allowIO(IOAccess.ALL)
            .allowNativeAccess(true)
            .option("islisp.Sourcepath", "../tests/util");
        try (var ctx = ctxBuilder.build()) {
            ctx.eval(Source.newBuilder("islisp", lispFile.toFile()).build());
            var expected = Files.readString(resultFile);
            var actual = output
                .toString(StandardCharsets.UTF_8)
                .replaceAll("\\r", "")
                .trim();
            assertEquals(expected, actual);
        }
    }

    @Test
    public void moduleTest() throws IOException {
        var srcName = "../tests/nonportable/modulestest/main.lisp";
        var output = new ByteArrayOutputStream();
        var isWindows = System.getProperty("os.name").startsWith("Windows");
        var sourcePath = List.of(
            "../tests/util",
            "../tests/nonportable/modulestest/root1",
            "../tests/nonportable/modulestest/root2")
            .stream()
            .collect(Collectors.joining(isWindows ? ";" : ":"));
        var ctxBuilder = Context.newBuilder()
            .in(new ByteArrayInputStream(new byte[0]))
            .out(output)
            .allowPolyglotAccess(PolyglotAccess.ALL)
            .allowIO(IOAccess.ALL)
            .allowNativeAccess(true)
            .option("islisp.Sourcepath", sourcePath);
        try (var ctx = ctxBuilder.build()) {
            ctx.eval(Source.newBuilder("islisp", new File(srcName)).build());
            var expected = "modulestest end";
            var actual = output.toString(StandardCharsets.UTF_8);
            assertEquals(expected, actual);
        }
    }

}
