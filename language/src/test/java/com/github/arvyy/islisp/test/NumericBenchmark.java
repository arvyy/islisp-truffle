package com.github.arvyy.islisp.test;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.infra.Blackhole;

import java.io.IOException;

@Warmup(iterations = 2)
@Measurement(iterations = 2)
@Fork(value = 1)
@BenchmarkMode(Mode.AverageTime)
public class NumericBenchmark {

    public static void main(String... args) throws IOException {
        org.openjdk.jmh.Main.main(args);
    }

    @State(Scope.Benchmark)
    public static class BenchmarkState {
        Context ctx;
        Value fib;

        @Setup
        public void setup() {
            ctx = Context.newBuilder()
                .in(System.in)
                .out(System.out)
                .err(System.err)
                .build();
            fib = ctx.eval(Source.create("islisp", """
                (defun fib (n)
                    (if (<= n 1)
                        1
                        (+ (fib (- n 1)) (fib (- n 2)))))
                #'fib
                """));
        }

        @TearDown
        public void teardown() {
            ctx.close();
        }

    }

    @Benchmark
    public void fib(Blackhole bh, BenchmarkState state) {
        for (var i = 0; i < 5; i++)
            bh.consume(state.fib.execute(30));
    }

}
