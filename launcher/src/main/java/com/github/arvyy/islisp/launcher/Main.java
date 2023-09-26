package com.github.arvyy.islisp.launcher;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;

import java.io.File;
import java.io.IOException;

public final class Main {

    private Main() { }

    public static void main(String[] args) throws IOException {

        var source = Source.newBuilder("islisp", new File(args[0]))
                .build();

        var port = "4242";

        var context = Context.newBuilder()
                .in(System.in)
                .out(System.out)
                //.option("inspect", port)
                .build();

        context.eval(source);
    }

}
