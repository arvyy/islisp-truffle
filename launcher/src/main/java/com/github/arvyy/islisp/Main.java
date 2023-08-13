package com.github.arvyy.islisp;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;

import java.io.File;
import java.io.IOException;

public class Main {

    public static void main(String[] args) throws IOException {

        var source = Source.newBuilder("islisp", new File(args[0]))
                .build();

        var port = "4242";
        var path = java.util.UUID.randomUUID().toString();

        var context = Context.newBuilder()
                .in(System.in)
                .out(System.out)
                /*
                .option("inspect", port)
                .option("inspect.Suspend", "true")
                .option("inspect.Path", path)
                 */
                .build();


        String hostAdress = "localhost";
        String url = String.format(
                "chrome-devtools://devtools/bundled/js_app.html?ws=%s:%s/%s",
                hostAdress, port, path);
        System.out.println("Debugger at: " + url);

        var result = context.eval(source);
        System.out.println(result);
    }

}
