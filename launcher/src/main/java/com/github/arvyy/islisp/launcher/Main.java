package com.github.arvyy.islisp.launcher;

import org.apache.commons.cli.*;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotAccess;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.io.IOAccess;

import java.io.*;

/**
 * Main ISLISP interpreter entrypoint.
 */
public final class Main {

    private Main() { }

    /**
     * Main ISLISP interpreter entrypoint.
     *
     * @param args commandline args
     * @throws IOException
     */
    public static void main(String[] args) throws IOException, ParseException {

        var chromeDebuggerOpt = chromeDebuggerOpt();
        var dapDebuggerOpt = dapDebuggerOpt();
        var helpOpt = helpOpt();

        var options = new Options();
        options.addOption(chromeDebuggerOpt);
        options.addOption(dapDebuggerOpt);
        options.addOption(helpOpt);

        var parser = DefaultParser.builder()
            .build();
        var commandLine = parser.parse(options, args);

        if (commandLine.hasOption(helpOpt)) {
            new HelpFormatter().printHelp("islisp [FILE]", options);
            var pw = new PrintWriter(System.out);
            new HelpFormatter().printWrapped(pw, 80, """
                Run islisp interpreter. If FILE is provided, given FILE is evaluated and the program exits.
                If FILE is not provided, interpreter enters repl mode.
                """);
            pw.flush();
            return;
        }

        var contextBuilder = Context.newBuilder()
            .in(System.in)
            .out(System.out)
            .err(System.err)
            .allowIO(IOAccess.ALL)
            .allowNativeAccess(true)
            .allowPolyglotAccess(PolyglotAccess.ALL);

        if (commandLine.hasOption(chromeDebuggerOpt)) {
            if (chromeDebuggerOpt.hasArg()) {
                contextBuilder.option("inspect", chromeDebuggerOpt.getValue());
            } else {
                contextBuilder.option("inspect", "9229");
            }
        }

        if (commandLine.hasOption(dapDebuggerOpt)) {
            if (dapDebuggerOpt.hasArg()) {
                contextBuilder.option("dap", dapDebuggerOpt.getValue());
            } else {
                contextBuilder.option("dap", "4711");
            }
        }
        var context = contextBuilder.build();

        if (commandLine.getArgList().isEmpty()) {
            var w = new OutputStreamWriter(System.out);
            var r = new BufferedReader(new InputStreamReader(System.in));
            w.write("ISLISP interpreter.\nUse ,h for help.\n\n");
            w.flush();
            int prompt = 0;
            while (true) {
                prompt++;
                w.write("#" + prompt + "> ");
                w.flush();
                var line = r.readLine();
                if (line == null) {
                    break;
                }
                if (line.equals(",h")) {
                    printReplHelp();
                    continue;
                }
                if (line.equals(",q")) {
                    break;
                }

                Source source;
                if (line.startsWith(",l ")) {
                    source = Source
                        .newBuilder("islisp", new File(line.substring(3)))
                        .build();
                } else {
                    source = Source
                        .newBuilder("islisp", line, "<repl " + "#" + prompt + ">")
                        .interactive(true)
                        .cached(false)
                        .buildLiteral();
                }
                try {
                    context.eval(source);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        } else {
            var source = Source.newBuilder("islisp", new File(commandLine.getArgList().get(0))).build();
            context.eval(source);
        }
    }

    static Option chromeDebuggerOpt() {
        return Option.builder()
            .argName("d")
            .longOpt("debug-chrome")
            .desc("Run in debugger mode using chrome debugger protocol")
            .build();
    }

    static Option dapDebuggerOpt() {
        return Option.builder()
            .argName("dap")
            .longOpt("debug-dap")
            .desc("Run in debugger mode using DAP")
            .build();
    }

    static Option helpOpt() {
        return Option.builder()
            .argName("h")
            .longOpt("help")
            .build();
    }

    static void printReplHelp() {
        var pw = new PrintWriter(System.out);
        new HelpFormatter().printWrapped(pw, 80, """
            ,h - This help message
            ,q - Quit
            ,l path - Load and evaluate file from given path
            """);
        pw.flush();
    }

}
