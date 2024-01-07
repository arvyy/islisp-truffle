package com.github.arvyy.islisp.launcher;

import com.github.arvyy.islisp.buildinfo.BuildInfo;
import org.apache.commons.cli.*;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotAccess;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.io.IOAccess;

import java.io.*;

/**
 * Main ISLISP interpreter entrypoint.
 */
public final class Main {

    private static final int HELP_WIDTH = 80;

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
        var versionOpt = versionOpt();
        var sourcepathOpt = sourcepathOpt();

        var options = new Options();
        options.addOption(chromeDebuggerOpt);
        options.addOption(dapDebuggerOpt);
        options.addOption(helpOpt);
        options.addOption(versionOpt);
        options.addOption(sourcepathOpt);

        var parser = DefaultParser.builder()
            .build();
        var commandLine = parser.parse(options, args);

        if (commandLine.hasOption(versionOpt)) {
            var properties = BuildInfo.getBuildProperties();
            var pw = new PrintWriter(System.out);
            new HelpFormatter().printWrapped(pw, HELP_WIDTH, """
                ISLISP Truffle
                Git commit {version}
                """.replace("{version}", properties.getProperty("git.commit.id.full")));
            pw.flush();
            return;
        }

        if (commandLine.hasOption(helpOpt)) {
            new HelpFormatter().printHelp("islisp [OPTION ...] [FILE]", options);
            var pw = new PrintWriter(System.out);
            new HelpFormatter().printWrapped(pw, HELP_WIDTH, """
                Run islisp interpreter.
                If FILE is `-`, interpreter non-interactively evaluates standard input.
                If FILE is a path, given FILE is evaluated and the program exits.
                If FILE is not provided, interpreter enters interactive REPL mode.
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

        if (commandLine.hasOption(sourcepathOpt)) {
            contextBuilder.option("islisp.Sourcepath", sourcepathOpt.getValue());
        }

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
        } else if (commandLine.getArgList().get(0).equals("-")) {
            var source = Source.newBuilder("islisp", new InputStreamReader(System.in), "<stdin>").build();
            context.eval(source);
        } else {
            var source = Source.newBuilder("islisp", new File(commandLine.getArgList().get(0))).build();
            context.eval(source);
        }
        context.close();
    }

    static Option chromeDebuggerOpt() {
        return Option.builder()
            .option("d")
            .longOpt("debug-chrome")
            .optionalArg(true)
            .argName("PORT")
            .desc("Run in debugger mode using chrome debugger protocol. "
                + "If port not provided, defaults to 9229.")
            .build();
    }

    static Option dapDebuggerOpt() {
        return Option.builder()
            .option("dap")
            .longOpt("debug-dap")
            .optionalArg(true)
            .argName("PORT")
            .desc("Run in debugger mode using DAP. "
                + "If port not provided, defaults to 4711.")
            .build();
    }

    static Option helpOpt() {
        return Option.builder()
            .option("h")
            .longOpt("help")
            .desc("Show help and exit.")
            .build();
    }

    static Option versionOpt() {
        return Option.builder()
            .option("v")
            .longOpt("version")
            .desc("Show version information and exit.")
            .build();
    }

    static Option sourcepathOpt() {
        return Option.builder()
            .option("sp")
            .longOpt("sourcepath")
            .desc("Source paths, separated by system path separator, relative which to search for required modules.")
            .build();
    }

    static void printReplHelp() {
        var pw = new PrintWriter(System.out);
        new HelpFormatter().printWrapped(pw, HELP_WIDTH, """
            ,h - This help message
            ,q - Quit
            ,l path - Load and evaluate file from given path
            """);
        pw.flush();
    }

}
