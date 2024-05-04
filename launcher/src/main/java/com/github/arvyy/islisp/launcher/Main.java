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

        var helpOpt = helpOpt();
        var versionOpt = versionOpt();

        var options = new Options();
        //options.addOption(helpOpt);
        options.addOption(versionOpt);

        var parser = DefaultParser.builder()
            .build();
        var commandLine = parser.parse(options, args, true);

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
            // disable checkstyle so it doesn't complain about
            // long lines and trailing spaces in empty lines
            // CHECKSTYLE:OFF
            new HelpFormatter().printWrapped(pw, HELP_WIDTH, """
                Run islisp interpreter.

                Options are each of form --<option>=<value>, and are transparently passed to truffle runtime.
                
                ISLISP specific options:
                * islisp.Sourcepath - when using `require` form, islisp searches from roots provided through this option.
                Option value: set of paths, separated by `:`.
                
                For other options see truffle documentation.
                
                If FILE is `-`, interpreter non-interactively evaluates standard input.
                If FILE is a path, given FILE is evaluated and the program exits.
                If FILE is not provided, interpreter enters interactive REPL mode.
                """);
            // CHECKSTYLE:ON
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

        String sourceArg = null;
        for (var arg: commandLine.getArgList()) {
            if (arg.startsWith("--")) {
                var e = arg.substring(2).split("=");
                var value = e.length > 1 ? e[1] : "true";
                contextBuilder.option(e[0], value);
            } else {
                sourceArg = arg;
            }
        }

        var context = contextBuilder.build();

        if (sourceArg == null) {
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
        } else if ("-".equals(sourceArg)) {
            var source = Source.newBuilder("islisp", new InputStreamReader(System.in), "<stdin>").build();
            context.eval(source);
        } else {
            var source = Source.newBuilder("islisp", new File(sourceArg)).build();
            context.eval(source);
        }
        context.close();
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
