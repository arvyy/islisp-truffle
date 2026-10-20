package com.github.arvyy.islisp.launcher;

import com.github.arvyy.islisp.buildinfo.BuildInfo;
import org.apache.commons.cli.*;
import org.commonmark.renderer.text.TextContentRenderer;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotAccess;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.io.IOAccess;

import java.io.*;
import java.util.ArrayList;

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
        options.addOption(helpOpt);
        options.addOption(versionOpt);

        var parser = DefaultParser.builder()
            .build();
        var commandLine = parser.parse(options, args, true);

        if (commandLine.hasOption(versionOpt)) {
            var properties = BuildInfo.getBuildProperties();
            var pw = new PrintWriter(System.out);
            var sb = new StringBuilder("ISLISP Truffle");
            var tag = properties.getProperty("git.tags");
            if (tag != null) {
                sb.append(" ").append(tag);
            } else {
                sb.append("\nGit commit ").append(properties.getProperty("git.commit.id.full"));
            }
            new HelpFormatter().printWrapped(pw, HELP_WIDTH, sb.toString());
            pw.flush();
            return;
        }

        if (commandLine.hasOption(helpOpt)) {
            new HelpFormatter().printHelp("islisp [OPTION ...] [FILE [ARG ...]]", options);
            var pw = new PrintWriter(System.out);
            // disable checkstyle so it doesn't complain about
            // long lines and trailing spaces in empty lines
            // CHECKSTYLE:OFF
            new HelpFormatter().printWrapped(pw, HELP_WIDTH, """
                Run islisp interpreter.

                Options are each of form --<option>=<value>, and are transparently passed to truffle runtime.
                
                ISLISP specific options:
                * islisp.Sourcepath - when using `require` form, islisp searches from roots provided through this option.
                Option value: set of paths, separated by `:`, defaults to `.`.
                * islisp.IncludeDocumentation - flag if documentation should be loaded. Disabling might improve startup time.
                Optional value: true or false, defaults to true.
                
                Use `--experimental-options` to enable experimental options.
                
                For other options see truffle documentation.
                
                If FILE is `-`, interpreter non-interactively evaluates standard input.
                If FILE is a path, given FILE is evaluated and the program exits.
                If FILE is not provided, interpreter enters interactive REPL mode.

                ARGs if given will be stored in *arg* dynamic variable, as a vector of strings.
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
        var passthroughCommandlineArgs = new ArrayList<String>();
        var sourceArgSeen = false;
        for (var arg: commandLine.getArgList()) {
            if (sourceArgSeen) {
                passthroughCommandlineArgs.add(arg);
                continue;
            }
            if ("--experimental-options".equals(arg)) {
                contextBuilder.allowExperimentalOptions(true);
                continue;
            }
            if (arg.startsWith("--")) {
                var e = arg.substring(2).split("=");
                var value = e.length > 1 ? e[1] : "true";
                contextBuilder.option(e[0], value);
            } else {
                sourceArg = arg;
                sourceArgSeen = true;
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
                if (line.startsWith(",d ")) {
                    var words = line.split("\\s+");
                    if (words.length != 3 && words.length != 2) {
                        System.out.println("Unrecognized ,d command format");
                        printReplHelp();
                        continue;
                    }
                    if (words.length == 2) {
                        printDocumentation(context, null, words[1]);
                    } else {
                        printDocumentation(context, words[1], words[2]);
                    }
                    continue;
                }

                Source source;
                if (line.startsWith(",l ")) {
                    source = Source
                        .newBuilder("islisp", new File(line.substring(3)))
                        .interactive(true)
                        .build();
                } else {
                    source = Source
                        .newBuilder("islisp", line, "REPL")
                        .interactive(true)
                        .cached(false)
                        .buildLiteral();
                }
                try {
                    context.eval(source);
                } catch (PolyglotException e) {
                    System.err.println(e.getMessage());
                }
            }
        } else {
            var argsSetterSrc = Source
                .newBuilder("islisp", """
                    (lambda (#:rest args)
                        (setf (dynamic *args*) (convert args <general-vector>)))
                    """, "<args init>")
                .interactive(false)
                .internal(true)
                .buildLiteral();
            var argsSetter = context.eval(argsSetterSrc);
            argsSetter.execute(passthroughCommandlineArgs.toArray());
            Source source;
            if ("-".equals(sourceArg)) {
                source = Source.newBuilder("islisp", new InputStreamReader(System.in), "<stdin>").build();
            } else {
                source = Source.newBuilder("islisp", new File(sourceArg)).build();
            }
            try {
                context.eval(source);
            } catch (PolyglotException e) {
                System.err.println(e.getMessage());
            }
        }

        context.close();
    }

    static void printDocumentation(Context context, String namespace, String name) {
        try {
            var extractor = context.eval("islisp", """
                (lambda (#:rest args)
                    (let* ((doc (if (= (length args) 2)
                                    (documentation (convert (elt args 0) <symbol>) (convert (elt args 1) <symbol>))
                                    (documentation (convert (elt args 0) <symbol>))))
                           (desc (assoc 'desc doc))
                           (result (if desc (cdr desc) "")))
                      result))
                """);
            Object[] args;
            if (namespace == null) {
                args = new Object[]{name};
            } else {
                args = new Object[]{namespace, name};
            }
            var desc = extractor.execute(args).asString();
            if (desc.isEmpty()) {
                System.out.println("No docs UwU");
            } else {
                System.out.println(markdownToPlaintext(desc));
            }
        } catch (PolyglotException e) {
            System.err.println(e.getMessage());
        }
    }

    static String markdownToPlaintext(String m) {
        var parser = org.commonmark.parser.Parser.builder().build();
        var document = parser.parse(m);
        var renderer = TextContentRenderer.builder().build();
        return renderer.render(document);
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
        // CHECKSTYLE:OFF
        new HelpFormatter().printWrapped(pw, HELP_WIDTH, """
            ,h
            This help message

            ,q
            Quit

            ,l path
            Load and evaluate file from given path

            ,d [namespace] symbol
            Lookup documentation for a symbol in scope. Optionally can specify namespace (one of: function, setf, syntax, value) to disambiguate.

            """);
        // CHECKSTYLE:ON
        pw.flush();
    }

}
