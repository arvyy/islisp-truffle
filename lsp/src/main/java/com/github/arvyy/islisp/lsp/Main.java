package com.github.arvyy.islisp.lsp;

import org.eclipse.lsp4j.launch.LSPLauncher;

import java.io.IOException;
import java.net.ServerSocket;

/**
 * LSP server entry point.
 */
public final class Main {

    private Main() {
    }

    /**
     * Entry point.
     * @param args cli args
     */
    public static void main(String... args) throws IOException {
        if (args.length > 0) {
            var port = args[0];
            startTCPServer(Integer.parseInt(port));
        } else {
            startStdInServer();
        }
    }

    private static void startTCPServer(int port) throws IOException {
        try (ServerSocket serverSocket = new ServerSocket(port)) {
            while (true) {
                var client = serverSocket.accept();
                var server = new ISLISPLanguageServer();
                var launcher = LSPLauncher.createServerLauncher(
                        server,
                        client.getInputStream(),
                        client.getOutputStream());
                launcher.startListening();
            }
        }
    }

    private static void startStdInServer() {
        var server = new ISLISPLanguageServer();
        var launcher = LSPLauncher.createServerLauncher(server, System.in, System.out);
        launcher.startListening();
    }

}
