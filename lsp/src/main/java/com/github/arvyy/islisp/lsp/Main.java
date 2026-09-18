package com.github.arvyy.islisp.lsp;

import org.eclipse.lsp4j.launch.LSPLauncher;

import java.io.IOException;
import java.net.ServerSocket;

public class Main {

    public static void main(String... args) throws IOException {
        startTCPServer();
    }

    private static void startTCPServer() throws IOException {
        ServerSocket serverSocket = new ServerSocket(8123);
        while (true) {
            var client = serverSocket.accept();
            var server = new ISLISPLanguageServer();
            var launcher = LSPLauncher.createServerLauncher(server, client.getInputStream(), client.getOutputStream());
            launcher.startListening();
        }
    }

}
