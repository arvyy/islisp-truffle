package com.github.arvyy.islisp.lsp;

import com.oracle.truffle.api.source.Source;
import org.eclipse.lsp4j.*;
import org.eclipse.lsp4j.jsonrpc.messages.Either;
import org.eclipse.lsp4j.services.TextDocumentService;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public class ISLISPTextDocumentService implements TextDocumentService {

    private final ISLISPLanguageServer server;

    public ISLISPTextDocumentService(ISLISPLanguageServer server) {
        this.server = server;
    }

    @Override
    public void didOpen(DidOpenTextDocumentParams params) {
        server.withLang(l -> {
            l.loadModuleForLSP(
                params.getTextDocument().getUri(),
                Source.newBuilder("islisp", params.getTextDocument().getText(), params.getTextDocument().getUri())
                    .build());
            return null;
        });
    }

    @Override
    public void didChange(DidChangeTextDocumentParams params) {
        System.out.println(params);
    }

    @Override
    public void didClose(DidCloseTextDocumentParams params) {

    }

    @Override
    public void didSave(DidSaveTextDocumentParams params) {

    }

    @Override
    public CompletableFuture<Hover> hover(HoverParams params) {
        System.out.println(params);
        var hover = new Hover();
        hover.setContents(List.of(Either.forLeft("Test hoveaar")));
        return CompletableFuture.completedFuture(hover);
    }

    @Override
    public CompletableFuture<Either<List<? extends Location>, List<? extends LocationLink>>> definition(DefinitionParams params) {
        System.out.println(params);
        server.withLang(l -> {
            l.findExpressionNode(Path.of(params.getTextDocument().getUri()), params.getPosition().getLine(), params.getPosition().getCharacter()).ifPresent(node -> {
                System.out.println("Found node: " + node);
            });
            return null;
        });
        return CompletableFuture.completedFuture(Either.forLeft(List.of()));
    }
}
