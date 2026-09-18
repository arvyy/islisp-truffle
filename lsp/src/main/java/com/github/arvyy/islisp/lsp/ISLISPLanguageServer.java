package com.github.arvyy.islisp.lsp;

import com.github.arvyy.islisp.ISLISPTruffleLanguage;
import org.eclipse.lsp4j.*;
import org.eclipse.lsp4j.services.LanguageServer;
import org.eclipse.lsp4j.services.TextDocumentService;
import org.eclipse.lsp4j.services.WorkspaceService;
import org.graalvm.polyglot.Context;

import java.net.URL;
import java.nio.file.Paths;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;

public class ISLISPLanguageServer implements LanguageServer {

    private Context context;
    private ISLISPTruffleLanguage lang;

    public <T> T withLang(Function<ISLISPTruffleLanguage, T> f) {
        context.enter();
        return f.apply(lang);
    }

    @Override
    public CompletableFuture<InitializeResult> initialize(InitializeParams initializeParams) {
        String sourcePath;
        try {
            sourcePath = Paths.get(new URL(initializeParams.getWorkspaceFolders().getFirst().getUri()).toURI()).toString();
        } catch (Exception e) {
            throw new RuntimeException("Failed to parse workspace location", e);
        }
        context = Context.newBuilder("islisp")
                .option("islisp.Sourcepath", sourcePath)
                .build();
        context.enter();
        context.initialize("islisp");
        lang = ISLISPTruffleLanguage.REFERENCE.get(null);
        var result = new InitializeResult();
        var info = new ServerInfo();
        info.setName("ISLISP-lsp");
        result.setServerInfo(info);
        var capabilities = new ServerCapabilities();
        capabilities.setHoverProvider(true);
        capabilities.setDefinitionProvider(true);
        capabilities.setTextDocumentSync(TextDocumentSyncKind.Full);
        result.setCapabilities(capabilities);
        return CompletableFuture.completedFuture(result);
    }

    @Override
    public CompletableFuture<Object> shutdown() {
        return CompletableFuture.completedFuture(null);
    }

    @Override
    public void exit() {
        lang = null;
        context.close();
    }

    @Override
    public TextDocumentService getTextDocumentService() {
        return new ISLISPTextDocumentService(this);
    }

    @Override
    public WorkspaceService getWorkspaceService() {
        return new ISLISPWorkspaceService();
    }
}
