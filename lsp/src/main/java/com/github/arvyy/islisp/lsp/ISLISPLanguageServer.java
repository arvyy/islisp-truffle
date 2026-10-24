package com.github.arvyy.islisp.lsp;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.ISLISPTruffleLanguage;
import org.eclipse.lsp4j.*;
import org.eclipse.lsp4j.services.LanguageServer;
import org.eclipse.lsp4j.services.TextDocumentService;
import org.eclipse.lsp4j.services.WorkspaceService;
import org.graalvm.polyglot.Context;

import java.net.URL;
import java.nio.file.Paths;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.function.BiFunction;

/**
 * ISLISP language server.
 */
public class ISLISPLanguageServer implements LanguageServer {

    private Context context;
    private ISLISPTruffleLanguage lang;
    private ISLISPContext islispContext;

    /**
     * Execute function in an active islisp context.
     * @param f function to be called
     * @param <T> return type
     * @return value that f returned
     */
    public <T> T withLang(BiFunction<ISLISPContext, ISLISPTruffleLanguage, T> f) {
        context.enter();
        var result = f.apply(islispContext, lang);
        context.leave();
        return result;
    }

    @Override
    public CompletableFuture<InitializeResult> initialize(InitializeParams initializeParams) {
        String sourcePath;
        try {
            sourcePath = Paths.get(new URL(initializeParams.getWorkspaceFolders().getFirst().getUri()).toURI())
                .toString();
        } catch (Exception e) {
            throw new RuntimeException("Failed to parse workspace location", e);
        }
        context = Context.newBuilder("islisp")
                .option("islisp.Sourcepath", sourcePath)
                .build();
        context.enter();
        context.initialize("islisp");
        lang = ISLISPTruffleLanguage.REFERENCE.get(null);
        islispContext = ISLISPContext.get(null);
        var result = new InitializeResult();
        var info = new ServerInfo();
        info.setName("ISLISP-lsp");
        result.setServerInfo(info);
        var capabilities = new ServerCapabilities();
        var completionOpts = new CompletionOptions();
        completionOpts.setTriggerCharacters(List.of("("));
        capabilities.setCompletionProvider(completionOpts);
        result.setCapabilities(capabilities);
        context.leave();
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
