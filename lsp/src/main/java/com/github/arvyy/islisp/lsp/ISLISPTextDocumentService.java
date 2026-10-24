package com.github.arvyy.islisp.lsp;

import org.eclipse.lsp4j.*;
import org.eclipse.lsp4j.jsonrpc.messages.Either;
import org.eclipse.lsp4j.services.TextDocumentService;

import com.github.arvyy.islisp.Utils;
import com.github.arvyy.islisp.runtime.ISLISPModule.DocumentationEntry;
import com.github.arvyy.islisp.runtime.Symbol;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

/**
 * ISLISP text document service implementation.
 */
public class ISLISPTextDocumentService implements TextDocumentService {

    private final ISLISPLanguageServer server;
    private List<CompletionItem> items = null;

    /**
     * Service constructor.
     * @param server reference to language server implementation.
     */
    public ISLISPTextDocumentService(ISLISPLanguageServer server) {
        this.server = server;
    }

    @Override
    public CompletableFuture<Either<List<CompletionItem>, CompletionList>> completion(CompletionParams position) {
        CompletableFuture<List<CompletionItem>> itemsFuture;
        if (items == null) {
            itemsFuture = CompletableFuture.supplyAsync(() -> {
                buildCompletionItems();
                return items;
            });
        } else {
            itemsFuture = CompletableFuture.completedFuture(items);
        }
        return itemsFuture.thenApply(Either::forLeft);
    }

    void buildCompletionItems() {
        server.withLang((ctx, lang) -> {
            var root = ctx.getModule("ROOT");
            var docs = root.getModuleDocumentations();
            items = docs.stream()
                .map(this::docToCompletionItem)
                .flatMap(Optional::stream)
                .toList();
            return null;
        });
    }

    Optional<CompletionItem> docToCompletionItem(DocumentationEntry e) {
        try {
            var doc = Utils.alistToMap(e.doc());
            var i = new CompletionItem();
            i.setLabel(e.symbol().name());
            i.setFilterText("(" + e.symbol().name() + ")");
            if (doc.containsKey("syntax")) {
                i.setInsertText((String) doc.get("syntax"));
            }
            if (doc.containsKey("kind")) {
                var kind = (Symbol) doc.get("kind");
                var details = new CompletionItemLabelDetails();
                details.setDetail(kind.name());
                i.setLabelDetails(details);
            }
            var desc = (String) doc.get("desc");
            i.setDocumentation(desc);
            return Optional.of(i);
        } catch (Exception ex) {
            ex.printStackTrace();
            return Optional.empty();
        }
    }

    @Override
    public void didOpen(DidOpenTextDocumentParams params) {
    }

    @Override
    public void didChange(DidChangeTextDocumentParams params) {
    }

    @Override
    public void didClose(DidCloseTextDocumentParams params) {

    }

    @Override
    public void didSave(DidSaveTextDocumentParams params) {
    }

}
