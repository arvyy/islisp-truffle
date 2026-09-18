package com.github.arvyy.islisp.lsp;

import org.eclipse.lsp4j.DidChangeConfigurationParams;
import org.eclipse.lsp4j.DidChangeWatchedFilesParams;
import org.eclipse.lsp4j.DidChangeWorkspaceFoldersParams;
import org.eclipse.lsp4j.services.WorkspaceService;

public class ISLISPWorkspaceService implements WorkspaceService {
    @Override
    public void didChangeConfiguration(DidChangeConfigurationParams params) {
        System.out.println(params);
    }

    @Override
    public void didChangeWatchedFiles(DidChangeWatchedFilesParams params) {
        System.out.println(params);
    }

    @Override
    public void didChangeWorkspaceFolders(DidChangeWorkspaceFoldersParams params) {
        System.out.println(params);
    }

}
