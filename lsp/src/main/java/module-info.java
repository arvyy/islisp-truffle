open module com.github.arvyy.islisp.lsp {
    requires org.graalvm.polyglot;
    requires org.graalvm.truffle;
    requires com.github.arvyy.islisp;
    requires org.eclipse.lsp4j;
    requires org.eclipse.lsp4j.jsonrpc;
    requires com.google.gson;
    exports com.github.arvyy.islisp.lsp;
}
