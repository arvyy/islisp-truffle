module com.github.arvyy.islisp {
  requires java.base;
  requires java.logging;
  requires jdk.unsupported;
  requires org.graalvm.polyglot;
  requires org.graalvm.truffle;
  provides com.oracle.truffle.api.provider.TruffleLanguageProvider with com.github.arvyy.islisp.ISLISPTruffleLanguageProvider;
}