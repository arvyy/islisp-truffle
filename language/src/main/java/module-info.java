module com.github.arvyy.islisp {
  requires java.base;
  requires java.logging;
  requires transitive org.graalvm.truffle;
  provides com.oracle.truffle.api.TruffleLanguage.Provider with com.github.arvyy.islisp.ISLISPTruffleLanguageProvider;
}