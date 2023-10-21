package com.github.arvyy.islisp.parser;

import com.github.arvyy.islisp.parser.Token;
import java.util.Optional;
import java.io.IOException;

%%

%class ISLISPLexer
%implements com.github.arvyy.islisp.parser.Lexer
%function readToken
%public
%unicode
%type Token
%apiprivate

%line
%column
%unicode

%{
    StringBuilder sb;
    int commentNesting = 0;
    boolean foldCase = false;

    Token peeked = null;

    @Override
    public Optional<Token> getToken() {
        try {
            if (peeked != null) {
                var p = peeked;
                peeked = null;
                return Optional.of(p);
            }
            var t = readToken();
            return Optional.ofNullable(t);
        } catch (Exception e) {
            throw new RuntimeException(e); //TODO
        }
    }

    public int getLine() {
        return yyline + 1;
    }

    public int getColumn() {
        return yycolumn + 1;
    }

    public int getLength() {
        return yylength();
    }

    @Override
    public Optional<Token> peekToken() {
        if (peeked != null)
            return Optional.of(peeked);
        Token t = null;
        try {
            t = readToken();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        if (t != null)
            peeked = t;
        else
            peeked = null;
        return Optional.ofNullable(t);
    }
%}

IntralineWhitespace = [ \t]
WhiteSpace = {LineEnding} | {IntralineWhitespace}
LineEnding = \r | \n | \r\n
EndOfLineComment     = ";" {InputCharacter}* {LineEnding}?
InputCharacter = [^\r\n]
DirectiveFoldCase = "#!fold-case"
DirectiveNoFoldCase = "#!no-fold-case"
Directive = {DirectiveFoldCase} | {DirectiveNoFoldCase}
Atmosphere = {WhiteSpace} | {Comment} | {Directive}
IntertokenSpace = {Atmosphere}*

Identifier = {Initial} {Subsequent}* | {PeculiarIdentifier}
Initial = {Letter} | {SpecialInitial}
Letter = [a-zA-Z]
SpecialInitial = "!" | "$" | "%" | "&" | "*" | "/" | ":" | "<" | "=" | ">" | "?" | "^" | "_" | "~"
Subsequent = {Initial} | {Digit} | {SpecialSubsequent}
BinDigit = [0-1]
Digit = [0-9]
OctDigit = [0-7]
HexDigit = {Digit} | [a-f] | [A-F]
ExplicitSign = "+" | "-"
SpecialSubsequent = {ExplicitSign} | "." | "@"
InlineHexEscape = "\x" {HexScalarValue} ";"
HexScalarValue = {HexDigit}+
MnemonicEscape = \a | \b | \t | \n | \r
PeculiarIdentifier =
    {ExplicitSign} |
    {ExplicitSign} {SignSubsequent} {Subsequent}* |
    {ExplicitSign} "." {DotSubsequent} {Subsequent}* |
    "." {DotSubsequent} {Subsequent}*

DotSubsequent = {SignSubsequent} | "."
SignSubsequent = {Initial} | {ExplicitSign} | "@"
SymbolElement = [^|\\] | {InlineHexEscape} | {MnemonicEscape} | \|
Character = "#\". | "#\" {CharacterName}
CharacterName = newline | space | tab



%state BLOCK_COMMENT
%state STRING
%state PIPED_IDENTIFIER
%state PIPED_IDENTIFIER_INLINEHEX
%state CHAR

%%

<YYINITIAL> {
    {Identifier} {
        String text = yytext();
        return new Token.IdentifierToken(yytext().toLowerCase());
    }

  "#(" {
    return new Token.VectorBracketOpenToken();
  }

  "(" {
    return new Token.BracketOpenToken();
  }

  ")" {
    return new Token.BracketCloseToken();
  }

  "#'" {
    return new Token.FunctionRefToken();
  }

  "'" {
    return new Token.QuoteToken();
  }

  "`" {
    return new Token.QuasiquoteToken();
  }

  "," {
    return new Token.UnquoteToken();
  }

  ",@" {
    return new Token.UnquoteSpliceToken();
  }

  "." {
    return new Token.PeriodToken();
  }

  \" {
    sb = new StringBuilder();
    yybegin(STRING);
  }

  \| {
    sb = new StringBuilder();
    yybegin(PIPED_IDENTIFIER);
  }

  {EndOfLineComment} {}

  "#|" {
    yybegin(BLOCK_COMMENT);
    commentNesting = 1;
  }

  "#\\" {
    yybegin(CHAR);
  }

  {ExplicitSign}? {Digit}+ "." {Digit}+ "e" {ExplicitSign}? {Digit}+    |
  {ExplicitSign}? {Digit}+ "." {Digit}+ "E" {ExplicitSign}? {Digit}+    |
  {ExplicitSign}? {Digit}+ "e" {ExplicitSign}? {Digit}+                 |
  {ExplicitSign}? {Digit}+ "E" {ExplicitSign}? {Digit}+                 |
  {ExplicitSign}? {Digit}+ "." {Digit}+ {
    return new Token.InexactNumberToken(Double.valueOf(yytext()));
  }

  ("#B" | "#b") {ExplicitSign}? {BinDigit}+ {
    return new Token.ExactNumberToken(Integer.parseInt(yytext().substring(2), 2));
  }

  ("#o" | "#O") {ExplicitSign}? {OctDigit}+ {
    return new Token.ExactNumberToken(Integer.parseInt(yytext().substring(2), 8));
  }

  {ExplicitSign}? {Digit}+ {
    return new Token.ExactNumberToken(Integer.parseInt(yytext()));
  }

  ("#x" | "#X") {ExplicitSign}? {HexDigit}+ {
    return new Token.ExactNumberToken(Integer.parseInt(yytext().substring(2), 16));
  }

  {WhiteSpace} {}
}

<STRING> {
  \" {
    yybegin(YYINITIAL);
    return new Token.StringToken(sb.toString());
  }
  [^\n\r\"\\]+ {
    sb.append(yytext());
  }
}

<BLOCK_COMMENT> {
    "#|" {
        commentNesting++;
    }
    "|#" {
        commentNesting--;
        if (commentNesting == 0) {
            yybegin(YYINITIAL);
        }
    }
    [^] {}
}

<PIPED_IDENTIFIER> {
    \| {
        yybegin(YYINITIAL);
        return new Token.IdentifierToken(sb.toString());
    }
    "\x" {
        yybegin(PIPED_IDENTIFIER_INLINEHEX);
    }
}

<PIPED_IDENTIFIER_INLINEHEX> {
    {HexScalarValue} {
        sb.appendCodePoint(Integer.parseInt(yytext(), 16));
    }
    ";" {
        yybegin(PIPED_IDENTIFIER);
    }
}

<CHAR> {
    "newline" {
        yybegin(YYINITIAL);
        return new Token.CharToken(0x0a);
    }
    "space" {
        yybegin(YYINITIAL);
        return new Token.CharToken(0x20);
    }
    . {
        yybegin(YYINITIAL);
        return new Token.CharToken(yytext().codePointAt(0));
    }
}

[^] {
    throw new RuntimeException("Failed to parse "+ yytext());
}