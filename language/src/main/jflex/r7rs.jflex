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
Digit = [0-9]
HexDigit = {Digit} | [a-f]
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
Character = "#\". | "#\x" {HexScalarValue} | "#\" {CharacterName}
CharacterName = alarm | backspace | delete | escape | newline | null | return | space | tab



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

  "#u8(" {
    return new Token.ByteVectorBracketOpenToken();
  }

  "#;(" {
    return new Token.StatementCommentBracketOpenToken();
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

  {ExplicitSign}? {Digit}+ {
    return new Token.ExactNumberToken(Integer.parseInt(yytext()));
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
    "alarm" {
        yybegin(YYINITIAL);
        return new Token.CharToken(0x07);
    }
    "backspace" {
        yybegin(YYINITIAL);
        return new Token.CharToken(0x08);
    }
    "delete" {
        yybegin(YYINITIAL);
        return new Token.CharToken(0x7f);
    }
    "escape" {
        yybegin(YYINITIAL);
        return new Token.CharToken(0x1b);
    }
    "newline" {
        yybegin(YYINITIAL);
        return new Token.CharToken(0x0a);
    }
    "null" {
        yybegin(YYINITIAL);
        return new Token.CharToken(0x00);
    }
    "return" {
        yybegin(YYINITIAL);
        return new Token.CharToken(0x0d);
    }
    "space" {
        yybegin(YYINITIAL);
        return new Token.CharToken(0x20);
    }
    "tab" {
        yybegin(YYINITIAL);
        return new Token.CharToken(0x09);
    }
    "x" {HexScalarValue} {
        yybegin(YYINITIAL);
        return new Token.CharToken(Integer.parseInt(yytext().substring(1), 16));
    }
    . {
        yybegin(YYINITIAL);
        return new Token.CharToken(yytext().codePointAt(0));
    }
}

[^] {
    throw new RuntimeException("Failed to parse "+ yytext());
}