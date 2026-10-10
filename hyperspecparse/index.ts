import { load, type Element, type Cheerio } from 'cheerio';
import type CheerioAPI = require('cheerio');
import * as fs from 'fs';
import { NodeHtmlMarkdown, NodeHtmlMarkdownOptions } from 'node-html-markdown'

type DocEntry = {
  name: string;
  desc: string;
} & ({
  kind: 'function' | 'local-function' | 'generic-function',
  syntax: string;
  minArgs: number;
  maxArgs: number | null;
  returnType: string | null;
} | {
  kind: 'syntax' | 'setf',
  syntax: string;
  returnType: string | null;
} | {
  kind: 'constant',
  valueType: string;
});

function main() {
  const html = fs.readFileSync('./islisp-v23.html');
  const $ = load(html);

  let docs: DocEntry[] = [];
  for (const genDef of $('.definition-generic')) {
    const name = $(genDef).find('.operator').text();
    if (name == 'generic-function-name') continue;
    let syntax = $(genDef).text();
    syntax = syntax.split('→')[0]!.trim();
    let minArgs = 0;
    let maxArgs: number | null = 0;
    for (const arg of $(genDef).find('.arg')) {
      if (arg.nextSibling?.type == 'text') {
        const text = arg.nextSibling.data;
        if (text.startsWith('*')) {
          maxArgs = null;
        } else if (text.startsWith('+')) {
          minArgs++;
          maxArgs = null;
        } else if (text.startsWith(']')) {
          maxArgs!++;
        } else {
          minArgs++;
          maxArgs!++;
        }
      }
    }
    const returnType = parseReturnType($, genDef);
    const desc = collectDescription(name, $, genDef);
    docs.push({
      kind: 'generic-function',
      name,
      minArgs,
      maxArgs,
      syntax,
      returnType,
      desc
    });
  }
  for (const defDef of $('.definition-defining')) {
    const name = $(defDef).find('.operator').text();
    if (name == 'defining-form-name') continue;
    let syntax = $(defDef).text();
    syntax = syntax.split('→')[0]!.trim();
    const returnType = parseReturnType($, defDef);
    const desc = collectDescription(name, $, defDef);
    docs.push({
      kind: 'syntax',
      name,
      syntax,
      returnType,
      desc
    });
  }
  for (const constDef of $('.definition-constant')) {
    const name = $(constDef).find('.terminal').text();
    const valueType = parseReturnType($, constDef);
    const desc = collectDescription(name, $, constDef);
    docs.push({
      kind: 'constant',
      name,
      valueType: valueType!,
      desc
    });
  }
  for (const specDef of $('.definition-special-form')) {
    let name = $($(specDef).find('.operator')[1]).text();
    if (!name) {
      // for base (setf place value) entry
      name = $($(specDef).find('.operator')[0]).text();
    }
    let syntax = $(specDef).text();
    syntax = syntax.split('→')[0]!.trim();
    const returnType = parseReturnType($, specDef);
    const desc = collectDescription(name, $, specDef);
    docs.push({
      // mark the base setf form itself as a syntax
      kind: name == 'setf' ? 'syntax' : 'setf',
      name,
      syntax,
      returnType,
      desc
    });
  }
  for (const specDef of $('.definition-special')) {
    const name = $(specDef).find('.operator').text();
    let syntax = $(specDef).text();
    syntax = syntax.split('→')[0]!.trim();
    const returnType = parseReturnType($, specDef);
    const desc = collectDescription(name, $, specDef);
    docs.push({
      kind: 'syntax',
      name,
      syntax,
      returnType,
      desc
    });
  }
  for (const funDef of $('.definition-function')) {
    const name = $(funDef).find('.operator').text();
    if (name == 'function-name') continue;
    let syntax = $(funDef).text();
    syntax = syntax.split('→')[0]!.trim();
    let minArgs = 0;
    let maxArgs: number | null = 0;
    for (const arg of $(funDef).find('.arg')) {
      if (arg.nextSibling?.type == 'text') {
        const text = arg.nextSibling.data;
        if (text.startsWith('*')) {
          maxArgs = null;
        } else if (text.startsWith('+')) {
          minArgs++;
          maxArgs = null;
        } else if (text.startsWith(']')) {
          maxArgs!++;
        } else {
          minArgs++;
          maxArgs!++;
        }
      }
    }
    const returnType = parseReturnType($, funDef);
    const desc = collectDescription(name, $, funDef);
    docs.push({
      kind: 'function',
      name,
      minArgs,
      maxArgs,
      syntax,
      returnType,
      desc
    });
  }
  for (const funDef of $('.definition-local')) {
    const name = $(funDef).find('.operator').text();
    if (name == 'local-function-name') continue;
    let syntax = $(funDef).text();
    syntax = syntax.split('→')[0]!.trim();
    let minArgs = 0;
    let maxArgs: number | null = 0;
    for (const arg of $(funDef).find('.arg')) {
      if (arg.nextSibling?.type == 'text') {
        const text = arg.nextSibling.data;
        if (text.startsWith('*')) {
          maxArgs = null;
        } else if (text.startsWith('+')) {
          minArgs++;
          maxArgs = null;
        } else if (text.startsWith(']')) {
          maxArgs!++;
        } else {
          minArgs++;
          maxArgs!++;
        }
      }
    }
    const returnType = parseReturnType($, funDef);
    const desc = collectDescription(name, $, funDef);
    docs.push({
      kind: 'local-function',
      name,
      minArgs,
      maxArgs,
      syntax,
      returnType,
      desc
    });
  }

  docs = docs.toSorted((a, b) => a.name.localeCompare(b.name));
  for (const doc of docs) {
    renderDocumentationSexpr(doc);
  }
}

function collectDescription(name: string, $: CheerioAPI.CheerioAPI, el: Element): string {
  let descHtml: string = '';
  let skippingGroupedDefs = true;
  for (const sib of $(el).nextAll()) {
    if ($(sib).hasClass('definition-function')
      || $(sib).hasClass('definition-special')
      || $(sib).hasClass('definition-special-form')
      || $(sib).hasClass('definition-defining')
      || $(sib).hasClass('definition-generic')
      || $(sib).hasClass('definition-constant')
      || $(sib).hasClass('definition-local')
    ) {
      if (skippingGroupedDefs) continue;
      else break;
    }
    skippingGroupedDefs = false;
    if ($(sib).is('h2') || $(sib).is('h3') || $(sib).is('h4') || $(sib).is('h5') || $(sib).is('h6') || $(sib).hasClass("example")) {
      break;
    }
    if ($(sib).is('#index')) {
      break;
    }
    descHtml += $(sib);
  }
  return htmlToPseudoAsciidoc(name, descHtml);
}

function htmlToPseudoAsciidoc(name: string, html: string): string {
  // handle lists
  html = html.replaceAll(/\s*(<ol>.*?<\/ol>)/gs, capture => {
    const $ = load(capture);
    let result = '\n\n';
    $('li').each((i, li) => { 
      const text = $(li).text();
      result += `${i+1}. ${text}\n`;
    })
    result += '\n';
    return result;
  });
  html = html.replaceAll(/\s*(<ul>.*?<\/ul>)/gs, capture => {
    const $ = load(capture);
    let result = '\n\n';
    for (const li of $('li')) {
      const text = $(li).text().trim();
      result += `BULLETPOINT ${text}\n`;
    }
    result += '\n';
    return result;
  });
  
  // handle tables
  html = html.replace(/\s*<table.*?>.*?<tbody>(.*?)<\/tbody>.*?<\/table>/s, capture => {
    const $ = load('<table>'+capture+'</table>');
    let columns = $('tr').first().find('th').length;
    if (columns == 0) {
      columns = $('tr').first().find('td').length;
    }
    let result = `\n[cols="`;
    for (let i = 0; i < columns; i++) {
      if (i != 0) {
        result += ',';
      }
      result += "1";
    }
    result += `"]\n|===\n`;
    $('tr').each((i, tr) => {
      $(tr).find('th').each((i, th) => {
        if (i != 0) {
          result += ' ';
        }
        const text = $(th).text();
        result += `| ${text}`;
      });
      result += '\n';
      $(tr).find('td').each((i, td) => {
        if (i != 0) {
          result += ' ';
        }
        const text = $(td).text();
        result += `| ${text}`;
      });
      result += '\n';
    });
    result += '|===\n\n';
    return result;
  });
  
  // replace already escaped characters to avoid double escape
  html = html.replaceAll("&lt;", "<");
  html = html.replaceAll("&gt;", ">");
  html = html.replaceAll("&amp;", "&");
  
  // replace special characters using unicode codepoint syntax
  // _ is replaced afterwards to not mess up span match
  html = html.replaceAll("&", "&#38;");
  html = html.replaceAll("*", "&#42;")
  html = html.replaceAll("~", "&#126;")
  html = html.replaceAll("+", "&#43;")
  html = html.replaceAll("_", "&#95;")

  // fix up after content escapes
  html = html.replaceAll('BULLETPOINT', '*');
  
  // note
  html = html.replaceAll(/<div class="note">Note: (.*?)<\/div>/gs, `\nNOTE: $1\n`);

  /*
  html.replaceAll(/<a href=".*">(.*?)<\/a>/g, r1 => { 
    html.replaceAll(/<a href=".*?">(.*?)<\/a>.*<\/a>/g, r2 => {
      console.log(name);
      console.log(r2);
      console.log();
      return '';
    });
    return '';
  })
  */
  // remove links
  html = html.replaceAll(/<a href=".*?">(.*?)<\/a>/g, "$1");

  // bolden terminals, italics for non-terminals and error references
  html = html.replaceAll(/<span class="terminal">(.*?)<\/span>/g, "*$1*");
  html = html.replaceAll(/<span class="nonterminal">(.*?)<\/span>/g, "_$1_");
  html = html.replaceAll(/<span class="error&#95;id">(.*?)<\/span>/g, "_$1_");
  html = html.replaceAll(/<i>(.*?)<\/i>/g, "_$1_");
  html = html.replaceAll(/<b>(.*?)<\/b>/g, "*$1*");
  html = html.replaceAll(/<q>(.*?)<\/q>/g, `"$1"`);

  // class reference, bolden + add <>
  html = html.replaceAll(/<span class="class">(.*?)<\/span>/g, "*<$1>*");
  
  // other spans
  html = html.replaceAll(/<span.*?>(.*?)<\/span>/g, "$1");

  // subscript
  html = html.replaceAll(/<sub>(.*?)<\/sub>/g, "~$1~");
  
  html = html.replaceAll(/\s*<pre>/g, `[subs="+quotes,+replacements"]\n----\n`);
  html = html.replaceAll(/\s*<\/pre>/g, "\n----\n");
  
  // new line for end of paragraph
  html = html.replaceAll("</p>", "\n\n");
  html = html.replaceAll(/<p.*?>/g, "");

  html = html.trim();
  
  return html;
}

function parseReturnType($: CheerioAPI.CheerioAPI, el: Element): string | null {
  const nonTerminal = $(el).find('.nonterminal');
  const classType = $(el).find('.class');
  if (nonTerminal.length) {
    return nonTerminal.text();
  } else if (classType.length) {
    return `<${classType.text()}>`;
  }
  return null;
}

function renderDocumentationSexpr(doc: DocEntry) {
  let namespace = '';
  switch (doc.kind) {
    case 'constant': namespace = 'value'; break;
    case 'syntax': namespace = 'syntax'; break;
    case 'setf': namespace = 'setf'; break;
    // TODO classes
    default: namespace = 'function'; break;
  }
  let str = `(setf (documentation '${namespace} '${doc.name})\n`;
  str += `    '((kind . ${doc.kind})\n`;
  if (doc.kind != 'constant') {
    str += `      (syntax . ${strToSexpr(doc.syntax)})\n`;
    str += `      (return-type . ${strToSexpr(doc.returnType)})\n`;
  }
  if (doc.kind == 'constant') {
    str += `      (value-type . ${strToSexpr(doc.valueType)})\n`;
  }
  if (doc.kind == 'function' || doc.kind == 'local-function' || doc.kind == 'generic-function') {
    str += `      (min-args . ${numToSexpr(doc.minArgs)})\n`;
    str += `      (max-args . ${numToSexpr(doc.maxArgs)})\n`;
  }
  str += `      (desc . ${strToSexpr(doc.desc)})))\n`;
  console.log(str);
}

function strToSexpr(s: string | null): string {
  if (!s) return 'nil';
  s = s.replaceAll("\\", "\\\\");
  s = s.replaceAll("\"", "\\\"");
  return `"${s}"`;
}

function numToSexpr(n: number | null): string {
  if (n == null) {
    return 'nil';
  }
  return '' + n;
}

main()
