package com.github.arvyy.islisp.parser;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.runtime.LispChar;
import com.github.arvyy.islisp.runtime.Pair;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.nodes.Node;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;

/**
 * Representation of a quasiquote, specifying holes and ways to substitute them during evaluation.
 */
//TODO preserve source location
//TODO calcify non-atoms into Atom if there are no internal substitutions
public sealed interface QuasiquoteTree {

    /**
     * Statically known quasiquote value to be used as is.
     *
     * @param value
     */
    record Atom(Object value) implements QuasiquoteTree { }

    /**
     * List of elements, some of which aren't statically known.
     *
     * @param children
     */
    record List(QuasiquoteTree[] children) implements QuasiquoteTree { }

    /**
     * A part that needs to be unquoted.
     *
     * @param value
     */
    record Unquote(Hole value) implements QuasiquoteTree { }

    /**
     * A part that needs to be unquoted and spliced.
     * @param value
     */
    record UnquoteSplicing(Hole value) implements QuasiquoteTree { }

    /**
     * Hole value, specifying which expression will fill it on execution.
     *
     * @param index expression index corresponding to the hole
     */
    record Hole(int index) implements QuasiquoteTree { }

    /**
     * Tuple of tree structure and expressions in holes.
     *
     * @param tree
     * @param expressions
     */
    record QuasiquoteTreeAndExpressions(QuasiquoteTree tree, Object[] expressions) { }

    /**
     * Parse sexpr into quasiquote tree.
     *
     * @param expr sexpr
     * @return quasiquote tree and expressions
     */
    static QuasiquoteTreeAndExpressions parseQuasiquoteTree(Object expr) {
        return parseQuasiquoteTree(expr, 0, 0);
    }

    private static QuasiquoteTreeAndExpressions rewrap(QuasiquoteTreeAndExpressions parsed, Symbol s) {
        var tree = parsed.tree;
        var content = new ArrayList<QuasiquoteTree>();
        content.add(new Atom(s));
        content.add(tree);
        tree = new List(content.toArray(QuasiquoteTree[]::new));
        return new QuasiquoteTreeAndExpressions(tree, parsed.expressions);
    }

    private static QuasiquoteTreeAndExpressions parseQuasiquoteTree(Object expr, int level, int holeIndex) {
        if (expr instanceof Pair p) {
            if (p.car() instanceof Symbol s) {
                Object rest;
                boolean isSplicing = false;
                switch (s.name()) {
                    case "quasiquote":
                        rest = ((Pair) p.cdr()).car();
                        if (level > 0) {
                            return rewrap(parseQuasiquoteTree(rest, level + 1, holeIndex), s);
                        } else {
                            return parseQuasiquoteTree(rest, level + 1, holeIndex);
                        }
                    case "unquote-splicing":
                        isSplicing = true;
                        // fallthrough
                    case "unquote":
                        rest = ((Pair) p.cdr()).car();
                        if (level < 1) {
                            throw new RuntimeException("Unquote outside of quasiquote");
                        }
                        if (level == 1) {
                            var hole = new Hole(holeIndex);
                            if (isSplicing) {
                                return new QuasiquoteTreeAndExpressions(new UnquoteSplicing(hole), new Object[]{rest});
                            } else {
                                return new QuasiquoteTreeAndExpressions(new Unquote(hole), new Object[]{rest});
                            }
                        } else {
                            return rewrap(parseQuasiquoteTree(rest, level - 1, holeIndex), s);
                        }
                     default:
                }
            }
            // regular list
            var expressions = new ArrayList<Object>();
            var children = new ArrayList<QuasiquoteTree>();
            for (var v: p) {
                var parsedChildResult = parseQuasiquoteTree(v, level, holeIndex + expressions.size());
                expressions.addAll(Arrays.asList(parsedChildResult.expressions));
                children.add(parsedChildResult.tree);
            }
            return new QuasiquoteTreeAndExpressions(
                    new List(children.toArray(QuasiquoteTree[]::new)),
                    expressions.toArray(Object[]::new));
        }
        if (expr instanceof Integer
            || expr instanceof BigInteger
            || expr instanceof Symbol
            || expr instanceof LispChar
            || expr instanceof String
            || expr instanceof StringBuffer
        ) {
            return new QuasiquoteTreeAndExpressions(new Atom(expr), new Object[]{});
        }
        throw new RuntimeException();
    }

    /**
     * Evaluate quasiquote tree, substituting holes with given values.
     *
     * @param tree quasiquote tree
     * @param substitutionValues values to be substituted
     * @param node node from which this execution is done; used in case of an error
     * @return evaluated value
     */
    static Object evalQuasiquoteTree(QuasiquoteTree tree, Object[] substitutionValues, Node node) {
        if (tree instanceof Atom a) {
            return a.value;
        }
        if (tree instanceof Unquote u) {
            return substitutionValues[u.value.index];
        }
        if (tree instanceof UnquoteSplicing) {
            throw new ISLISPError("Bad unquotesplicing use", node);
        }
        if (tree instanceof List l) {
            var values = new ArrayList<Object>();
            for (var child: l.children) {
                if (child instanceof UnquoteSplicing us) {
                    if (substitutionValues[us.value.index] instanceof Pair p) {
                        for (var v: p) {
                            values.add(v);
                        }
                    } else if (substitutionValues[us.value.index] instanceof Symbol s) {
                        if (s != ISLISPContext.get(null).getNil()) {
                            throw new ISLISPError("Unquote splicing not list", node);
                        }
                    } else {
                        throw new ISLISPError("Unquote splicing not list", node);
                    }
                } else {
                    values.add(evalQuasiquoteTree(child, substitutionValues, node));
                }
            }
            Object lispList = ISLISPContext.get(null).getNil();
            for (int i = values.size() - 1; i >= 0; i--) {
                lispList = new Pair(values.get(i), lispList);
            }
            return lispList;
        }
        throw new ISLISPError("?", node);
    }

}
