package com.github.arvyy.islisp.parser;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.ControlFlowException;
import com.oracle.truffle.api.source.SourceSection;

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
     * Vector of elements, some of which aren't statically known.
     *
     * @param children
     */
    record Vector(QuasiquoteTree[] children) implements QuasiquoteTree { }

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
     * @param sourceSection source section spanning the quasiquote tree
     * @param expr sexpr
     * @return quasiquote tree and expressions
     */
    static QuasiquoteTreeAndExpressions parseQuasiquoteTree(SourceSection sourceSection, Object expr) {
        return parseQuasiquoteTree(sourceSection, expr, 0, 0);
    }

    private static QuasiquoteTreeAndExpressions rewrap(QuasiquoteTreeAndExpressions parsed, Symbol s) {
        var tree = parsed.tree;
        var content = new ArrayList<QuasiquoteTree>();
        content.add(new Atom(s));
        content.add(tree);
        tree = new List(content.toArray(QuasiquoteTree[]::new));
        return new QuasiquoteTreeAndExpressions(tree, parsed.expressions);
    }

    private static QuasiquoteTreeAndExpressions parseQuasiquoteTree(
        SourceSection sourceSection,
        Object expr,
        int level,
        int holeIndex
    ) {
        if (expr instanceof Pair p) {
            if (p.car() instanceof Symbol s) {
                Object rest;
                boolean isSplicing = false;
                switch (s.name()) {
                    case "quasiquote":
                        rest = ((Pair) p.cdr()).car();
                        if (level > 0) {
                            return rewrap(parseQuasiquoteTree(sourceSection, rest, level + 1, holeIndex), s);
                        } else {
                            return parseQuasiquoteTree(sourceSection, rest, level + 1, holeIndex);
                        }
                    case "unquote-splicing":
                        isSplicing = true;
                        // fallthrough
                    case "unquote":
                        rest = ((Pair) p.cdr()).car();
                        if (level < 1) {
                            throw new ParsingException(
                                sourceSection,
                                "Unexpected " + (isSplicing ? "unquote-splicing" : "unquote"));
                        }
                        if (level == 1) {
                            var hole = new Hole(holeIndex);
                            if (isSplicing) {
                                return new QuasiquoteTreeAndExpressions(new UnquoteSplicing(hole), new Object[]{rest});
                            } else {
                                return new QuasiquoteTreeAndExpressions(new Unquote(hole), new Object[]{rest});
                            }
                        } else {
                            return rewrap(parseQuasiquoteTree(sourceSection, rest, level - 1, holeIndex), s);
                        }
                     default:
                }
            }
            // regular list
            var expressions = new ArrayList<Object>();
            var children = new ArrayList<QuasiquoteTree>();
            for (var v: p) {
                var parsedChildResult = parseQuasiquoteTree(sourceSection, v, level, holeIndex + expressions.size());
                expressions.addAll(Arrays.asList(parsedChildResult.expressions));
                children.add(parsedChildResult.tree);
            }
            return new QuasiquoteTreeAndExpressions(
                    new List(children.toArray(QuasiquoteTree[]::new)),
                    expressions.toArray(Object[]::new));
        }
        if (expr instanceof LispVector v) {
            var expressions = new ArrayList<Object>();
            var children = new ArrayList<QuasiquoteTree>();
            for (var el: v.values()) {
                var parsedChildResult = parseQuasiquoteTree(sourceSection, el, level, holeIndex + expressions.size());
                expressions.addAll(Arrays.asList(parsedChildResult.expressions));
                children.add(parsedChildResult.tree);
            }
            return new QuasiquoteTreeAndExpressions(
                new Vector(children.toArray(QuasiquoteTree[]::new)),
                expressions.toArray(Object[]::new));
        }
        if (expr instanceof Integer
            || expr instanceof LispBigInteger
            || expr instanceof Symbol
            || expr instanceof LispChar
            || expr instanceof String
            || expr instanceof StringBuffer
        ) {
            return new QuasiquoteTreeAndExpressions(new Atom(expr), new Object[]{});
        }
        throw new ParsingException(sourceSection, "Unrecognized quasi-quote form.");
    }

    /**
     * Evaluate quasiquote tree, substituting holes with given values.
     *
     * @param tree quasiquote tree
     * @param substitutionValues values to be substituted
     * @return evaluated value
     */
    @CompilerDirectives.TruffleBoundary
    static Object evalQuasiquoteTree(QuasiquoteTree tree, Object[] substitutionValues) {
        if (tree instanceof Atom a) {
            return a.value;
        }
        if (tree instanceof Unquote u) {
            return substitutionValues[u.value.index];
        }
        if (tree instanceof List l) {
            Object lispList = ISLISPContext.get(null).getNil();
            var values = evalQuasiquoteCollectionContent(l.children, substitutionValues);
            for (int i = values.size() - 1; i >= 0; i--) {
                lispList = new Pair(values.get(i), lispList);
            }
            return lispList;
        }
        if (tree instanceof Vector v) {
            var values =  evalQuasiquoteCollectionContent(v.children, substitutionValues);
            return new LispVector(values.toArray());
        }
        return ISLISPContext.get(null).getNil();
    }

    private static java.util.List<Object> evalQuasiquoteCollectionContent(
        QuasiquoteTree[] children,
        Object[] substitutionValues
    ) {
        var values = new ArrayList<Object>();
        for (var child: children) {
            if (child instanceof UnquoteSplicing us) {
                var substitution = substitutionValues[us.value.index];
                if (substitution instanceof Pair p) {
                    for (var v: p) {
                        values.add(v);
                    }
                } else if (substitution instanceof Symbol s) {
                    if (s != ISLISPContext.get(null).getNil()) {
                        throw new UnquoteSpliceNotAListException(substitution);
                    }
                } else {
                    throw new UnquoteSpliceNotAListException(substitution);
                }
            } else {
                values.add(evalQuasiquoteTree(child, substitutionValues));
            }
        }
        return values;
    }

    /**
     * Exception signalling unquote-splicing didn't produce a list.
     * Caught in ISLISPQuasiquoteNode to produce proper condition signalling.
     */
    class UnquoteSpliceNotAListException extends ControlFlowException {
        private final Object value;

        /**
         * @param value evaluated quasiquote-splice hole that didn't yield a list.
         */
        public UnquoteSpliceNotAListException(Object value) {
            this.value = value;
        }

        /**
         * @return evaluated quasiquote-splice hole that didn't yield a list.
         */
        public Object getValue() {
            return value;
        }
    }

}
