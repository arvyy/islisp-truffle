package com.github.arvyy.islisp.parser;

import com.github.arvyy.islisp.nodes.ISLISPDefunNode;
import com.github.arvyy.islisp.nodes.ISLISPExpressionNode;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeVisitor;
import com.oracle.truffle.api.source.SourceSection;

import java.util.Optional;

public class NodeFinderVisitor implements NodeVisitor {

    private final SourceSection target;
    private ISLISPExpressionNode mostSpecificFit = null;

    public NodeFinderVisitor(SourceSection section) {
        target = section;
    }

    @Override
    public boolean visit(Node node) {
        if (node instanceof ISLISPDefunNode f) {
            f.getFunctionNode().accept(this);
        }
        //TODO
        if (node instanceof ISLISPExpressionNode e) {
            if (isSubSection(e.getSourceSection(), target)) {
                if (mostSpecificFit == null) {
                    mostSpecificFit = e;
                } else if (isSubSection(mostSpecificFit.getSourceSection(), e.getSourceSection())) {
                    mostSpecificFit = e;
                }
            }
        }
        return true;
    }


    public static boolean isSubSection(SourceSection a, SourceSection b) {
        return a.getStartColumn() <= b.getStartColumn()
            && a.getStartLine() <= b.getStartLine()
            && a.getEndColumn() >= b.getEndColumn()
            && a.getEndLine() >= b.getEndColumn();
    }


    public Optional<ISLISPExpressionNode> foundNode() {
        return Optional.ofNullable(mostSpecificFit);
    }
}
