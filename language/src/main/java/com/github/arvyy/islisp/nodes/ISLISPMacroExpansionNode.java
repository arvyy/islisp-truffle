package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.parser.EqWrapper;
import com.github.arvyy.islisp.parser.Parser;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

import java.util.List;
import java.util.Map;

/**
 * Helper node for deferring full parsing (which requires running user code) until runtime.
 */
public class ISLISPMacroExpansionNode extends ISLISPExpressionNode {

    private final List<Object> source;
    private final Map<EqWrapper, SourceSection> sourceSectionMap;

    /**
     * Create macro expansion node.
     * @param sourceSectionMap map containing source code locations
     * @param source top level user code
     */
    public ISLISPMacroExpansionNode(Map<EqWrapper, SourceSection> sourceSectionMap, List<Object> source) {
        super(null);
        this.source = source;
        this.sourceSectionMap = sourceSectionMap;
        markInternal();
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        CompilerDirectives.transferToInterpreterAndInvalidate();
        var parser = new Parser(sourceSectionMap);
        var replacement = parser.executeMacroExpansion(source);
        return replace(replacement).executeGeneric(frame);
    }
}
