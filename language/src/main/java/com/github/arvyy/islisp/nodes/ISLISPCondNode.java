package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.Utils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

import java.util.ArrayList;
import java.util.List;

/**
 * Implements `cond` syntax.
 */
public class ISLISPCondNode extends ISLISPExpressionNode {

    @Children
    private ISLISPExpressionNode[] content;

    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private final int[] offsets;

    /**
     * Create cond node.
     *
     * @param content 2d array representing cond data. External array wraps cases. Each internal
     *                array consists of 1 test expression and n forms.
     * @param sourceSection corresponding source section to this node.
     */
    public ISLISPCondNode(ISLISPExpressionNode[][] content, SourceSection sourceSection) {
        super(sourceSection);
        // internally flattening, because truffle requires @Children to be a 1D array.
        var flatContent = new ArrayList<ISLISPExpressionNode>();
        var offsetLst = new ArrayList<Integer>();
        for (var caseEntry: content) {
            offsetLst.add(flatContent.size());
            flatContent.addAll(List.of(caseEntry));
        }
        this.content = flatContent.toArray(ISLISPExpressionNode[]::new);
        this.offsets = offsetLst.stream().mapToInt(i -> i).toArray();
    }

    @Override
    @ExplodeLoop
    public Object executeGeneric(VirtualFrame frame) {
        var nil = ISLISPContext.get(this).getNil();
        for (int i = 0; i < offsets.length; i++) {
            var start = offsets[i];
            var end = i == offsets.length - 1
                ? content.length
                : offsets[i + 1];
            var testValue = content[start].executeGeneric(frame);
            if (!Utils.isNil(testValue)) {
                if (start + 1 == end) {
                    return testValue;
                }
                for (int j = start + 1; j < end - 1; j++) {
                    content[j].executeGeneric(frame);
                }
                return content[end - 1].executeGeneric(frame);
            }
        }
        return nil;
    }
}
