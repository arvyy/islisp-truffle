package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.Utils;
import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

import java.util.ArrayList;
import java.util.List;

/**
 * Implements `case` syntax.
 */
public class ISLISPCaseNode extends ISLISPExpressionNode {

    private final int[] offsets;

    private final Object[][] keys;

    @Child
    ISLISPExpressionNode keyForm;

    @Child
    ISLISPExpressionNode comparisonFnExpression;

    @Children
    private ISLISPExpressionNode[] expressions;

    @Children
    private ISLISPExpressionNode[] elseCase;

    /**
     * Create case syntax node.
     *
     * @param keyForm expression to evaluate to key to be compared to.
     * @param comparisonFnExpression expression to evalute to comparison function.
     * @param exprs expressions; outter array is cases, inner array is this case's expressions.
     * @param keys keys to compare to; outter array is cases, inner array is the objects to compare to.
     * @param elseCase expressions for the 'else' / t case. Empty if not present.
     * @param sourceSection corresponding source section to this node.
     */
    public ISLISPCaseNode(
        ISLISPExpressionNode keyForm,
        ISLISPExpressionNode comparisonFnExpression,
        ISLISPExpressionNode[][] exprs,
        Object[][] keys,
        ISLISPExpressionNode[] elseCase,
        SourceSection sourceSection
    ) {
        super(sourceSection);
        this.comparisonFnExpression = comparisonFnExpression;
        this.elseCase = elseCase;
        var offsetsLst = new ArrayList<Integer>();
        var exprsLst = new ArrayList<ISLISPExpressionNode>();
        for (int i = 0; i < keys.length; i++) {
            offsetsLst.add(exprsLst.size());
            exprsLst.addAll(List.of(exprs[i]));
        }
        this.offsets = offsetsLst.stream().mapToInt(i -> i).toArray();
        this.expressions = exprsLst.toArray(ISLISPExpressionNode[]::new);
        this.keys = keys;
        this.keyForm = keyForm;
    }

    @Override
    @ExplodeLoop
    public Object executeGeneric(VirtualFrame frame) {
        var nil = ISLISPContext.get(this).getNil();
        var value = keyForm.executeGeneric(frame);
        var fn = comparisonFnExpression.executeGeneric(frame);
        var interop = InteropLibrary.getUncached(fn);
        for (int i = 0; i < offsets.length; i++) {
            var matched = false;
            for (var obj: keys[i]) {
                try {
                    if (!Utils.isNil(interop.execute(fn, obj, value))) {
                        matched = true;
                        break;
                    }
                } catch (InteropException e) {
                    throw new ISLISPError("Interop error in case", this);
                }
            }
            if (!matched) {
                continue;
            }
            var start = offsets[i];
            var end = i == offsets.length - 1
                ? expressions.length
                : offsets[i + 1];
            if (start == end) {
                return nil;
            }
            for (int j = start; j < end - 1; j++) {
                expressions[j].executeGeneric(frame);
            }
            return expressions[end - 1].executeGeneric(frame);
        }
        if (elseCase.length != 0) {
            for (int i = 0; i < elseCase.length - 1; i++) {
                elseCase[i].executeGeneric(frame);
            }
            return elseCase[elseCase.length - 1].executeGeneric(frame);
        }
        return nil;
    }

}
