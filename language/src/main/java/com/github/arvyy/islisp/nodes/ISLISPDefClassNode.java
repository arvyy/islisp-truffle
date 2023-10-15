package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.functions.ISLISPClassSlotReaderNodeGen;
import com.github.arvyy.islisp.functions.ISLISPClassSlotWriterNodeGen;
import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.api.staticobject.DefaultStaticProperty;
import com.oracle.truffle.api.staticobject.StaticProperty;
import com.oracle.truffle.api.staticobject.StaticShape;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class ISLISPDefClassNode extends ISLISPExpressionNode {

    private final Symbol name;
    private final List<Symbol> superclassName;
    private final List<SlotDefinition> slots;
    private final boolean isAbstract;

    @Children
    private final ISLISPExpressionNode[] slotDefMethods;

    public ISLISPDefClassNode(
            TruffleLanguage<?> language,
            Symbol name,
            List<Symbol> superclassName,
            List<SlotDefinition> slots,
            boolean isAbstract,
            SourceSection sourceSection
    ) {
        super(sourceSection);
        this.name = name;
        this.superclassName = superclassName;
        this.slots = slots;
        this.isAbstract = isAbstract;
        slotDefMethods = buildSlotFunctionNodes(language);
    }

    private ISLISPExpressionNode[] buildSlotFunctionNodes(TruffleLanguage<?> language) {
        var exprs = new ArrayList<ISLISPExpressionNode>();
        for (var slot: slots) {
            for (var reader: slot.getReaderName()) {
                exprs.add(new ISLISPDefGenericNode(reader, false, 1, false, null));
                exprs.add(new ISLISPDefMethodNode(
                        ISLISPDefMethodNode.MethodQualifier.none,
                        reader,
                        false,
                        new Symbol[]{name},
                        1,
                        false,
                        ISLISPClassSlotReaderNodeGen.create(slot.getName(), language)));
            }
            for (var writer: slot.getWriterName()) {
                exprs.add(new ISLISPDefGenericNode(writer, false, 2, false, null));
                exprs.add(new ISLISPDefMethodNode(
                        ISLISPDefMethodNode.MethodQualifier.none,
                        writer,
                        false,
                        new Symbol[]{name, ISLISPContext.get(this).namedSymbol("<object>")},
                        2,
                        false,
                        ISLISPClassSlotWriterNodeGen.create(slot.getName(), language)));
            }
        }
        return exprs.toArray(ISLISPExpressionNode[]::new);
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        executeOutsideTruffle();
        for (var function: slotDefMethods) {
            function.executeGeneric(frame);
        }
        return name;
    }

    @CompilerDirectives.TruffleBoundary
    void executeOutsideTruffle() {
        var ctx = ISLISPContext.get(null);
        var myslots = new HashMap<SymbolReference, StandardClass.Slot>();
        var shapeBuilder = StaticShape.newBuilder(ctx.getLanguage());
        var superclasses = new ArrayList<LispClass>();
        // collect inherited slots from parent classes
        for (var superclass: superclassName) {
            var clazz = ctx.lookupClass(superclass.identityReference());
            if (clazz == null) {
                throw new ISLISPError("Class not found " + superclass.name(), this);
            }
            superclasses.add(clazz);
            if (clazz instanceof StandardClass standardClass) {
                for (var parentSlot: standardClass.slots()) {
                    if (!myslots.containsKey(parentSlot.name())) {
                        var property = new DefaultStaticProperty(parentSlot.name().getId() + "");
                        shapeBuilder.property(property, Object.class, false);
                        var slot = new StandardClass.Slot(
                                parentSlot.name(),
                                property,
                                parentSlot.initForm(),
                                parentSlot.initArg());
                        myslots.put(parentSlot.name(), slot);
                    }
                }
            }
        }
        // add own unique properties
        for (var slot: slots) {
            var slotInParent = myslots.get(slot.getName().identityReference());
            StaticProperty property;
            SymbolReference initArg;
            LispFunction initForm;
            if (slotInParent == null) {
                property = new DefaultStaticProperty(slot.getName().identityReference().getId() + "");
                shapeBuilder.property(property, Object.class, false);
            } else {
                property = slotInParent.property();
            }
            if (slot.getInitArg() != null) {
                initArg = slot.getInitArg().identityReference();
            } else if (slotInParent != null) {
                initArg = slotInParent.initArg();
            } else {
                initArg = null;
            }
            if (slot.getInitializer() != null) {
                initForm = new LispFunction(slot.getInitializer().getCallTarget());
            } else if (slotInParent != null) {
                initForm = slotInParent.initForm();
            } else {
                initForm = null;
            }
            var newSlot = new StandardClass.Slot(slot.getName().identityReference(), property, initForm, initArg);
            myslots.put(slot.getName().identityReference(), newSlot);
        }
        var newClass = new StandardClass(
                superclasses.toArray(LispClass[]::new),
                shapeBuilder.build(),
                myslots.values().toArray(StandardClass.Slot[]::new),
                isAbstract
        );
        ctx.registerClass(name.identityReference(), newClass);
    }

    public static class SlotDefinition {
        private Symbol name;
        private Symbol[] readerName;
        private Symbol[] writerName;
        private Symbol[] accessorName;
        private Symbol[] boundpName;
        private ISLISPRootNode initializer;
        private Symbol initArg;

        public Symbol getName() {
            return name;
        }

        public void setName(Symbol name) {
            this.name = name;
        }

        public Symbol[] getReaderName() {
            return readerName;
        }

        public void setReaderName(Symbol[] readerName) {
            this.readerName = readerName;
        }

        public Symbol[] getWriterName() {
            return writerName;
        }

        public void setWriterName(Symbol[] writerName) {
            this.writerName = writerName;
        }

        public Symbol[] getAccessorName() {
            return accessorName;
        }

        public void setAccessorName(Symbol[] accessorName) {
            this.accessorName = accessorName;
        }

        public Symbol[] getBoundpName() {
            return boundpName;
        }

        public void setBoundpName(Symbol[] boundpName) {
            this.boundpName = boundpName;
        }

        public ISLISPRootNode getInitializer() {
            return initializer;
        }

        public void setInitializer(ISLISPRootNode initializer) {
            this.initializer = initializer;
        }

        public Symbol getInitArg() {
            return initArg;
        }

        public void setInitArg(Symbol initArg) {
            this.initArg = initArg;
        }
    }

}
