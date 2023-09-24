package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.ISLISPError;
import com.github.arvyy.islisp.builtins.BuiltinClassSlotReader;
import com.github.arvyy.islisp.builtins.BuiltinClassSlotReaderNodeGen;
import com.github.arvyy.islisp.builtins.BuiltinClassSlotWriterNodeGen;
import com.github.arvyy.islisp.runtime.*;
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
    private ISLISPExpressionNode[] slotDefMethods;

    public ISLISPDefClassNode(TruffleLanguage<?> language, Symbol name, List<Symbol> superclassName, List<SlotDefinition> slots, boolean isAbstract, SourceSection sourceSection) {
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
            for (var reader: slot.readerName) {
                exprs.add(new ISLISPDefGeneric(reader, 1, false, null));
                exprs.add(new ISLISPDefMethodNode(
                        ISLISPDefMethodNode.MethodQualifier.none,
                        reader,
                        new Symbol[]{ name },
                        1,
                        false,
                        BuiltinClassSlotReaderNodeGen.create(slot.name, language)));
            }
            for (var writer: slot.writerName) {
                exprs.add(new ISLISPDefGeneric(writer, 2, false, null));
                exprs.add(new ISLISPDefMethodNode(
                        ISLISPDefMethodNode.MethodQualifier.none,
                        writer,
                        new Symbol[]{ name, ISLISPContext.get(this).namedSymbol("<object>") },
                        2,
                        false,
                        BuiltinClassSlotWriterNodeGen.create(slot.name, language)));
            }
        }
        return exprs.toArray(ISLISPExpressionNode[]::new);
    }

    @Override
    public Value executeGeneric(VirtualFrame frame) {
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
                        var property = new DefaultStaticProperty(parentSlot.name().id + "");
                        shapeBuilder.property(property, Object.class, false);
                        var slot = new StandardClass.Slot(parentSlot.name(), property, parentSlot.initForm(), parentSlot.initArg());
                        myslots.put(parentSlot.name(), slot);
                    }
                }
            }
        }
        // add own unique properties
        for (var slot: slots) {
            var slotInParent = myslots.get(slot.name.identityReference());
            StaticProperty property;
            SymbolReference initArg;
            LispFunction initForm;
            if (slotInParent == null) {
                property = new DefaultStaticProperty(slot.name.identityReference().id + "");
                shapeBuilder.property(property, Object.class, false);
            } else {
                property = slotInParent.property();
            }
            if (slot.initArg != null) {
                initArg = slot.initArg.identityReference();
            } else if (slotInParent != null) {
                initArg = slotInParent.initArg();
            } else {
                initArg = null;
            }
            if (slot.initializer != null) {
                initForm = new LispFunction(slot.initializer.getCallTarget());
            } else if (slotInParent != null) {
                initForm = slotInParent.initForm();
            } else {
                initForm = null;
            }
            var newSlot = new StandardClass.Slot(slot.name.identityReference(), property, initForm, initArg);
            myslots.put(slot.name.identityReference(), newSlot);
        }
        var newClass = new StandardClass(
                superclasses.toArray(LispClass[]::new),
                shapeBuilder.build(),
                myslots.values().toArray(StandardClass.Slot[]::new),
                isAbstract
        );
        ctx.registerClass(name.identityReference(), newClass);
        for (var function: slotDefMethods) {
            function.executeGeneric(frame);
        }
        return name;
    }

    public static class SlotDefinition {
        public Symbol name;
        public Symbol[] readerName;
        public Symbol[] writerName;
        public Symbol[] accessorName;
        public Symbol[] boundpName;
        public ISLISPRootNode initializer;
        public Symbol initArg;
    }

}
