/**
 * Copyright (C) 2009 Kenji Nozawa
 * This file is part of LAPIN.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */
package lapin.comp.asm;
import lapin.comp.ByteCodeGenerator;
import lapin.comp.Callables;
import lapin.comp.Classes;
import lapin.comp.ClassInfo;
import lapin.comp.Insts;
import lapin.comp.MethodInfo;
import lapin.function.Function;
import lapin.function.LambdaList;
import lapin.io.Printer;
import lapin.lang.Data;
import lapin.lang.Env;
import lapin.lang.Lists;
import lapin.lang.NotReachedException;
import lapin.lang.Symbol;
import lapin.lang.Symbols;
import lapin.util.Logger;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Iterator;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

/**
 * ByteCodeGenerator which uses
 * <a href="http://asm.ow2.org/">ASM</a>.
 * @see ByteCodeGenerator
 * @see ClassInfo
 * @see MethodInfo
 */
public final class ASMByteCodeGenerator extends ByteCodeGenerator {
    static private final Type TYPE_ENV = Type.getType(Env.class);
    static private final Type TYPE_LAMBDA_LIST = Type.getType(LambdaList.class);
    static private final Type TYPE_SYMBOL = Type.getType(Symbol.class);
    static private final Type TYPE_OBJECT = Type.getType(Object.class);
    static private final Type TYPE_STRING = Type.getType(String.class);
    static private final Type[] TYPE_NO_ARGS = new Type[] {};

    /**
     * data structure used to generate the bytecode of the method body.
     * <pre>
     *  lapin.function.Callable#call(Object,Env)
     *  lapin.function.Callable[N]#call[N](Object,...,Object,Env)
     *  lapin.function.Callable[N]r#call[N](Object,...,Object,Object,Env)
     * </pre>
     */
    static class CallableInfo {
        /** method info stored in the class info */
        MethodInfo mi;
        /** name of the interface to be implemented by the subr */
        String interfaceName;
        /**
         * counter which manages the slot of the local var
         * used in the call (or call[N]) method
         */
        int localVars;
        /**
         * param types for call (or call[N]) method
         */
        Type[] paramTypes;
        /**
         * return type of call (or call[N]) method
         */
        Type retType;

        /** table: (var, slot of local var) */
        final HashMap localTable = new HashMap();
        /** table: (tag, ASM label object) */
        final HashMap labelTable = new HashMap();

        CallableInfo(MethodInfo mi, Env env) {
            this.mi = mi;

            int nargs = mi.nargs();
            boolean rest = mi.rest();
            boolean implCallable = mi.implCallable();
            Class type = implCallable
                ? Callables.getType(nargs, rest, true) : null;
            Class[] pTypes = mi.paramTypes();
            Class rType = mi.retType();

            this.interfaceName = implCallable
                ? type.getName() : null;

            this.paramTypes = new Type[pTypes.length];
            for (int i = 0; i < pTypes.length; i++) {
                this.paramTypes[i] = Type.getType(pTypes[i]);
            }

            this.retType = Type.getType(rType);

            int slot = 0;
            if (nargs >= 0) {
                // slot for THIS
                localTable.put(Callables.LOCAL_SLOT_THIS,
                               Data.toFixnum(slot));
                slot += Classes.sizeOf(Function.class);
                for (int i = 0; i < nargs; i++) {
                    // slot for method argument ARG[i]
                    localTable.put(Callables.getArgSlot(i),
                                   Data.toFixnum(slot));
                    slot += Classes.sizeOf(pTypes[i]);
                }
                if (rest) {
                    // slot for method argument REST
                    localTable.put(Callables.LOCAL_SLOT_ARGS,
                                   Data.toFixnum(slot));
                    slot += Classes.sizeOf(pTypes[nargs]);
                    // slot for method argument ENV
                    localTable.put(Callables.LOCAL_SLOT_ENV,
                                   Data.toFixnum(slot));
                    slot += Classes.sizeOf(pTypes[nargs+1]);
                }
                else {
                    // slot for method argument ENV
                    localTable.put(Callables.LOCAL_SLOT_ENV,
                                   Data.toFixnum(slot));
                    slot += Classes.sizeOf(pTypes[nargs]);
                }
            }
            else {
                // slot for THIS
                localTable.put(Callables.LOCAL_SLOT_THIS,
                               Data.toFixnum(slot));
                slot += Classes.sizeOf(Function.class);
                // slot for method argument ARGS
                localTable.put(Callables.LOCAL_SLOT_ARGS,
                               Data.toFixnum(slot));
                slot += Classes.sizeOf(pTypes[0]);
                // slot for method argument ENV
                localTable.put(Callables.LOCAL_SLOT_ENV,
                               Data.toFixnum(slot));
                slot += Classes.sizeOf(pTypes[1]);
            }
            this.localVars = slot;

            if (Logger.debuglevelp(env)) {
                Logger.debug("[asm:init] interface =~S",
                             Lists.list(interfaceName), env);
                //Logger.debug("[asm:init] localVars =~S",
                //             Lists.list(Data.toFixnum(localVars)), env);
                Logger.debug("[asm:init] methodName=~S",
                             Lists.list(mi.name()), env);
                Logger.debug("[asm:init] params.len=~S",
                             Lists.list(Data.toFixnum(pTypes.length)),
                             env);
            }
        }
    }/* end of CallableInfo */

    /** ASM ClassWriter object */
    private ClassWriter _cw;

    /** list of CallableInfo */
    private Object ciList = Symbols.NIL;

    /** table: (const obj, field name) */
    private final IdentityHashMap constTable = new IdentityHashMap();
    /** table: (var, field name) */
    private final HashMap varTable = new HashMap();
    /** table: (lambdaList, field name) */
    private final HashMap llTable = new HashMap();

    /**
     * a count of the instance field used in the compiledExpr.
     */
    private int fields = 0;
    /**
     * flag that indicates whether this object has been used or not.
     */
    private boolean finished = false;

    protected void init(Env env) {
        if (Logger.debuglevelp(env))
            Logger.debug("[asm:init] className =~S",
                         Lists.list(super.classInfo.classname()), env);
        // CallableInfo
        Iterator it = super.classInfo.methodIterator();
        while (it.hasNext()) {
            MethodInfo mi = (MethodInfo) it.next();
            CallableInfo ci = new CallableInfo(mi, env);
            ciList = Lists.cons(ci, ciList);
        }
    }
    protected byte[] doGenerate(Env env) {
        if (finished) {
            throw new NotReachedException
                ("repeated use of the same object is not allowed",
                 Symbols.NIL);
        }
        try {
            initASMObject(env);
            for (Object l = ciList; !Lists.isEnd(l); l = Lists.cdr(l)) {
                prepareCompilation((CallableInfo) Lists.car(l), env);
            }
            generateFields(env);
            generateClassInitializer(env);
            generateConstructor(env);
            for (Object l = ciList; !Lists.isEnd(l); l = Lists.cdr(l)) {
                generateCall((CallableInfo) Lists.car(l), env);
            }
            return getBytes(env);
        }
        finally {
            finished = true;
        }
    }
    private String toInternalName(String className) {
        if (className == null)
            throw new NullPointerException("className is null");
        return className.replace('.','/');
    }
    private String toTypeDescriptor(String className) {
        if (className == null)
            throw new NullPointerException("className is null");
        return "L"+className.replace('.','/')+";";
    }
    private void initASMObject(Env env) {
        ArrayList al = new ArrayList();
        for (Object l = ciList; !Lists.isEnd(l); l = Lists.cdr(l)) {
            CallableInfo ci = (CallableInfo) Lists.car(l);
            if (ci.mi.implCallable())
                al.add(toInternalName(ci.interfaceName));
        }
        String[] ifNames = new String[al.size()];
        al.toArray(ifNames);
        // ClassWriter
        _cw = new ClassWriter(ClassWriter.COMPUTE_MAXS);
        _cw.visit(Opcodes.V1_1,
                  Opcodes.ACC_PUBLIC,
                  toInternalName(super.classInfo.classname()),
                  null,
                  "lapin/function/CompiledExpr",
                  ifNames);
    }
    private void prepareCompilation(CallableInfo ci, Env env) {
        // instruction list
        int len = ci.mi.instLen();
        // list of label-tag
        Object taglist = Symbols.NIL;

        // 1. arrange the catch-tags
        for (int i = 0; i < len; i++) {
            Object inst = ci.mi.getInst(i);
            if (Data.isSymbol(inst)) {
                continue;
            }
            Object id = Lists.car(inst);
            if (id == Insts.CATCH_FROM ||
                id == Insts.CATCH_TO ||
                id == Insts.CATCH_HANDLER) {
                if (i == len-1) {
                    throw new NotReachedException
                        ("~S cannot be put "+
                         "at the tail of the code list: ~S.",
                         Lists.list(id, inst));
                }
                Symbol tag = Data.symbol(Lists.cadr(inst));
                ci.mi.setInst(i, tag);
                if (Logger.debuglevelp(env))
                    Logger.debug
                        ("[asm:prepare]"+i+":\tid ~S replaced to tag: ~S",
                         Lists.list(id, tag), env);
            }
        }

        // 2. store info in xxxTable
        for (int i = 0; i < len; i++) {
            Object inst = ci.mi.getInst(i);
            if (Logger.tracelevelp(env))
                Logger.trace("[asm:prepare]"+i+":\t~S",
                             Lists.list(inst), env);

            // inst is symbol
            if (Data.isSymbol(inst)) {
                // push tag to taglist
                Symbol tag = Data.symbol(inst);
                taglist = Lists.cons(tag, taglist);
                continue;
            }

            // process taglist (regist labelInfo)
            if (taglist != Symbols.NIL) {
                Label label = new Label();
                for (Object l = taglist; !Lists.isEnd(l); l = Lists.cdr(l)) {
                    Symbol tag = Data.symbol(Lists.car(l));
                    ci.labelTable.put(tag, label);
                    if (Logger.tracelevelp(env))
                        Logger.trace("[asm:prepare]"+i+":\ttag ~S"+
                                     " added in labelTable(~S)",
                                     Lists.list(tag, label), env);
                }
                // reset taglist
                taglist = Symbols.NIL;
            }

            // inst must be the form of (id <arg1> <arg2> ....)
            Object id = Lists.car(inst);
            if (id == Insts.CONST) {
                Object obj = Lists.cadr(inst);
                if (!constTable.containsKey(obj)) {
                    constTable.put(obj, "_const_"+(fields++));
                    if (Logger.tracelevelp(env))
                        Logger.trace("[asm:prepare]"+i+":\tobj ~S"+
                                     " added in constTable",
                                     Lists.list(obj), env);
                }
            }
            else if (id == Insts.VAR) {
                Symbol var = Data.symbol(Lists.cadr(inst));
                if (!varTable.containsKey(var)) {
                    varTable.put(var, "_var_"+(fields++));
                    if (Logger.tracelevelp(env))
                        Logger.trace("[asm:prepare]"+i+":\tvar ~S"+
                                     " added in varTable",
                                     Lists.list(var), env);
                }
            }
            else if (id == Insts.LAMBDA_LIST) {
                LambdaList ll = Data.lambdaList(Lists.cadr(inst));
                if (!llTable.containsKey(ll)) {
                    llTable.put(ll, "_ll_"+(fields++));
                    if (Logger.tracelevelp(env))
                        Logger.trace("[asm:prepare]"+i+":\tll ~S"+
                                     " added in llTable",
                                     Lists.list(ll.params()), env);
                }
            }
            else if (id == Insts.ENV_CHILD) {
                Object oldEnvVar = Lists.cadr(inst);
                Object newEnvVar = Lists.caddr(inst);
                Object oldVar = Lists.car(oldEnvVar);
                Object newVar = Lists.car(newEnvVar);
                Object oldSlot = Lists.cadr(oldEnvVar);
                Object newSlot = Lists.cadr(newEnvVar);
                if (!ci.localTable.containsKey(oldSlot)) {
                    ci.localTable.put(oldSlot, Data.toFixnum(ci.localVars++));
                    if (Logger.tracelevelp(env))
                        Logger.trace("[asm:prepare]"+i+":\toldSlot ~S"+
                                     " added in localTable",
                                     Lists.list(oldSlot), env);
                }
                if (Data.isSymbol(oldVar) && !varTable.containsKey(oldVar)) {
                    varTable.put(oldVar, "_var_"+(fields++));
                    if (Logger.tracelevelp(env))
                        Logger.trace("[asm:prepare]"+i+":\tvar ~S"+
                                     " added in varTable",
                                     Lists.list(oldVar), env);
                }
                if (!ci.localTable.containsKey(newSlot)) {
                    ci.localTable.put(newSlot, Data.toFixnum(ci.localVars++));
                    if (Logger.tracelevelp(env))
                        Logger.trace("[asm:prepare]"+i+":\tnewSlot ~S"+
                                     " added in localTable",
                                     Lists.list(newSlot), env);
                }
                if (Data.isSymbol(newVar) && !varTable.containsKey(newVar)) {
                    varTable.put(newVar, "_var_"+(fields++));
                    if (Logger.tracelevelp(env))
                        Logger.trace("[asm:prepare]"+i+":\tvar ~S"+
                                     " added in varTable",
                                     Lists.list(newVar), env);
                }
            }
            else if (id == Insts.LOAD ||
                     id == Insts.STORE) {
                Object localVar = Lists.cadr(inst);
                Object var = Lists.car(localVar);
                Object slot = Lists.cadr(localVar);
                Class type = Data.javaClass(Lists.caddr(localVar));
                if (!ci.localTable.containsKey(slot)) {
                    switch (Classes.sizeOf(type)) {
                    case 1:
                        ci.localTable.put
                            (slot, Data.toFixnum(ci.localVars++));
                        break;
                    case 2:
                        Object slot2 = Data.toFixnum
                            (Data.fixnum(slot).intValue()+1);
                        ci.localTable.put
                            (slot, Data.toFixnum(ci.localVars++));
                        ci.localTable.put
                            (slot2, Data.toFixnum(ci.localVars++));
                        break;
                    default:
                        throw new NotReachedException
                            ("unsupported type: ~S.", Lists.list(type));
                    }
                    if (Logger.tracelevelp(env))
                        Logger.trace("[asm:prepare]"+i+":\tslot ~S"+
                                     " added in localTable: type is ~S",
                                     Lists.list(slot, type), env);
                }
                if (Data.isSymbol(var) && !varTable.containsKey(var)) {
                    varTable.put(var, "_var_"+(fields++));
                    if (Logger.tracelevelp(env))
                        Logger.trace("[asm:prepare]"+i+":\tvar ~S"+
                                     " added in varTable",
                                     Lists.list(var), env);
                }
            }
        }
        if (taglist != Symbols.NIL) {
            throw new NotReachedException
                ("unprocessed taglist: ~S.", Lists.list(taglist));
        }
        if (Logger.debuglevelp(env)) {
            Logger.debug("[asm:prepare] ~S: localVars=~S",
                         Lists.list(ci.mi.name(),
                                    Data.toFixnum(ci.localVars)),
                         env);
        }
    }
    private void generateFields(Env env) {
        Iterator it;

        // fields for static subr (singleton) instance
        _cw.visitField
            (Opcodes.ACC_STATIC + Opcodes.ACC_VOLATILE,
             "SELF",
             toTypeDescriptor(super.classInfo.classname()),
             null, null).visitEnd();

        // fields for constants
        it = constTable.keySet().iterator();
        while (it.hasNext()) {
            Object key = it.next(); // key: const object
            String val = Data.string(constTable.get(key));
            _cw.visitField
                (Opcodes.ACC_PRIVATE,
                 val,
                 TYPE_OBJECT.getDescriptor(),
                 null, null).visitEnd();
        }

        // fields for vars
        it = varTable.keySet().iterator();
        while (it.hasNext()) {
            Object key = it.next(); // key: symbol (var)
            String val = Data.string(varTable.get(key));
            _cw.visitField
                (Opcodes.ACC_PRIVATE,
                 val,
                 TYPE_SYMBOL.getDescriptor(),
                 null, null).visitEnd();
        }

        // fields for lls
        it = llTable.keySet().iterator();
        while (it.hasNext()) {
            Object key = it.next(); // key: lambdaList
            String val = Data.string(llTable.get(key));
            _cw.visitField
                (Opcodes.ACC_PRIVATE,
                 val,
                 TYPE_LAMBDA_LIST.getDescriptor(),
                 null, null).visitEnd();
        }
    }
    private void generateClassInitializer(Env env) {
        MethodVisitor mv = _cw.visitMethod
            (Opcodes.ACC_STATIC, "<clinit>", "()V", null, null);

        // field for SELF (static field)
        mv.visitCode();
        mv.visitInsn(Opcodes.ACONST_NULL);
        mv.visitFieldInsn
            (Opcodes.PUTSTATIC,
             toInternalName(super.classInfo.classname()),
             "SELF",
             toTypeDescriptor(super.classInfo.classname()));
        mv.visitInsn(Opcodes.RETURN);
        mv.visitMaxs(0, 0);
        mv.visitEnd();
    }
    private void generateConstructor(Env env) {
        /*
         * local variables (reserved)
         * 0: this
         * 1: env
         */
        MethodVisitor mv = _cw.visitMethod
            (Opcodes.ACC_PUBLIC, "<init>",
             //"(Llapin/lang/Env;)V",
             Type.getMethodDescriptor
             (Type.VOID_TYPE, new Type[]{TYPE_ENV}),
             null, null);

        mv.visitCode();
        mv.visitVarInsn(Opcodes.ALOAD, 0);
        mv.visitLdcInsn(super.classInfo.name().pname());
        mv.visitMethodInsn
            (Opcodes.INVOKESPECIAL,
             "lapin/function/CompiledExpr",
             "<init>",
             //"(Ljava/lang/String;)V");
             Type.getMethodDescriptor
             (Type.VOID_TYPE, new Type[]{TYPE_STRING}));

        Iterator it;

        // fields for constants
        it = constTable.keySet().iterator();
        while (it.hasNext()) {
            Object key = it.next(); // key: object (const)
            String val = Data.string(constTable.get(key));
            mv.visitVarInsn(Opcodes.ALOAD, 0);
            mv.visitVarInsn(Opcodes.ALOAD, 0);
            mv.visitLdcInsn(Printer.prin1ToString(key, env));
            mv.visitVarInsn(Opcodes.ALOAD, 1);
            mv.visitMethodInsn
                (Opcodes.INVOKESPECIAL,
                 "lapin/function/CompiledExpr",
                 "toSexp",
                 Type.getMethodDescriptor
                 (TYPE_OBJECT, new Type[]{TYPE_STRING,TYPE_ENV}));
            mv.visitFieldInsn
                (Opcodes.PUTFIELD,
                 toInternalName(super.classInfo.classname()),
                 val,
                 TYPE_OBJECT.getDescriptor());
        }

        // fields for vars
        it = varTable.keySet().iterator();
        while (it.hasNext()) {
            Object key = it.next(); // key: symbol (var)
            String val = Data.string(varTable.get(key));
            mv.visitVarInsn(Opcodes.ALOAD, 0);
            mv.visitVarInsn(Opcodes.ALOAD, 0);
            mv.visitLdcInsn(Printer.prin1ToString(key, env));
            mv.visitVarInsn(Opcodes.ALOAD, 1);
            mv.visitMethodInsn
                (Opcodes.INVOKESPECIAL,
                 "lapin/function/CompiledExpr",
                 "toSexp",
                 Type.getMethodDescriptor
                 (TYPE_OBJECT, new Type[]{TYPE_STRING,TYPE_ENV}));
            mv.visitMethodInsn
                (Opcodes.INVOKESTATIC,
                 "lapin/lang/Data",
                 "symbol",
                 Type.getMethodDescriptor
                 (TYPE_SYMBOL, new Type[]{TYPE_OBJECT}));
            mv.visitFieldInsn
                (Opcodes.PUTFIELD,
                 toInternalName(super.classInfo.classname()),
                 val,
                 TYPE_SYMBOL.getDescriptor());
        }

        // fields for lambdaLists
        it = llTable.keySet().iterator();
        while (it.hasNext()) {
            Object key = it.next(); // key: lambdaList
            String val = Data.string(llTable.get(key));
            mv.visitVarInsn(Opcodes.ALOAD, 0);
            mv.visitVarInsn(Opcodes.ALOAD, 0);
            mv.visitLdcInsn(Printer.prin1ToString
                            (Data.lambdaList(key).params(), env));
            mv.visitVarInsn(Opcodes.ALOAD, 1);
            mv.visitMethodInsn
                (Opcodes.INVOKESPECIAL,
                 "lapin/function/CompiledExpr",
                 "toLambdaList",
                 Type.getMethodDescriptor
                 (TYPE_LAMBDA_LIST, new Type[]{TYPE_STRING, TYPE_ENV}));
            mv.visitFieldInsn
                (Opcodes.PUTFIELD,
                 toInternalName(super.classInfo.classname()),
                 val,
                 TYPE_LAMBDA_LIST.getDescriptor());
        }

        // field for SELF (static field)
        mv.visitVarInsn(Opcodes.ALOAD, 0);
        mv.visitFieldInsn
            (Opcodes.PUTSTATIC,
             toInternalName(super.classInfo.classname()),
             "SELF",
             toTypeDescriptor(super.classInfo.classname()));
        mv.visitInsn(Opcodes.RETURN);

        mv.visitMaxs(0, 0);
        mv.visitEnd();
    }
    private void generateCall(CallableInfo ci, Env env) {
        /*
         * local variables
         * <Callable#call>
         * 0: this
         * 1: args (list of arguments)
         * 2: env
         *
         * <Callable0#call0>
         * 0: this
         * 1: env
         *
         * <Callable1#call1>
         * 0: this
         * 1: arg0
         * 2: env
         *
         * <Callable2#call2>
         * 0: this
         * 1: arg0
         * 2: arg1
         * 3: env
         *
         * ...
         *
         */
        MethodVisitor mv = _cw.visitMethod
            (ci.mi.implCallable()
             ? Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL
             : Opcodes.ACC_FINAL,
             ci.mi.name(),
             Type.getMethodDescriptor
             (ci.retType, ci.paramTypes),
             null, null);

        // instruction list
        int len = ci.mi.instLen();
        // label
        Label label = null;

        // generate code
        for (int i = 0; i < len; i++) {
            Object inst = ci.mi.getInst(i);
            if (Logger.tracelevelp(env))
                Logger.trace("[asm:gen]"+i+":\t~S",
                             Lists.list(inst), env);

            // inst is symbol
            // -> convert tag (Symbol) to label (ASMe Label object)
            if (Data.isSymbol(inst)) {
                Symbol tag = Data.symbol(inst);
                Label l = (Label) ci.labelTable.get(tag);
                if (l == null) {
                    throw new NotReachedException
                        ("label is null: ~S.", Lists.list(tag));
                }
                else if (l != label) {
                    mv.visitLabel(l);
                    label = l;
                    if (Logger.tracelevelp(env))
                        Logger.trace("[asm:gen]"+i+":\ttag ~S -> label ~S",
                                     Lists.list(tag, l), env);
                }
                else {
                    if (Logger.tracelevelp(env))
                        Logger.trace("[asm:gen]"+i+":\ttag ~S -> label ~S"+
                                     " (dup)",
                                     Lists.list(tag, l), env);
                }
                continue;
            }

            // inst must be the form of (id <arg1> <arg2> ....)
            Object id = Lists.car(inst);
            if (id == Insts.CONST) {
                /* push const on the stack. */
                Object obj = Lists.cadr(inst);
                String val = Data.string(constTable.get(obj));
                mv.visitVarInsn(Opcodes.ALOAD, 0);
                mv.visitFieldInsn
                    (Opcodes.GETFIELD,
                     toInternalName(super.classInfo.classname()),
                     val,
                     TYPE_OBJECT.getDescriptor());
            }
            else if (id == Insts.VAR) {
                /* push var on the stack */
                Object var = Lists.cadr(inst);
                String val = Data.string(varTable.get(var));
                mv.visitVarInsn(Opcodes.ALOAD, 0);
                mv.visitFieldInsn
                    (Opcodes.GETFIELD,
                     toInternalName(super.classInfo.classname()),
                     val,
                     TYPE_SYMBOL.getDescriptor());
            }
            else if (id == Insts.LAMBDA_LIST) {
                /* push lambdaList on the stack */
                Object var = Lists.cadr(inst);
                /* push _ll_<i> on the stack */
                String val = Data.string(llTable.get(var));
                mv.visitVarInsn(Opcodes.ALOAD, 0);
                mv.visitFieldInsn
                    (Opcodes.GETFIELD,
                     toInternalName(super.classInfo.classname()),
                     val,
                     TYPE_LAMBDA_LIST.getDescriptor());
            }
            else if (id == Insts.ENV_GET) {
                /* env.get */
                Class type = Data.javaClass(Lists.cadr(inst));
                if (type.equals(int.class)) {
                    mv.visitMethodInsn
                        (Opcodes.INVOKEVIRTUAL,
                         TYPE_ENV.getInternalName(),
                         "getInt",
                         Type.getMethodDescriptor
                         (Type.INT_TYPE, new Type[] { TYPE_SYMBOL }));
                }
                else if (type.equals(double.class)) {
                    mv.visitMethodInsn
                        (Opcodes.INVOKEVIRTUAL,
                         TYPE_ENV.getInternalName(),
                         "getDouble",
                         Type.getMethodDescriptor
                         (Type.DOUBLE_TYPE, new Type[] { TYPE_SYMBOL }));
                }
                else if (type.equals(char.class)) {
                    mv.visitMethodInsn
                        (Opcodes.INVOKEVIRTUAL,
                         TYPE_ENV.getInternalName(),
                         "getChar",
                         Type.getMethodDescriptor
                         (Type.CHAR_TYPE, new Type[] { TYPE_SYMBOL }));
                }
                else if (Object.class.isAssignableFrom(type)) {
                    mv.visitMethodInsn
                        (Opcodes.INVOKEVIRTUAL,
                         TYPE_ENV.getInternalName(),
                         "get",
                         Type.getMethodDescriptor
                         (TYPE_OBJECT, new Type[] { TYPE_SYMBOL }));
                }
                else {
                    throw new NotReachedException
                        ("unsupported type: ~S.", Lists.list(type));
                }
            }
            else if (id == Insts.ENV_SET) {
                /* env.set */
                Class type = Data.javaClass(Lists.cadr(inst));
                if (type.equals(int.class)) {
                    mv.visitMethodInsn
                        (Opcodes.INVOKEVIRTUAL,
                         TYPE_ENV.getInternalName(),
                         "set",
                         Type.getMethodDescriptor
                         (Type.VOID_TYPE,
                          new Type[] { TYPE_SYMBOL, Type.INT_TYPE }));
                }
                else if (type.equals(double.class)) {
                    mv.visitMethodInsn
                        (Opcodes.INVOKEVIRTUAL,
                         TYPE_ENV.getInternalName(),
                         "set",
                         Type.getMethodDescriptor
                         (Type.VOID_TYPE,
                          new Type[] { TYPE_SYMBOL, Type.DOUBLE_TYPE }));
                }
                else if (type.equals(char.class)) {
                    mv.visitMethodInsn
                        (Opcodes.INVOKEVIRTUAL,
                         TYPE_ENV.getInternalName(),
                         "set",
                         Type.getMethodDescriptor
                         (Type.VOID_TYPE,
                          new Type[] { TYPE_SYMBOL, Type.CHAR_TYPE }));
                }
                else if (Object.class.isAssignableFrom(type)) {
                    mv.visitMethodInsn
                        (Opcodes.INVOKEVIRTUAL,
                         TYPE_ENV.getInternalName(),
                         "set",
                         Type.getMethodDescriptor
                         (Type.VOID_TYPE,
                          new Type[] { TYPE_SYMBOL, TYPE_OBJECT }));
                }
                else {
                    throw new NotReachedException
                        ("unsupported type: ~S.", Lists.list(type));
                }
            }
            else if (id == Insts.ENV_BIND) {
                /* env.bind */
                Class type = Data.javaClass(Lists.cadr(inst));
                if (type.equals(int.class)) {
                    mv.visitMethodInsn
                        (Opcodes.INVOKEVIRTUAL,
                         TYPE_ENV.getInternalName(),
                         "bind",
                         Type.getMethodDescriptor
                         (Type.VOID_TYPE,
                          new Type[] { TYPE_SYMBOL, Type.INT_TYPE }));
                }
                else if (type.equals(double.class)) {
                    mv.visitMethodInsn
                        (Opcodes.INVOKEVIRTUAL,
                         TYPE_ENV.getInternalName(),
                         "bind",
                         Type.getMethodDescriptor
                         (Type.VOID_TYPE,
                          new Type[] { TYPE_SYMBOL, Type.DOUBLE_TYPE }));
                }
                else if (type.equals(char.class)) {
                    mv.visitMethodInsn
                        (Opcodes.INVOKEVIRTUAL,
                         TYPE_ENV.getInternalName(),
                         "bind",
                         Type.getMethodDescriptor
                         (Type.VOID_TYPE,
                          new Type[] { TYPE_SYMBOL, Type.CHAR_TYPE }));
                }
                else if (Object.class.isAssignableFrom(type)) {
                    mv.visitMethodInsn
                        (Opcodes.INVOKEVIRTUAL,
                         TYPE_ENV.getInternalName(),
                         "bind",
                         Type.getMethodDescriptor
                         (Type.VOID_TYPE,
                          new Type[] { TYPE_SYMBOL, TYPE_OBJECT }));
                }
                else {
                    throw new NotReachedException
                        ("unsupported type: ~S.", Lists.list(type));
                }
            }
            else if (id == Insts.ENV_UNBIND) {
                /* env.unbind */
                Class type = Data.javaClass(Lists.cadr(inst));
                if (type.equals(int.class)) {
                    mv.visitMethodInsn
                        (Opcodes.INVOKEVIRTUAL,
                         TYPE_ENV.getInternalName(),
                         "unbindInt",
                         Type.getMethodDescriptor
                         (Type.INT_TYPE, new Type[] { TYPE_SYMBOL }));
                }
                else if (type.equals(double.class)) {
                    mv.visitMethodInsn
                        (Opcodes.INVOKEVIRTUAL,
                         TYPE_ENV.getInternalName(),
                         "unbindDouble",
                         Type.getMethodDescriptor
                         (Type.DOUBLE_TYPE, new Type[] { TYPE_SYMBOL }));
                }
                else if (type.equals(char.class)) {
                    mv.visitMethodInsn
                        (Opcodes.INVOKEVIRTUAL,
                         TYPE_ENV.getInternalName(),
                         "unbindChar",
                         Type.getMethodDescriptor
                         (Type.CHAR_TYPE, new Type[] { TYPE_SYMBOL }));
                }
                else if (Object.class.isAssignableFrom(type)) {
                    mv.visitMethodInsn
                        (Opcodes.INVOKEVIRTUAL,
                         TYPE_ENV.getInternalName(),
                         "unbind",
                         Type.getMethodDescriptor
                         (TYPE_OBJECT, new Type[] { TYPE_SYMBOL }));
                }
                else {
                    throw new NotReachedException
                        ("unsupported type: ~S.", Lists.list(type));
                }
            }
            else if (id == Insts.ENV_CHILD) {
                /* env.child */
                Object oldEnvVar = Lists.cadr(inst);
                Object newEnvVar = Lists.caddr(inst);
                Object oldSlot = Lists.cadr(oldEnvVar);
                Object newSlot = Lists.cadr(newEnvVar);
                int oldLocal = Data.fixnum
                    (ci.localTable.get(oldSlot)).intValue();
                int newLocal = Data.fixnum
                    (ci.localTable.get(newSlot)).intValue();
                if (Logger.tracelevelp(env))
                    Logger.trace
                        ("[asm:gen]"+i+":\tenv-child: local ~S -> ~S",
                         Lists.list(Data.toFixnum(oldLocal),
                                    Data.toFixnum(newLocal)), env);
                mv.visitVarInsn(Opcodes.ALOAD, oldLocal);
                mv.visitMethodInsn
                    (Opcodes.INVOKEVIRTUAL,
                     TYPE_ENV.getInternalName(),
                     "child",
                     Type.getMethodDescriptor(TYPE_ENV, TYPE_NO_ARGS));
                mv.visitVarInsn(Opcodes.ASTORE, newLocal);
            }
            else if (id == Insts.CALL) {
                /* funcall */
                int nargs = Data.fixnum(Lists.cadr(inst)).intValue();
                String className = "lapin.eval.Funcall";
                String methodName = nargs < 0
                    ? "funcall" : "funcall"+nargs;
                Class rType = Object.class;
                Class[] pTypes;
                if (nargs < 0) {
                    pTypes = new Class[] {Function.class,
                                          Object.class, // list of args
                                          Env.class};
                } else {
                    pTypes = new Class[nargs+2];
                    pTypes[0] = Function.class;
                    for (int j = 0; j < nargs; j++) {
                        pTypes[j+1] = Object.class;
                    }
                    pTypes[nargs+1] = Env.class;
                }

                Type retType = Type.getType(rType);
                Type[] paramTypes = new Type[pTypes.length];
                for (int j = 0; j < pTypes.length; j++)
                    paramTypes[j] = Type.getType(pTypes[j]);

                mv.visitMethodInsn
                    (Opcodes.INVOKESTATIC,
                     toInternalName(className),
                     methodName,
                     Type.getMethodDescriptor(retType, paramTypes));
            }
            else if (id == Insts.CALL_DIRECT) {
                /*
                 * public class Foo
                 *   extends CompiledExpr implements Callable2 {
                 *  public Object call2(Object arg0, Object arg1, Env env) {
                 *   ...
                 *  }
                 * }
                 */
                MethodInfo mi = (MethodInfo) Lists.cadr(inst);
                String className = mi.classInfo().classname();
                int nargs = mi.nargs();
                boolean rest = mi.rest();
                String methodName = mi.name();
                Class rType = mi.retType();
                Class[] pTypes = mi.paramTypes();

                Type retType = Type.getType(rType);
                Type[] paramTypes = new Type[pTypes.length];
                for (int j = 0; j < pTypes.length; j++)
                    paramTypes[j] = Type.getType(pTypes[j]);

                mv.visitMethodInsn
                    (Opcodes.INVOKEVIRTUAL,
                     toInternalName(className),
                     methodName,
                     Type.getMethodDescriptor(retType, paramTypes));
            }
            else if (id == Insts.COMPILED_EXPR) {
                /*
                 * public class Foo extends CompiledExpr {
                 *  static public Foo SELF;
                 *  ...
                 * }
                 */
                String className = Data.string(Lists.cadr(inst));
                String fieldName = "SELF";
                String typeName = className;
                mv.visitFieldInsn
                    (Opcodes.GETSTATIC,
                     toInternalName(className),
                     fieldName,
                     toTypeDescriptor(typeName));
            }
            else if (id == Insts.RETURN) {
                /* return */
                Class type = Data.javaClass(Lists.cadr(inst));
                if (type.equals(int.class) ||
                    type.equals(short.class) ||
                    type.equals(byte.class) ||
                    type.equals(char.class)) {
                    mv.visitInsn(Opcodes.IRETURN);
                }
                else if (type.equals(long.class)) {
                    mv.visitInsn(Opcodes.LRETURN);
                }
                else if (type.equals(float.class)) {
                    mv.visitInsn(Opcodes.FRETURN);
                }
                else if (type.equals(double.class)) {
                    mv.visitInsn(Opcodes.DRETURN);
                }
                else if (type.equals(void.class)) {
                    //mv.visitInsn(Opcodes.RETURN);
                    throw new NotReachedException
                        ("unsupported returnType: ~S.", Lists.list(type));
                }
                else {
                    mv.visitInsn(Opcodes.ARETURN);
                }
            }
            else if (id == Insts.IFEQ) {
                /* conditional jump */
                Symbol tag = Data.symbol(Lists.cadr(inst));
                Label l = (Label) ci.labelTable.get(tag);
                if (l == null) {
                    throw new NotReachedException
                        ("label not found: ~S.", Lists.list(tag));
                }
                mv.visitJumpInsn(Opcodes.IFEQ, l);
            }
            else if (id == Insts.IFNE) {
                /* conditional jump */
                Symbol tag = Data.symbol(Lists.cadr(inst));
                Label l = (Label) ci.labelTable.get(tag);
                if (l == null) {
                    throw new NotReachedException
                        ("label not found: ~S.", Lists.list(tag));
                }
                mv.visitJumpInsn(Opcodes.IFNE, l);
            }
            else if (id == Insts.GOTO) {
                /* jump */
                Symbol tag = Data.symbol(Lists.cadr(inst));
                Label l = (Label) ci.labelTable.get(tag);
                if (l == null) {
                    throw new NotReachedException
                        ("label not found: ~S.", Lists.list(tag));
                }
                mv.visitJumpInsn(Opcodes.GOTO, l);
            }
            else if (id == Insts.LOAD) {
                /* local -> stack */
                Object localVar = Lists.cadr(inst);
                Object slot = Lists.cadr(localVar);
                Class type = Data.javaClass(Lists.caddr(localVar));
                int local = Data.fixnum(ci.localTable.get(slot)).intValue();
                int op = Type.getType(type).getOpcode(Opcodes.ILOAD);
                if (Logger.tracelevelp(env))
                    Logger.trace
                        ("[asm:gen]"+i+":\tload: local=~S type=~S",
                         Lists.list(Data.toFixnum(local), type), env);
                mv.visitVarInsn(op, local);
            }
            else if (id == Insts.STORE) {
                /* stack -> local */
                Object localVar = Lists.cadr(inst);
                Object slot = Lists.cadr(localVar);
                Class type = Data.javaClass(Lists.caddr(localVar));
                int local = Data.fixnum(ci.localTable.get(slot)).intValue();
                int op = Type.getType(type).getOpcode(Opcodes.ISTORE);
                if (Logger.tracelevelp(env))
                    Logger.trace
                        ("[asm:gen]"+i+":\tstore: local=~S type=~S",
                         Lists.list(Data.toFixnum(local), type), env);
                mv.visitVarInsn(op, local);
            }
            else if (id == Insts.POP) {
                /* pop a value and discard it */
                Class type = Data.javaClass(Lists.cadr(inst));
                int op;
                switch (Classes.sizeOf(type)) {
                case 1:
                    op = Opcodes.POP;
                    break;
                case 2:
                    op = Opcodes.POP2;
                    break;
                default:
                    throw new NotReachedException
                        ("unsupported type: ~S.", Lists.list(type));
                }
                mv.visitInsn(op);
            }
            else if (id == Insts.DUP) {
                /* peek a value and duplicate it */
                Class type = Data.javaClass(Lists.cadr(inst));
                int op;
                switch (Classes.sizeOf(type)) {
                case 1:
                    op = Opcodes.DUP;
                    break;
                case 2:
                    op = Opcodes.DUP2;
                    break;
                default:
                    throw new NotReachedException
                        ("unsupported type: ~S.", Lists.list(type));
                }
                mv.visitInsn(op);
            }
            else if (id == Insts.PUSH) {
                /* push a constant */
                Object val = Lists.cadr(inst);
                if (Data.isJavaBoolean(val)) {
                    if (Data.javaBoolean(val).booleanValue())
                        mv.visitInsn(Opcodes.ICONST_1);
                    else
                        mv.visitInsn(Opcodes.ICONST_0);
                }
                else if (val instanceof Byte ||
                         val instanceof Short ||
                         val instanceof Integer) {
                    int n = Data.javaNumber(val).intValue();
                    if (n == -1)
                        mv.visitInsn(Opcodes.ICONST_M1);
                    else if (n == 0)
                        mv.visitInsn(Opcodes.ICONST_0);
                    else if (n == 1)
                        mv.visitInsn(Opcodes.ICONST_1);
                    else if (n == 2)
                        mv.visitInsn(Opcodes.ICONST_2);
                    else if (n == 3)
                        mv.visitInsn(Opcodes.ICONST_3);
                    else if (n == 4)
                        mv.visitInsn(Opcodes.ICONST_4);
                    else if (n == 5)
                        mv.visitInsn(Opcodes.ICONST_5);
                    else if (Byte.MIN_VALUE <= n && n <= Byte.MAX_VALUE)
                        mv.visitIntInsn(Opcodes.BIPUSH, n);
                    else if (Short.MIN_VALUE <= n && n <= Short.MAX_VALUE)
                        mv.visitIntInsn(Opcodes.SIPUSH, n);
                    else
                        mv.visitLdcInsn(Data.toFixnum(n));
                }
                else if (val instanceof Long) {
                    long n = Data.javaNumber(val).longValue();
                    if (n == 0L)
                        mv.visitInsn(Opcodes.LCONST_0);
                    else if (n == 1L)
                        mv.visitInsn(Opcodes.LCONST_1);
                    else
                        mv.visitLdcInsn(val);
                }
                else if (val instanceof Float) {
                    float n = Data.javaNumber(val).floatValue();
                    if (n == 0.0f)
                        mv.visitInsn(Opcodes.FCONST_0);
                    else if (n == 1.0f)
                        mv.visitInsn(Opcodes.FCONST_1);
                    else if (n == 2.0f)
                        mv.visitInsn(Opcodes.FCONST_2);
                    else
                        mv.visitLdcInsn(val);
                }
                else if (val instanceof Double) {
                    double n = Data.javaNumber(val).doubleValue();
                    if (n == 0.0)
                        mv.visitInsn(Opcodes.DCONST_0);
                    else if (n == 1.0)
                        mv.visitInsn(Opcodes.DCONST_1);
                    else
                        mv.visitLdcInsn(val);
                }
                else if (Data.isCharacter(val)) {
                    Character c = Data.character(val);
                    int n = (int) c.charValue();
                    if (Byte.MIN_VALUE <= n && n <= Byte.MAX_VALUE)
                        mv.visitIntInsn(Opcodes.BIPUSH, n);
                    else if (Short.MIN_VALUE <= n && n <= Short.MAX_VALUE)
                        mv.visitIntInsn(Opcodes.SIPUSH, n);
                    else
                        mv.visitLdcInsn(Data.toFixnum(n));
                }
                else if (Data.isString(val)) {
                    mv.visitLdcInsn(val);
                }
                else {
                    throw new NotReachedException
                        ("cannot push: ~S.", Lists.list(val));
                }
            }
            else if (id == Insts.GET) {
                Field f = Data.javaField(Lists.cadr(inst));
                String fieldName = f.getName();
                Class c = f.getDeclaringClass();
                String className = c.getName();
                Class t = f.getType();
                String typeName = t.getName();

                boolean isStatic = Classes.isStatic(f);
                int op
                    = isStatic ? Opcodes.GETSTATIC
                    : Opcodes.GETFIELD;

                mv.visitFieldInsn
                    (op,
                     toInternalName(className),
                     fieldName,
                     toTypeDescriptor(typeName));
            }
            else if (id == Insts.PUT) {
                Field f = Data.javaField(Lists.cadr(inst));
                String fieldName = f.getName();
                Class c = f.getDeclaringClass();
                String className = c.getName();
                Class t = f.getType();
                String typeName = t.getName();

                boolean isStatic = Classes.isStatic(f);
                int op
                    = isStatic ? Opcodes.PUTSTATIC
                    : Opcodes.PUTFIELD;

                mv.visitFieldInsn
                    (op,
                     toInternalName(className),
                     fieldName,
                     toTypeDescriptor(typeName));
            }
            else if (id == Insts.INVOKE) {
                Method m = Data.javaMethod(Lists.cadr(inst));
                String methodName = m.getName();
                Class c = m.getDeclaringClass();
                String className = c.getName();
                Class rType = m.getReturnType();
                Class[] pTypes = m.getParameterTypes();
                if (rType.equals(void.class)) {
                    throw new NotReachedException
                        ("unsupported returnType: ~S.", Lists.list(rType));
                }
                Type retType = Type.getType(rType);
                Type[] paramTypes = new Type[pTypes.length];
                for (int j = 0; j < pTypes.length; j++)
                    paramTypes[j] = Type.getType(pTypes[j]);

                boolean isStatic = Classes.isStatic(m);
                boolean isInterface = c.isInterface();
                int op
                    = isStatic ? Opcodes.INVOKESTATIC
                    : isInterface ? Opcodes.INVOKEINTERFACE
                    : Opcodes.INVOKEVIRTUAL;

                mv.visitMethodInsn
                    (op,
                     toInternalName(className),
                     methodName,
                     Type.getMethodDescriptor(retType, paramTypes));
            }
            else if (id == Insts.CHECKCAST) {
                Class c = Data.javaClass(Lists.cadr(inst));
                Type t = Type.getType(c);
                mv.visitTypeInsn(Opcodes.CHECKCAST, t.getInternalName());
            }
            else if (id == Insts.THROW) {
                mv.visitInsn(Opcodes.ATHROW);
            }
            else if (id == Insts.CATCH) {
                Symbol tagS = Data.symbol(Lists.cadr(inst));
                Symbol tagE = Data.symbol(Lists.caddr(inst));
                Symbol tagH = Data.symbol(Lists.cadddr(inst));
                String className;
                if (Lists.isEnd(Lists.cddddr(inst))) {
                    className = null;
                }
                else {
                    Class c = Data.javaClass(Lists.car(Lists.cddddr(inst)));
                    className = toInternalName(c.getName());
                }
                Label labelS = (Label) ci.labelTable.get(tagS);
                if (labelS == null) {
                    throw new NotReachedException
                        ("label not found: ~S.", Lists.list(tagS));
                }
                Label labelE = (Label) ci.labelTable.get(tagE);
                if (labelE == null) {
                    throw new NotReachedException
                        ("label not found: ~S.", Lists.list(tagE));
                }
                Label labelH = (Label) ci.labelTable.get(tagH);
                if (labelH == null) {
                    throw new NotReachedException
                        ("label not found: ~S.", Lists.list(tagH));
                }
                mv.visitTryCatchBlock(labelS, labelE, labelH, className);
            }
            //else if (id == Insts.CATCH_FROM ||
            //         id == Insts.CATCH_TO ||
            //         id == Insts.CATCH_HANDLER) {
            //    /* nothing emitted */
            //    continue;
            //}
            else {
                throw new NotReachedException
                    ("unknown inst: ~S.", Lists.list(inst));
            }
        }
        mv.visitMaxs(0, 0);
        mv.visitEnd();
    }
    private byte[] getBytes(Env env) {
        return _cw.toByteArray();
    }
}
