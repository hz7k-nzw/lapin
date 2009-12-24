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
package lapin.comp.bcel;
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
import java.util.TreeSet;
import org.apache.bcel.Constants;
import org.apache.bcel.generic.BranchInstruction;
import org.apache.bcel.generic.ClassGen;
import org.apache.bcel.generic.ConstantPoolGen;
import org.apache.bcel.generic.FieldGen;
import org.apache.bcel.generic.InstructionConstants;
import org.apache.bcel.generic.InstructionFactory;
import org.apache.bcel.generic.InstructionHandle;
import org.apache.bcel.generic.InstructionList;
import org.apache.bcel.generic.MethodGen;
import org.apache.bcel.generic.ObjectType;
import org.apache.bcel.generic.PUSH;
import org.apache.bcel.generic.Type;

/**
 * ByteCodeGenerator which uses
 * <a href="http://jakarta.apache.org/bcel/">BCEL</a>.
 * @see ByteCodeGenerator
 * @see ClassInfo
 * @see MethodInfo
 */
public final class BCELByteCodeGenerator extends ByteCodeGenerator {
    static private final Type TYPE_ENV = Type.getType(Env.class);
    static private final Type TYPE_LAMBDA_LIST = Type.getType(LambdaList.class);
    static private final Type TYPE_SYMBOL = Type.getType(Symbol.class);

    /*
     * XXX
     * BCEL cannot be used in multi-threaded environment.
     * (This LOCK is used in doGenerate method.)
     */
    static private final Object LOCK = new Object();

    static class JumpInfo {
        /** index of the intermediate instruction labeled by the tag. */
        int index;
        /** tag object which labels the next instruction. */
        Symbol tag;
        /** instruction handle object specified by the index. */
        InstructionHandle ih;
        JumpInfo(int index, Symbol tag, InstructionHandle ih) {
            this.index = index;
            this.tag = tag;
            this.ih = ih;
        }
    }/* end of JumpInfo */

    static class BranchInfo {
        /** index of the intermediate instruction. */
        int index;
        /** tag object which represents GOTO-target. */
        Symbol tag;
        /** branch instruction object specified by the index */
        BranchInstruction bi;
        BranchInfo(int index, Symbol tag, BranchInstruction bi) {
            this.index = index;
            this.tag = tag;
            this.bi = bi;
        }
    }/* end of BranchInfo */

    static class ExceptionInfo {
        Symbol tagStart;
        Symbol tagEnd;
        Symbol tagHandler;
        Class exceptionType;
        ExceptionInfo(Symbol s, Symbol e, Symbol h, Class t) {
            this.tagStart = s;
            this.tagEnd = e;
            this.tagHandler = h;
            this.exceptionType = t;
        }
    }/* end of ExceptionInfo */

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
         * param types of call (or call[N]) method
         */
        Type[] paramTypes;
        /**
         * param names of call (or call[N]) method
         */
        String[] paramNames;
        /**
         * return type of call (or call[N]) method
         */
        Type retType;

        /** table: (var, slot of local var) */
        final HashMap localTable = new HashMap();
        /** table: (tag, jumpInfo) */
        final HashMap jumpTable = new HashMap();
        /** table: (index of instruction, branchInfo) */
        final HashMap branchTable = new HashMap();
        /** table: (index of instruction, exceptionInfo) */
        final HashMap exceptionTable = new HashMap();

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

            if (nargs >= 0) {
                if (rest) {
                    this.paramNames = new String[nargs+2];
                    for (int i = 0; i < nargs; i++)
                        this.paramNames[i] = "arg"+i;
                    this.paramNames[nargs] = "args";
                    this.paramNames[nargs+1] = "env";
                }
                else {
                    this.paramNames = new String[nargs+1];
                    for (int i = 0; i < nargs; i++)
                        this.paramNames[i] = "arg"+i;
                    this.paramNames[nargs] = "env";
                }
            }
            else {
                this.paramNames = new String[] { "args", "env" };
            }

            if (Logger.debuglevelp(env)) {
                Logger.debug("[bcel:init] interface =~S",
                             Lists.list(interfaceName), env);
                //Logger.debug("[bcel:init] localVars =~S",
                //             Lists.list(Data.toFixnum(localVars)), env);
                Logger.debug("[bcel:init] methodName=~S",
                             Lists.list(mi.name()), env);
                Logger.debug("[bcel:init] params.len=~S",
                             Lists.list(Data.toFixnum(pTypes.length)),
                             env);
            }
        }
    }/* end of CallableInfo */

    /** BCEL ClassGen object */
    private ClassGen _cg;
    /** BCEL ConstantPoolGen object */
    private ConstantPoolGen _cp;
    /** BCEL InstructionFactory object */
    private InstructionFactory _factory;

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
            Logger.debug("[bcel:init] className =~S",
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
        /*
         * XXX
         * BCEL cannot be used in multi-threaded environment.
         */
        synchronized (LOCK) {
            return _doGenerate(env);
        }
    }
    private byte[] _doGenerate(Env env) {
        if (finished) {
            throw new NotReachedException
                ("repeated use of the same object is not allowed",
                 Symbols.NIL);
        }
        try {
            initBCELObject(env);
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
    private void initBCELObject(Env env) {
        ArrayList al = new ArrayList();
        for (Object l = ciList; !Lists.isEnd(l); l = Lists.cdr(l)) {
            CallableInfo ci = (CallableInfo) Lists.car(l);
            if (ci.mi.implCallable())
                al.add(ci.interfaceName);
        }
        String[] ifNames = new String[al.size()];
        al.toArray(ifNames);
        // classGen
        _cg = new ClassGen
            (super.classInfo.classname(),
             "lapin.function.CompiledExpr", "<Unknown>",
             Constants.ACC_PUBLIC | Constants.ACC_SUPER, ifNames);
        // constantPool
        _cp = _cg.getConstantPool();
        // instructionFactory
        _factory = new InstructionFactory(_cg, _cp);
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
            if (id == Insts.CATCH_FROM) {
                if (i == len-1) {
                    throw new NotReachedException
                        ("CATCH-FROM cannot be put "+
                         "at the tail of the code list: ~S.",
                         Lists.list(inst));
                }
                Symbol tag = Data.symbol(Lists.cadr(inst));
                ci.mi.setInst(i, tag);
            }
            else if (id == Insts.CATCH_TO) {
                if (i == 0) {
                    throw new NotReachedException
                        ("CATCH-TO cannot be put "+
                         "at the head of the code list: ~S.",
                         Lists.list(inst));
                }
                Symbol tag = Data.symbol(Lists.cadr(inst));
                Object prevInst = ci.mi.getInst(i-1);
                if (!Data.isList(prevInst)) {
                    throw new NotReachedException
                        ("CATCH-TO cannot be put after a tag: ~S.",
                         Lists.list(inst));
                }
                // swap: tag <-> prevInst
                ci.mi.setInst(i-1, tag);
                ci.mi.setInst(i, prevInst);
            }
            else if (id == Insts.CATCH_HANDLER) {
                if (i == len-1) {
                    throw new NotReachedException
                        ("CATCH-HANDLER cannot be put "+
                         "at the tail of the code list: ~S.",
                         Lists.list(inst));
                }
                Symbol tag = Data.symbol(Lists.cadr(inst));
                ci.mi.setInst(i, tag);
            }
        }

        // 2. store info in xxxTable
        for (int i = 0; i < len; i++) {
            Object inst = ci.mi.getInst(i);
            if (Logger.tracelevelp(env))
                Logger.trace("[bcel:prepare]"+i+":\t~S",
                             Lists.list(inst), env);

            // inst is symbol
            if (Data.isSymbol(inst)) {
                // push tag to taglist
                Symbol tag = Data.symbol(inst);
                taglist = Lists.cons(tag, taglist);
                continue;
            }

            // process taglist (regist jumpinfo)
            if (taglist != Symbols.NIL) {
                for (Object l = taglist; !Lists.isEnd(l); l = Lists.cdr(l)) {
                    Symbol tag = Data.symbol(Lists.car(l));
                    ci.jumpTable.put(tag, new JumpInfo(i, tag, null));
                    if (Logger.tracelevelp(env))
                        Logger.trace("[bcel:prepare]"+i+":\ttag ~S"+
                                     " added in jumpTable", 
                                     Lists.list(tag), env);
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
                        Logger.trace("[bcel:prepare]"+i+":\tobj ~S"+
                                     " added in constTable",
                                     Lists.list(obj), env);
                }
            }
            else if (id == Insts.VAR) {
                Symbol var = Data.symbol(Lists.cadr(inst));
                if (!varTable.containsKey(var)) {
                    varTable.put(var, "_var_"+(fields++));
                    if (Logger.tracelevelp(env))
                        Logger.trace("[bcel:prepare]"+i+":\tvar ~S"+
                                     " added in varTable",
                                     Lists.list(var), env);
                }
            }
            else if (id == Insts.LAMBDA_LIST) {
                LambdaList ll = Data.lambdaList(Lists.cadr(inst));
                if (!llTable.containsKey(ll)) {
                    llTable.put(ll, "_ll_"+(fields++));
                    if (Logger.tracelevelp(env))
                        Logger.trace("[bcel:prepare]"+i+":\tll ~S"+
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
                        Logger.trace("[bcel:prepare]"+i+":\toldSlot ~S"+
                                     " added in localTable",
                                     Lists.list(oldSlot), env);
                }
                if (Data.isSymbol(oldVar) && !varTable.containsKey(oldVar)) {
                    varTable.put(oldVar, "_var_"+(fields++));
                    if (Logger.tracelevelp(env))
                        Logger.trace("[bcel:prepare]"+i+":\tvar ~S"+
                                     " added in varTable",
                                     Lists.list(oldVar), env);
                }
                if (!ci.localTable.containsKey(newSlot)) {
                    ci.localTable.put(newSlot, Data.toFixnum(ci.localVars++));
                    if (Logger.tracelevelp(env))
                        Logger.trace("[bcel:prepare]"+i+":\tnewSlot ~S"+
                                     " added in localTable",
                                     Lists.list(newSlot), env);
                }
                if (Data.isSymbol(newVar) && !varTable.containsKey(newVar)) {
                    varTable.put(newVar, "_var_"+(fields++));
                    if (Logger.tracelevelp(env))
                        Logger.trace("[bcel:prepare]"+i+":\tvar ~S"+
                                     " added in varTable",
                                     Lists.list(newVar), env);
                }
            }
            else if (id == Insts.IFEQ ||
                     id == Insts.IFNE ||
                     id == Insts.GOTO) {
                Symbol tag = Data.symbol(Lists.cadr(inst));
                ci.branchTable.put(Data.toFixnum(i),
                                   new BranchInfo(i, tag, null));
                if (Logger.tracelevelp(env))
                    Logger.trace("[bcel:prepare]"+i+":\ttag ~S"+
                                 " added in branchTable",
                                 Lists.list(tag), env);
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
                        Object slot2= Data.toFixnum
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
                        Logger.trace("[bcel:prepare]"+i+":\tslot ~S"+
                                     " added in localTable: type is ~S",
                                     Lists.list(slot, type), env);
                }
                if (Data.isSymbol(var) && !varTable.containsKey(var)) {
                    varTable.put(var, "_var_"+(fields++));
                    if (Logger.tracelevelp(env))
                        Logger.trace("[bcel:prepare]"+i+":\tvar ~S"+
                                     " added in varTable",
                                     Lists.list(var), env);
                }
            }
            else if (id == Insts.CATCH) {
                Symbol tagS = Data.symbol(Lists.cadr(inst));
                Symbol tagE = Data.symbol(Lists.caddr(inst));
                Symbol tagH = Data.symbol(Lists.cadddr(inst));
                Class clazz;
                if (Lists.isEnd(Lists.cddddr(inst))) {
                    clazz = null;
                }
                else {
                    clazz = Data.javaClass(Lists.car(Lists.cddddr(inst)));
                }
                ci.exceptionTable.put(Data.toFixnum(i),
                                      new ExceptionInfo(tagS, tagE,
                                                        tagH, clazz));
                if (Logger.tracelevelp(env))
                    Logger.trace("[bcel:prepare]"+i+":\tinfo ~S"+
                                 " added in exceptionTable",
                                 Lists.list(Lists.cdr(inst)), env);
            }
        }
        if (taglist != Symbols.NIL) {
            throw new NotReachedException
                ("unprocessed taglist: ~S.", Lists.list(taglist));
        }
        if (Logger.debuglevelp(env)) {
            Logger.debug("[bcel:prepare] ~S: localVars=~S",
                         Lists.list(ci.mi.name(),
                                    Data.toFixnum(ci.localVars)),
                         env);
        }
    }
    private void generateFields(Env env) {
        FieldGen field;
        Iterator it;

        // fields for static subr (singleton) instance
        field = new FieldGen
            (Constants.ACC_STATIC | Constants.ACC_VOLATILE,
             new ObjectType(super.classInfo.classname()), "SELF", _cp);
        _cg.addField(field.getField());

        // fields for constants
        it = constTable.keySet().iterator();
        while (it.hasNext()) {
            Object key = it.next(); // key: const object
            String val = Data.string(constTable.get(key));
            field = new FieldGen
                (Constants.ACC_PRIVATE, Type.OBJECT, val, _cp);
            _cg.addField(field.getField());
        }

        // fields for vars
        it = varTable.keySet().iterator();
        while (it.hasNext()) {
            Object key = it.next(); // key: symbol (var)
            String val = Data.string(varTable.get(key));
            field = new FieldGen
                (Constants.ACC_PRIVATE, TYPE_SYMBOL, val, _cp);
            _cg.addField(field.getField());
        }

        // fields for lls
        it = llTable.keySet().iterator();
        while (it.hasNext()) {
            Object key = it.next(); // key: lambdaList
            String val = Data.string(llTable.get(key));
            field = new FieldGen
                (Constants.ACC_PRIVATE, TYPE_LAMBDA_LIST, val, _cp);
            _cg.addField(field.getField());
        }
    }
    private void generateClassInitializer(Env env) {
        InstructionList il = new InstructionList();
        MethodGen method = new MethodGen
            (Constants.ACC_STATIC, Type.VOID, Type.NO_ARGS,
             new String[] {  /* no args */ }, "<clinit>",
             super.classInfo.classname(), il, _cp);

        // field for SELF (static field)
        il.append(InstructionConstants.ACONST_NULL);
        il.append(_factory.createFieldAccess
                  (super.classInfo.classname(), "SELF",
                   new ObjectType(super.classInfo.classname()),
                   Constants.PUTSTATIC));

        il.append(_factory.createReturn(Type.VOID));
        method.setMaxStack();
        method.setMaxLocals();
        _cg.addMethod(method.getMethod());
        il.dispose();
    }
    private void generateConstructor(Env env) {
        /*
         * local variables (reserved)
         * 0: this
         * 1: env
         */
        InstructionList il = new InstructionList();
        MethodGen method = new MethodGen
            (Constants.ACC_PUBLIC, Type.VOID,
             new Type[] { TYPE_ENV }, new String[] { "env" },
             "<init>", super.classInfo.classname(), il, _cp);

        // <init>
        il.append(_factory.createLoad(Type.OBJECT, 0));
        il.append(new PUSH(_cp, super.classInfo.name().pname()));
        il.append(_factory.createInvoke
                  ("lapin.function.CompiledExpr", "<init>",
                   Type.VOID, new Type[] { Type.STRING },
                   Constants.INVOKESPECIAL));

        Iterator it;

        // fields for constants
        it = constTable.keySet().iterator();
        while (it.hasNext()) {
            Object key = it.next(); // key: object (const)
            String val = Data.string(constTable.get(key));
            il.append(_factory.createLoad(Type.OBJECT, 0));
            il.append(_factory.createLoad(Type.OBJECT, 0));
            il.append(new PUSH(_cp, Printer.prin1ToString(key, env)));
            il.append(_factory.createLoad(Type.OBJECT, 1));
            il.append(_factory.createInvoke
                      ("lapin.function.CompiledExpr", "toSexp",
                       Type.OBJECT, new Type[] { Type.STRING, TYPE_ENV },
                       Constants.INVOKESPECIAL));
            il.append(_factory.createFieldAccess
                      (super.classInfo.classname(), val, Type.OBJECT,
                       Constants.PUTFIELD));
        }

        // fields for vars
        it = varTable.keySet().iterator();
        while (it.hasNext()) {
            Object key = it.next(); // key: symbol (var)
            String val = Data.string(varTable.get(key));
            il.append(_factory.createLoad(Type.OBJECT, 0));
            il.append(_factory.createLoad(Type.OBJECT, 0));
            il.append(new PUSH(_cp, Printer.prin1ToString(key, env)));
            il.append(_factory.createLoad(Type.OBJECT, 1));
            il.append(_factory.createInvoke
                      ("lapin.function.CompiledExpr", "toSexp",
                       Type.OBJECT, new Type[] { Type.STRING, TYPE_ENV },
                       Constants.INVOKESPECIAL));
            il.append(_factory.createInvoke
                      ("lapin.lang.Data", "symbol", 
                       TYPE_SYMBOL, new Type[] { Type.OBJECT },
                       Constants.INVOKESTATIC));
            il.append(_factory.createFieldAccess
                      (super.classInfo.classname(), val, TYPE_SYMBOL,
                       Constants.PUTFIELD));
        }

        // fields for lambdaLists
        it = llTable.keySet().iterator();
        while (it.hasNext()) {
            Object key = it.next(); // key: lambdaList
            String val = Data.string(llTable.get(key));
            il.append(_factory.createLoad(Type.OBJECT, 0));
            il.append(_factory.createLoad(Type.OBJECT, 0));
            il.append(new PUSH(_cp, Printer.prin1ToString
                               (Data.lambdaList(key).params(), env)));
            il.append(_factory.createLoad(Type.OBJECT, 1));
            il.append(_factory.createInvoke
                      ("lapin.function.CompiledExpr", "toLambdaList",
                       TYPE_LAMBDA_LIST, new Type[] { Type.STRING, TYPE_ENV },
                       Constants.INVOKESPECIAL));
            il.append(_factory.createFieldAccess
                      (super.classInfo.classname(), val, TYPE_LAMBDA_LIST,
                       Constants.PUTFIELD));
        }

        // field for SELF (static field)
        il.append(_factory.createLoad(Type.OBJECT, 0));
        il.append(_factory.createFieldAccess
                  (super.classInfo.classname(), "SELF",
                   new ObjectType(super.classInfo.classname()),
                   Constants.PUTSTATIC));

        il.append(_factory.createReturn(Type.VOID));
        method.setMaxStack();
        method.setMaxLocals();
        _cg.addMethod(method.getMethod());
        il.dispose();
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
        InstructionList il = new InstructionList();

        MethodGen method = new MethodGen
            (ci.mi.implCallable()
             ? Constants.ACC_PUBLIC | Constants.ACC_FINAL
             : Constants.ACC_FINAL,
             ci.retType, ci.paramTypes,
             ci.paramNames, ci.mi.name(),
             super.classInfo.classname(), il, _cp);

        // instruction list
        int len = ci.mi.instLen();
        // list of label-tag
        Object taglist = Symbols.NIL;
        // instruction handle
        InstructionHandle ih = null;

        // generate code
        for (int i = 0; i < len; i++) {
            Object inst = ci.mi.getInst(i);
            if (Logger.tracelevelp(env))
                Logger.trace("[bcel:gen]"+i+":\t~S",
                             Lists.list(inst), env);

            // inst is symbol
            // -> push the symbol to taglist and go to next
            if (Data.isSymbol(inst)) {
                Symbol tag = Data.symbol(inst);
                taglist = Lists.cons(tag, taglist);
                continue;
            }

            // inst must be the form of (id <arg1> <arg2> ....)
            Object id = Lists.car(inst);
            if (id == Insts.CONST) {
                /* push const on the stack. */
                Object obj = Lists.cadr(inst);
                String val = Data.string(constTable.get(obj));
                ih = il.append(_factory.createLoad(Type.OBJECT, 0));
                il.append(_factory.createFieldAccess
                          (super.classInfo.classname(), val, Type.OBJECT,
                           Constants.GETFIELD));
            }
            else if (id == Insts.VAR) {
                /* push var on the stack */
                Object var = Lists.cadr(inst);
                String val = Data.string(varTable.get(var));
                ih = il.append(_factory.createLoad(Type.OBJECT, 0));
                il.append(_factory.createFieldAccess
                          (super.classInfo.classname(), val, TYPE_SYMBOL,
                           Constants.GETFIELD));
            }
            else if (id == Insts.LAMBDA_LIST) {
                /* push lambdaList on the stack */
                Object var = Lists.cadr(inst);
                /* push _ll_<i> on the stack */
                String val = Data.string(llTable.get(var));
                ih = il.append(_factory.createLoad(Type.OBJECT, 0));
                il.append(_factory.createFieldAccess
                          (super.classInfo.classname(), val, TYPE_LAMBDA_LIST,
                           Constants.GETFIELD));
            }
            else if (id == Insts.ENV_GET) {
                /* env.get */
                Class type = Data.javaClass(Lists.cadr(inst));
                if (type.equals(int.class)) {
                    ih = il.append(_factory.createInvoke
                                   ("lapin.lang.Env", "getInt",
                                    Type.INT, new Type[] { TYPE_SYMBOL },
                                    Constants.INVOKEVIRTUAL));
                }
                else if (type.equals(double.class)) {
                    ih = il.append(_factory.createInvoke
                                   ("lapin.lang.Env", "getDouble",
                                    Type.DOUBLE, new Type[] { TYPE_SYMBOL },
                                    Constants.INVOKEVIRTUAL));
                }
                else if (type.equals(char.class)) {
                    ih = il.append(_factory.createInvoke
                                   ("lapin.lang.Env", "getChar",
                                    Type.CHAR, new Type[] { TYPE_SYMBOL },
                                    Constants.INVOKEVIRTUAL));
                }
                else if (Object.class.isAssignableFrom(type)) {
                    ih = il.append(_factory.createInvoke
                                   ("lapin.lang.Env", "get",
                                    Type.OBJECT, new Type[] { TYPE_SYMBOL },
                                    Constants.INVOKEVIRTUAL));
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
                    ih = il.append(_factory.createInvoke
                                   ("lapin.lang.Env", "set",
                                    Type.VOID,
                                    new Type[] { TYPE_SYMBOL, Type.INT },
                                    Constants.INVOKEVIRTUAL));
                }
                else if (type.equals(double.class)) {
                    ih = il.append(_factory.createInvoke
                                   ("lapin.lang.Env", "set",
                                    Type.VOID,
                                    new Type[] { TYPE_SYMBOL, Type.DOUBLE },
                                    Constants.INVOKEVIRTUAL));
                }
                else if (type.equals(char.class)) {
                    ih = il.append(_factory.createInvoke
                                   ("lapin.lang.Env", "set",
                                    Type.VOID,
                                    new Type[] { TYPE_SYMBOL, Type.CHAR },
                                    Constants.INVOKEVIRTUAL));
                }
                else if (Object.class.isAssignableFrom(type)) {
                    ih = il.append(_factory.createInvoke
                                   ("lapin.lang.Env", "set",
                                    Type.VOID,
                                    new Type[] { TYPE_SYMBOL, Type.OBJECT },
                                    Constants.INVOKEVIRTUAL));
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
                    ih = il.append(_factory.createInvoke
                                   ("lapin.lang.Env", "bind",
                                    Type.VOID,
                                    new Type[] { TYPE_SYMBOL, Type.INT },
                                    Constants.INVOKEVIRTUAL));
                }
                else if (type.equals(double.class)) {
                    ih = il.append(_factory.createInvoke
                                   ("lapin.lang.Env", "bind",
                                    Type.VOID,
                                    new Type[] { TYPE_SYMBOL, Type.DOUBLE },
                                    Constants.INVOKEVIRTUAL));
                }
                else if (type.equals(char.class)) {
                    ih = il.append(_factory.createInvoke
                                   ("lapin.lang.Env", "bind",
                                    Type.VOID,
                                    new Type[] { TYPE_SYMBOL, Type.CHAR },
                                    Constants.INVOKEVIRTUAL));
                }
                else if (Object.class.isAssignableFrom(type)) {
                    ih = il.append(_factory.createInvoke
                                   ("lapin.lang.Env", "bind",
                                    Type.VOID,
                                    new Type[] { TYPE_SYMBOL, Type.OBJECT },
                                    Constants.INVOKEVIRTUAL));
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
                    ih = il.append(_factory.createInvoke
                                   ("lapin.lang.Env", "unbindInt",
                                    Type.INT, new Type[] { TYPE_SYMBOL },
                                    Constants.INVOKEVIRTUAL));
                }
                else if (type.equals(double.class)) {
                    ih = il.append(_factory.createInvoke
                                   ("lapin.lang.Env", "unbindDouble",
                                    Type.DOUBLE, new Type[] { TYPE_SYMBOL },
                                    Constants.INVOKEVIRTUAL));
                }
                else if (type.equals(char.class)) {
                    ih = il.append(_factory.createInvoke
                                   ("lapin.lang.Env", "unbindChar",
                                    Type.DOUBLE, new Type[] { TYPE_SYMBOL },
                                    Constants.INVOKEVIRTUAL));
                }
                else if (Object.class.isAssignableFrom(type)) {
                    ih = il.append(_factory.createInvoke
                                   ("lapin.lang.Env", "unbind",
                                    Type.OBJECT, new Type[] { TYPE_SYMBOL },
                                    Constants.INVOKEVIRTUAL));
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
                        ("[bcel:gen]"+i+":\tenv-child: local ~S -> ~S",
                         Lists.list(Data.toFixnum(oldLocal),
                                    Data.toFixnum(newLocal)), env);
                ih = il.append(_factory.createLoad(TYPE_ENV, oldLocal));
                il.append(_factory.createInvoke
                          ("lapin.lang.Env", "child",
                           TYPE_ENV, Type.NO_ARGS,
                           Constants.INVOKEVIRTUAL));
                il.append(_factory.createStore(TYPE_ENV, newLocal));
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

                ih = il.append(_factory.createInvoke
                               (className, methodName,
                                retType, paramTypes,
                                Constants.INVOKESTATIC));
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

                ih = il.append(_factory.createInvoke
                               (className, methodName,
                                retType, paramTypes,
                                Constants.INVOKEVIRTUAL));
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
                ih = il.append(_factory.createFieldAccess
                               (className, fieldName,
                                new ObjectType(typeName),
                                Constants.GETSTATIC));
            }
            else if (id == Insts.RETURN) {
                /* return */
                Class type = Data.javaClass(Lists.cadr(inst));
                ih = il.append(_factory.createReturn(Type.getType(type)));
            }
            else if (id == Insts.IFEQ) {
                /* conditional jump */
                BranchInstruction bi_ifeq = _factory
                    .createBranchInstruction(Constants.IFEQ, null);
                ih = il.append(bi_ifeq);
                BranchInfo bi = (BranchInfo)
                    ci.branchTable.get(Data.toFixnum(i));
                bi.bi = bi_ifeq;
            }
            else if (id == Insts.IFNE) {
                /* conditional jump */
                BranchInstruction bi_ifne = _factory
                    .createBranchInstruction(Constants.IFNE, null);
                ih = il.append(bi_ifne);
                BranchInfo bi = (BranchInfo)
                    ci.branchTable.get(Data.toFixnum(i));
                bi.bi = bi_ifne;
            }
            else if (id == Insts.GOTO) {
                /* jump */
                BranchInstruction bi_goto = _factory
                    .createBranchInstruction(Constants.GOTO, null);
                ih = il.append(bi_goto);
                BranchInfo bi = (BranchInfo)
                    ci.branchTable.get(Data.toFixnum(i));
                bi.bi = bi_goto;
            }
            else if (id == Insts.LOAD) {
                /* local -> stack */
                Object localVar = Lists.cadr(inst);
                Object slot = Lists.cadr(localVar);
                Class type = Data.javaClass(Lists.caddr(localVar));
                int local = Data.fixnum(ci.localTable.get(slot)).intValue();
                if (Logger.tracelevelp(env))
                    Logger.trace
                        ("[bcel:gen]"+i+":\tload: local=~S type=~S",
                         Lists.list(Data.toFixnum(local), type), env);
                ih = il.append(_factory.createLoad
                               (Type.getType(type), local));
            }
            else if (id == Insts.STORE) {
                /* stack -> local */
                Object localVar = Lists.cadr(inst);
                Object slot = Lists.cadr(localVar);
                Class type = Data.javaClass(Lists.caddr(localVar));
                int local = Data.fixnum(ci.localTable.get(slot)).intValue();
                if (Logger.tracelevelp(env))
                    Logger.trace
                        ("[bcel:gen]"+i+":\tstore: local=~S type=~S",
                         Lists.list(Data.toFixnum(local), type), env);
                ih = il.append(_factory.createStore
                               (Type.getType(type), local));
            }
            else if (id == Insts.POP) {
                /* pop a value and discard it */
                Class type = Data.javaClass(Lists.cadr(inst));
                int categ = Classes.sizeOf(type);
                ih = il.append(_factory.createPop(categ));
            }
            else if (id == Insts.DUP) {
                /* peek a value and duplicate it */
                Class type = Data.javaClass(Lists.cadr(inst));
                int categ = Classes.sizeOf(type);
                ih = il.append(_factory.createDup(categ));
            }
            else if (id == Insts.PUSH) {
                /* push a constant */
                Object val = Lists.cadr(inst);
                if (Data.isJavaBoolean(val))
                    ih = il.append(new PUSH(_cp, Data.javaBoolean(val)));
                else if (Data.isJavaNumber(val))
                    ih = il.append(new PUSH(_cp, Data.javaNumber(val)));
                else if (Data.isCharacter(val))
                    ih = il.append(new PUSH(_cp, Data.character(val)));
                else if (Data.isString(val))
                    ih = il.append(new PUSH(_cp, Data.string(val)));
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

                boolean isStatic = Classes.isStatic(f);
                short op
                    = isStatic ? Constants.GETSTATIC
                    : Constants.GETFIELD;

                ih = il.append(_factory.createFieldAccess
                               (className, fieldName,
                                Type.getType(t), op));
            }
            else if (id == Insts.PUT) {
                Field f = Data.javaField(Lists.cadr(inst));
                String fieldName = f.getName();
                Class c = f.getDeclaringClass();
                String className = c.getName();
                Class t = f.getType();
                String typeName = t.getName();

                boolean isStatic = Classes.isStatic(f);
                short op
                    = isStatic ? Constants.PUTSTATIC
                    : Constants.PUTFIELD;

                ih = il.append(_factory.createFieldAccess
                               (className, fieldName,
                                Type.getType(t), op));
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
                short op
                    = isStatic ? Constants.INVOKESTATIC
                    : isInterface ? Constants.INVOKEINTERFACE
                    : Constants.INVOKEVIRTUAL;

                ih = il.append(_factory.createInvoke
                               (className, methodName,
                                retType, paramTypes, op));
            }
            else if (id == Insts.CHECKCAST) {
                Class c = Data.javaClass(Lists.cadr(inst));
                String className = c.getName();
                ih = il.append(_factory.createCheckCast
                               (new ObjectType(className)));
            }
            else if (id == Insts.THROW) {
                ih = il.append(InstructionConstants.ATHROW);
            }
            else if (id == Insts.CATCH) {
                /* nothing emitted */
                continue;
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

            // when taglist is not nil,
            // set InstructionHandle to JumpInfo
            if (taglist != Symbols.NIL) {
                if (ih == null) {
                    throw new NotReachedException
                        ("ih is null: ~S.", Lists.list(inst));
                }
                for (Object l = taglist; !Lists.isEnd(l); l = Lists.cdr(l)) {
                    Symbol tag = Data.symbol(Lists.car(l));
                    JumpInfo ji = (JumpInfo) ci.jumpTable.get(tag);
                    ji.ih = ih;
                }
                // reset taglist
                taglist = Symbols.NIL;
            }
            ih = null;
        }
        if (taglist != Symbols.NIL) {
            throw new NotReachedException
                ("unprocessed taglist: ~S.", Lists.list(taglist));
        }

        Iterator it;

        // connect targets
        it = new TreeSet(ci.branchTable.keySet()).iterator();
        while (it.hasNext()) {
            Object key = it.next();
            BranchInfo bi = (BranchInfo) ci.branchTable.get(key);
            if (bi.bi == null) {
                throw new NotReachedException
                    ("BranchInstraction for ~S is null: ["+bi.index+"] ~S.",
                     Lists.list(key, ci.mi.getInst(bi.index)));
            }
            JumpInfo ji = (JumpInfo) ci.jumpTable.get(bi.tag);
            if (ji.ih == null) {
                throw new NotReachedException
                    ("InstructionHandle for ~S is null: ["+ji.index+"] ~S.",
                     Lists.list(bi.tag, ci.mi.getInst(ji.index)));
            }
            if (Logger.tracelevelp(env))
                Logger.trace
                    ("[bcel:gen] connect "+
                     "["+bi.index+"] ~S -> ["+ji.index+"] ~S",
                     Lists.list(ci.mi.getInst(bi.index),
                                ci.mi.getInst(ji.index)), env);

            bi.bi.setTarget(ji.ih);
        }

        // construct exception table
        it = new TreeSet(ci.exceptionTable.keySet()).iterator();
        while (it.hasNext()) {
            Object key = it.next();
            ExceptionInfo ei = (ExceptionInfo) ci.exceptionTable.get(key);
            InstructionHandle ihStart,ihEnd,ihHandler;
            ObjectType type;
            Symbol tag;
            JumpInfo ji;
            // start
            tag = ei.tagStart;
            ji= (JumpInfo) ci.jumpTable.get(tag);
            if (ji.ih == null) {
                throw new NotReachedException
                    ("InstructionHandle for ~S is null: ["+ji.index+"] ~S.",
                     Lists.list(tag, ci.mi.getInst(ji.index)));
            }
            ihStart = ji.ih;
            if (Logger.tracelevelp(env))
                Logger.trace
                    ("[bcel:gen] exceptionTable ~S -> ["+ji.index+"] ~S",
                     Lists.list(tag, ci.mi.getInst(ji.index)), env);
            // end
            tag = ei.tagEnd;
            ji= (JumpInfo) ci.jumpTable.get(tag);
            if (ji.ih == null) {
                throw new NotReachedException
                    ("InstructionHandle for ~S is null: ["+ji.index+"] ~S.",
                     Lists.list(tag, ci.mi.getInst(ji.index)));
            }
            ihEnd = ji.ih;
            if (Logger.tracelevelp(env))
                Logger.trace
                    ("[bcel:gen] exceptionTable ~S -> ["+ji.index+"] ~S",
                     Lists.list(tag, ci.mi.getInst(ji.index)), env);
            // handler
            tag = ei.tagHandler;
            ji= (JumpInfo) ci.jumpTable.get(tag);
            if (ji.ih == null) {
                throw new NotReachedException
                    ("InstructionHandle for ~S is null: ["+ji.index+"] ~S.",
                     Lists.list(tag, ci.mi.getInst(ji.index)));
            }
            ihHandler = ji.ih;
            if (Logger.tracelevelp(env))
                Logger.trace
                    ("[bcel:gen] exceptionTable ~S -> ["+ji.index+"] ~S",
                     Lists.list(tag, ci.mi.getInst(ji.index)), env);
            // type
            if (ei.exceptionType == null)
                type = null;
            else
                type = (ObjectType) Type.getType(ei.exceptionType);
            if (Logger.tracelevelp(env))
                Logger.trace
                    ("[bcel:gen] exceptionTable [type] ~S",
                     Lists.list(ei.exceptionType), env);
            method.addExceptionHandler(ihStart, ihEnd, ihHandler, type);
        }
        method.setMaxStack();
        method.setMaxLocals();
        _cg.addMethod(method.getMethod());
        il.dispose();
    }
    private byte[] getBytes(Env env) {
        return _cg.getJavaClass().getBytes();
    }
}
