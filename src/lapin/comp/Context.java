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
package lapin.comp;
import lapin.function.Expr;
import lapin.function.Function;
import lapin.function.LambdaList;
import lapin.function.Subr;
import lapin.lang.Data;
import lapin.lang.Env;
import lapin.lang.Lists;
import lapin.lang.NotReachedException;
import lapin.lang.Package;
import lapin.lang.ProgramException;
import lapin.lang.Symbol;
import lapin.lang.Symbols;
import lapin.load.Loader;
import lapin.io.IO;
import lapin.io.Printer;
import lapin.util.Logger;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;

/** context object used in the compilation process. */
final class Context implements lapin.comp.ClassInfo {
    // uninterned symbol: for private use only
    static private final Symbol EXPR_BODY = Symbol.gensym("_EXPR-BODY_");
    static private final Symbol ENV_KEY = Symbol.gensym("_COMPILE-CONTEXT_");

    static Context get(Env env) {
        return (Context) env.get(ENV_KEY);
    }
    static void bind(Context ctx, Env env) {
        env.bind(ENV_KEY, ctx);
    }

    static class TypeInfo {
        /** form to be compiled with expected type. */
        final Object exp;
        /** type expected */
        final Class expected;
        /** flag which indicates whether the expected type
            can accepts the multiple values or not. */
        final boolean canConsumeValues;

        /** */
        boolean inProg = false;
        /** counter which is incremented
            when Compiler#compileTypeAdjustment is called */
        int count = 0;

        TypeInfo(Object exp, Class expected, boolean canConsumeValues) {
            this.exp = exp;
            this.expected = expected;
            this.canConsumeValues = canConsumeValues;
        }
    } /* end of TypeInfo */

    /** BlockInfo represents the block structure
        appearing in the expr body. */
    static class BlockInfo {
        static final int ROOT = 0;
        static final int LAMBDA = 1;
        static final int PROG = 2;
        static final int MV_BIND = 3;

        /** type of this block */
        final int type;
        /** tag which indicates the start point of the block */
        final Symbol startTag;
        /** tag which indicates the end point of the block */
        final Symbol endTag;
        /** loval var of a environment used by the block */
        final Object envVar;
        /** flag which indicates whether the env is switched or not. */
        final boolean envSwitched;
        /** declaration info applied to the block */
        final DeclInfo di;

        /** mapping which mapps a tag in (GO ...) to the unique tag */
        private final HashMap tagMap = new HashMap();
        /** alist of var info defined in the block */
        private Object vars = Symbols.NIL;

        BlockInfo(int type, Symbol startTag, Symbol endTag,
                  Object envVar, boolean envSwitched, DeclInfo di) {
            this.type = type;
            this.startTag = startTag;
            this.endTag = endTag;
            this.envVar = envVar;
            this.envSwitched = envSwitched;
            this.di = di;
        }
        Object pushVar(Symbol var, Object slot, Class type) {
            Object vi = Lists.list(var, slot, type);
            vars = Lists.cons(vi, vars);
            return vi;
        }
        Object popVar() {
            Object vi = Lists.car(vars);
            vars = Lists.cdr(vars);
            return vi;
        }
        Object getVar(Symbol var) {
            return Lists.assq(var, vars);
        }
        Object getVars() {
            return vars;
        }
    }/* end of BlockInfo */

    /** data structure of a method to be compiled. */
    static class MethodInfo implements lapin.comp.MethodInfo {
        /** owner of this object */
        private final Context ctx;
        /** number of arguments of the method
            (-1 means ANY lambdalist is acceptable) */
        private final int nargs;
        /** flag which specifies whether the method accepts
            rest arguments. When nargs == -1, this value is ignored. */
        private final boolean rest;
        /** flag which specifies whether the method is
            an implementation of the Callable interface. */
        private final boolean implCallable;
        /** name of this method. */
        private final String name;
        /** parameter types of this method. */
        private final Class[] paramTypes;
        /** return type of this method. */
        private final Class retType;

        /** list of instructions (intermediate code) */
        private final ArrayList instList = new ArrayList();
        /** stack of block info which represents the body
            of this method. (prog ...) and ((lambda ...) ...)
            inside the method body are represented as nested
            block info. */
        private Object block = Symbols.NIL;
        /** slot count ("slot" is a number which is mapped
            to the index of local variable of the JVM)  */
        private int slot = 0;
        /** max slot count */
        private int maxSlot = 0;
        /** counter used to name a tag */
        private int tagId = 0;
        /** stack on which the type of local vars are pushed. */
        private Object typeOfSlot = Symbols.NIL;
        /** stack of type info */
        private Object type = Symbols.NIL;
        /** list of var info which represents
            the arguments of this method. */
        private Object argVars = Symbols.NIL;
        /** list of var info for the global vars. */
        private Object globalVars = Symbols.NIL;

        //new MethodInfo
        MethodInfo(Context ctx, int nargs, boolean rest,
                   boolean implCallable, Env env) {
            this.ctx = ctx;
            this.nargs = nargs;
            this.rest = rest;
            this.implCallable = implCallable;

            LambdaList ll = ctx.expr.lambdaList();
            DeclInfo di = ctx.di.child().pushDecls(ctx.expr.decls());

            // init name, paramTypes, and argVars
            this.argVars = Lists.cons
                (Lists.list(Callables.LOCAL_SLOT_THIS,
                            Callables.LOCAL_SLOT_THIS,
                            Function.class), argVars);
            if (nargs == -1) {
                this.name = implCallable ? "call" : "_call";
                this.paramTypes = new Class[2];
                this.paramTypes[0] = Object.class;
                this.paramTypes[1] = Env.class;
                this.argVars = Lists.cons
                    (Lists.list(Callables.LOCAL_SLOT_ARGS,
                                Callables.LOCAL_SLOT_ARGS,
                                this.paramTypes[0]), argVars);
                this.argVars = Lists.cons
                    (Lists.list(Callables.LOCAL_SLOT_ENV,
                                Callables.LOCAL_SLOT_ENV,
                                this.paramTypes[1]), argVars);
            }
            else if (rest) {
                this.name = implCallable ? "call"+nargs : "_call"+nargs;
                this.paramTypes = new Class[nargs+2];
                for (int i = 0; i < nargs; i++) {
                    this.paramTypes[i] = implCallable ? Object.class
                        : di.javaTypeOfVar(Data.symbol(ll.var(i)), true);
                    this.argVars = Lists.cons
                       (Lists.list(Callables.getArgSlot(i),
                                    Callables.getArgSlot(i),
                                    this.paramTypes[i]), argVars);
                }
                this.paramTypes[nargs] = Object.class;
                this.argVars = Lists.cons
                    (Lists.list(Callables.LOCAL_SLOT_ARGS,
                                Callables.LOCAL_SLOT_ARGS,
                                this.paramTypes[nargs]), argVars);
                this.paramTypes[nargs+1] = Env.class;
                this.argVars = Lists.cons
                    (Lists.list(Callables.LOCAL_SLOT_ENV,
                                Callables.LOCAL_SLOT_ENV,
                                this.paramTypes[nargs+1]), argVars);
            }
            else {
                this.name = implCallable ? "call"+nargs : "_call"+nargs;
                this.paramTypes = new Class[nargs+1];
                for (int i = 0; i < nargs; i++) {
                    this.paramTypes[i] = implCallable ? Object.class
                        : di.javaTypeOfVar(Data.symbol(ll.var(i)), true);
                    this.argVars = Lists.cons
                        (Lists.list(Callables.getArgSlot(i),
                                    Callables.getArgSlot(i),
                                    this.paramTypes[i]), argVars);
                }
                this.paramTypes[nargs] = Env.class;
                this.argVars = Lists.cons
                    (Lists.list(Callables.LOCAL_SLOT_ENV,
                                Callables.LOCAL_SLOT_ENV,
                                this.paramTypes[nargs]), argVars);
            }

            // init retType
            this.retType = implCallable ? Object.class
                : di.javaRetTypeOfFunc(true);

            // push root block
            Symbol startTag = null;
            Symbol endTag = null;
            Object envVar = Lists.car(this.argVars); // XXX
            boolean envSwitched = false;
            Object vars = ll.vars();
            BlockInfo bi = genBlock(BlockInfo.ROOT,
                                    startTag, endTag, envVar,
                                    envSwitched, di, vars, env);
            this.block = Lists.cons(bi, block);
        }
        public ClassInfo classInfo() {
            return ctx;
        }
        public int nargs() {
            return nargs;
        }
        public boolean rest() {
            return rest;
        }
        public boolean implCallable() {
            return implCallable;
        }
        public String name() {
            return name;
        }
        public Class[] paramTypes() {
            return paramTypes;
        }
        public Class retType() {
            return retType;
        }
        public int instLen() {
            return instList.size();
        }
        public Object getInst(int i) {
            return instList.get(i);
        }
        public Object setInst(int i, Object inst) {
            return instList.set(i, inst);
        }
        public void addInst(Object inst) {
            instList.add(inst);
        }
        public void addInst(int i, Object inst) {
            instList.add(i, inst);
        }
        public Object removeInst(int i) {
            return instList.remove(i);
        }
        void incSlot(Class type) {
            slot = slot + Classes.sizeOf(type);
            typeOfSlot = Lists.cons(type, typeOfSlot);
            if (maxSlot < slot)
                maxSlot = slot;
        }
        void decSlot() {
            Class type = Data.javaClass(Lists.car(typeOfSlot));
            typeOfSlot = Lists.cdr(typeOfSlot);
            slot = slot - Classes.sizeOf(type);
        }
        Object curSlot() {
            return Data.toFixnum(slot);
        }
        Object maxSlot() {
            return Data.toFixnum(maxSlot);
        }
        Object pushGlobalVar(Symbol var, Env env) {
            if (Logger.debuglevelp(env)) {
                Logger.debug("[pushGlobalVar] var=~S",
                             Lists.list(var), env);
            }
            BlockInfo bi = peekBlock();
            Class type = bi.di.javaTypeOfVar(var, true);
            Object slot = Symbols.NIL; /* var is special */
            if (!bi.di.isSpecial(var)) {
                Logger.warn("[pushGlobalVar] "+
                            "var ~S is not declared as SPECIAL",
                            Lists.list(var), env);
            }
            Object vi = Lists.list(var, slot, type);
            globalVars = Lists.cons(vi, globalVars);
            return vi;
        }
        Object pushTempVar(Class type) {
            Symbol var = genTag("TMP");
            BlockInfo bi = peekBlock();
            Object slot = curSlot(); incSlot(type);
            return bi.pushVar(var, slot, type);
        }
        Object popVar() {
            BlockInfo bi = peekBlock();
            Object vi = bi.popVar();
            if (Lists.cadr(vi) != Symbols.NIL) {
                decSlot();
            }
            return vi;
        }
        Symbol genTag(String id) {
            return Symbol.gensym(id+"-"+(tagId++));
        }
        BlockInfo genBlock(int blockType, Symbol startTag, Symbol endTag,
                           Object envVar, boolean envSwitched, DeclInfo di,
                           Object vars, Env env) {
            BlockInfo bi = new BlockInfo(blockType, startTag, endTag,
                                         envVar, envSwitched, di);

            if (Logger.tracelevelp(env)) {
                Logger.trace("[genBlock] block  =~S",
                             Lists.list(bi), env);
                Logger.trace("[genBlock] decl   ="+
                             "SPECIAL:~S VAR-TYPE:~S FUNC-TYPE:~S",
                             Lists.list(di.specialVars,
                                        di.varTypes,
                                        di.funcTypes), env);
                Logger.trace("[genBlock] vars=~S",
                             Lists.list(vars), env);
            }

            for (Object l = vars; !Lists.isEnd(l); l = Lists.cdr(l)) {
                Symbol var = Data.symbol(Lists.car(l));
                Class type = bi.di.javaTypeOfVar(var, true);
                Object slot;
                if (bi.di.isSpecial(var)) {
                    /* var is special */
                    slot = Symbols.NIL;
                }
                else {
                    /* var is local */
                    slot = curSlot(); incSlot(type);
                }
                bi.pushVar(var, slot, type);
            }

            return bi;
        }
        BlockInfo pushBlock(int type, Symbol startTag, Symbol endTag,
                            Object decls, Object vars, Env env) {
            // make child DeclInfo and push decls into the child.
            DeclInfo di = getDecl().child().pushDecls(decls);
            // speVarFound: true if special var found in vars
            boolean speVarFound
                = di.findSpecial(vars) != Symbols.NIL;
            // oldEnvVar
            Object oldEnvVar = getEnvVar();
            // newEnvVar
            Object newEnvVar;
            if (speVarFound) {
                // value of ctx.curSlot() is assigned to envVar
                newEnvVar = pushTempVar(Env.class);
            } else {
                // envVar unchanged
                newEnvVar = oldEnvVar;
            }
            // gen and push new block
            BlockInfo bi = genBlock(type, startTag, endTag,
                                    newEnvVar, speVarFound, di,
                                    vars, env);
            block = Lists.cons(bi, block);
            // emit start tag
            addInst(bi.startTag);
            if (speVarFound) {
                // switch env from oldEnvVar to newEnvVar
                // -> assigns new environment to this block
                addInst(Lists.list(Insts.ENV_CHILD, oldEnvVar, newEnvVar));
            }
            return bi;
        }
        BlockInfo peekBlock() {
            if (block == Symbols.NIL)
                throw new NotReachedException
                    ("block is empty", Symbols.NIL);
            else
                return (BlockInfo) Lists.car(block);
        }
        BlockInfo popBlock() {
            BlockInfo bi = peekBlock();
            // emit endTag
            addInst(bi.endTag);
            // pop local vars
            while (!Lists.isEnd(bi.getVars())) {
                popVar();
            }
            // pop env var
            if (bi.envSwitched) {
                popVar();
            }
            // pop bi
            block = Lists.cdr(block);
            return bi;
        }
        void addBlockTag(Symbol tag) {
            BlockInfo bi = peekBlock();
            if (bi.tagMap.containsKey(tag)) {
                throw new ProgramException
                    ("tag already exists: ~S.", Lists.list(tag));
            }
            // maps tag to unique genTag
            bi.tagMap.put(tag, genTag("IN-BLOCK-BODY-"+tag.pname()));
        }
        Symbol getBlockTag(Symbol tag) {
            // search blocks from inner to outer
            for (Object l = block; !Lists.isEnd(l); l = Lists.cdr(l)) {
                BlockInfo bi = (BlockInfo) Lists.car(l);
                if (bi.tagMap.containsKey(tag))
                    return Data.symbol(bi.tagMap.get(tag));
            }
            throw new ProgramException
                ("tag not exists: ~S.", Lists.list(tag));
        }
        Object getBlockTags() {
            HashSet tags = new HashSet();
            for (Object l = block; !Lists.isEnd(l); l = Lists.cdr(l)) {
                BlockInfo bi = (BlockInfo) Lists.car(l);
                tags.addAll(bi.tagMap.keySet());
            }
            return Lists.toList(tags.toArray());
        }
        Symbol getBlockStartTag() {
            BlockInfo bi = peekBlock();
            return bi.startTag;
        }
        Symbol getBlockEndTag() {
            BlockInfo bi = peekBlock();
            return bi.endTag;
        }
        Object getEnvVar() {
            BlockInfo bi = peekBlock();
            return bi.envVar;
        }
        DeclInfo getDecl() {
            BlockInfo bi = peekBlock();
            return bi.di;
        }
        Object getVars() {
            BlockInfo bi = peekBlock();
            return bi.getVars();
        }
        Object getVar(Symbol var, Env env) {
            Object vi;
            // returns a var info for the specified var.
            // search blocks from inner to outer
            for (Object l = block; !Lists.isEnd(l); l = Lists.cdr(l)) {
                BlockInfo bi = (BlockInfo) Lists.car(l);
                vi = bi.getVar(var);
                if (vi != Symbols.NIL)
                    return vi;
            }
            // argVar (arguments of this method)
            vi = Lists.assq(var, argVars);
            if (vi != Symbols.NIL)
                return vi;

            // globalVar
            vi = Lists.assq(var, globalVars);
            if (vi != Symbols.NIL)
                return vi;

            return pushGlobalVar(var, env);
        }
        TypeInfo pushType(Object exp, Class expected,
                          boolean canConsumeValues) {
            TypeInfo ti = new TypeInfo(exp, expected, canConsumeValues);
            type = Lists.cons(ti, type);
            return ti;
        }
        TypeInfo peekType() {
            if (type == Symbols.NIL)
                throw new NotReachedException
                    ("type is empty", Symbols.NIL);
            else
                return (TypeInfo) Lists.car(type);
        }
        TypeInfo popType() {
            TypeInfo ti = peekType();
            // pop ti
            type = Lists.cdr(type);
            return ti;
        }
        Symbol getProgBlockEndTag() {
            for (Object l = block; !Lists.isEnd(l); l = Lists.cdr(l)) {
                BlockInfo bi = (BlockInfo) Lists.car(l);
                if (bi.type == BlockInfo.PROG)
                    return bi.endTag;
            }
            throw new ProgramException
                ("no PROG block is visible.", Symbols.NIL);
        }
        TypeInfo peekProgReturnType() {
            for (Object l = type; !Lists.isEnd(l); l = Lists.cdr(l)) {
                TypeInfo ti = (TypeInfo) Lists.car(l);
                if (ti.inProg)
                    return ti;
            }
            throw new ProgramException
                ("no PROG block is visible.", Symbols.NIL);
        }
        void dumpInsts(Object stream, Env env) {
            // print nargs
            Printer.format("nargs: ~S~%",
                           Lists.list(Data.toFixnum(nargs())), stream, env);
            // print rest
            Printer.format("rest: ~S~%",
                           Lists.list(Data.toPredicate(rest())), stream, env);
            // print implCallable
            Printer.format("implCallable: ~S~%",
                           Lists.list(Data.toPredicate(implCallable())),
                           stream, env);
            // print name
            Printer.format("name: ~S~%",
                           Lists.list(name()), stream, env);
            // print maxslot
            Printer.format("maxslot: ~S~%",
                           Lists.list(maxSlot()), stream, env);
            // print insts
            Printer.format("insts: ~%", 
                           Symbols.NIL, stream, env);
            int i = 0;
            for (Iterator it = instList.iterator(); it.hasNext();) {
                Printer.format((i++)+":\t~S~%",
                               Lists.list(it.next()), stream, env);
            }
        }
        public String toString() {
            return "[method"+
                ":nargs="+nargs+
                ",rest="+rest+
                ",implCallable="+implCallable+
                ",name="+name+
                ",context="+ctx+"]";
        }
    }/* end of MethodInfo */

    /** java package used to create class name */
    private final String javapkg;
    /** id assigned to the expr,
        which is used to create class name */
    private final String funId;
    /** name of the (compiled) expr */
    private final Symbol name;
    /** expr to be compiled */
    private final Expr expr;
    /** declaration info (declared outside the expr) */
    private final DeclInfo di;
    /** owner of this context */
    private final ContextGroup cg;
    /** class name of the (compiled) expr */
    private final String classname;

    /** list of methodInfo */
    private Object methods = Symbols.NIL;
    /** list of methodInfo to be compiled */
    private Object methodsToBeCompiled = Symbols.NIL;
    /** bytecode emitted by ByteCodeGenerator */
    private byte[] byteCode = null;
    /** list of the context of the nested expr */
    private final ArrayList children = new ArrayList();
    /** */
    private final HashMap formMap = new HashMap();

    Context(String javapkg, String funId, Symbol name,
            Expr expr, DeclInfo di, ContextGroup cg) {
        this.javapkg = javapkg;
        this.funId = funId;
        this.name = name;
        this.expr = expr;
        this.di = di;
        this.cg = cg;
        this.classname = "lapin.function.subrs.gen."+javapkg+".fun$"+funId;
    }
    public Symbol name() {
        return name;
    }
    public Expr expr() {
        return expr;
    }
    public String classname() {
        return classname;
    }
    public Iterator methodIterator() {
        return Lists.asIterator(methods);
    }
    String javapkg() {
        return javapkg;
    }
    String funId() {
        return funId;
    }
    ContextGroup group() {
        return cg;
    }
    Object exprBody() {
        return formMap.get(EXPR_BODY);
    }
    Object initform(Object var) {
        return formMap.get(var);
    }
    byte[] byteCode() {
        return byteCode;
    }
    Context child(Expr expr, Env env) {
        int count = this.children.size();
        String javapkg = this.javapkg;
        String funId = this.funId+"$"+count;
        Symbol name = Data.symbol
            (env.lisp().getObarray().intern
             (Package.get(env),
              this.name.pname()+"-"+count).nth(0));
        DeclInfo di = getDecl()
            // create child DeclInfo for the expr
            .child(name, expr.lambdaList())
            // push default func type
            .pushFuncType(Lists.list(name), Symbols.T);
        ContextGroup cg = this.cg;
        Context child = new Context(javapkg, funId, name, expr, di, cg);
        this.children.add(child);
        return child;
    }
    Context getLastChild() {
        int len = childLen();
        return getChild(len-1);
    }
    Context getChild(int i) {
        return (Context) children.get(i);
    }
    int childLen() {
        return children.size();
    }
    MethodInfo pushMethod(int nargs, boolean rest,
                          boolean implCallable, Env env) {
        MethodInfo mi = new MethodInfo
            (this, nargs, rest, implCallable, env);
        this.methods = Lists.cons(mi, this.methods);
        // enqueue mi into methodsToBeCompiled
        this.methodsToBeCompiled
            = Lists.nconc(this.methodsToBeCompiled, Lists.list(mi));
        return mi;
    }
    MethodInfo peekMethod() {
        if (methodsToBeCompiled == Symbols.NIL)
            throw new NotReachedException
                ("methodsToBeCompiled is empty", Symbols.NIL);
        return (MethodInfo) Lists.car(methodsToBeCompiled);
    }
    boolean hasMethod() {
        return methodsToBeCompiled != Symbols.NIL;
    }
    void nextMethod() {
        if (methodsToBeCompiled == Symbols.NIL)
            throw new NotReachedException
                ("methodsToBeCompiled is empty", Symbols.NIL);
        // dequeue mi from methodsToBeCompiled
        this.methodsToBeCompiled
            = Lists.cdr(this.methodsToBeCompiled);
    }
    int nargs() {
        MethodInfo mi = peekMethod();
        return mi.nargs();
    }
    boolean rest() {
        MethodInfo mi = peekMethod();
        return mi.rest();
    }
    boolean implCallable() {
        MethodInfo mi = peekMethod();
        return mi.implCallable();
    }
    int instLen() {
        MethodInfo mi = peekMethod();
        return mi.instLen();
    }
    Object getInst(int i) {
        MethodInfo mi = peekMethod();
        return mi.getInst(i);
    }
    void addInst(Object inst) {
        MethodInfo mi = peekMethod();
        mi.addInst(inst);
    }
    void addInst(int i, Object inst) {
        MethodInfo mi = peekMethod();
        mi.addInst(i, inst);
    }
    Object setInst(int i, Object inst) {
        MethodInfo mi = peekMethod();
        return mi.setInst(i, inst);
    }
    Object removeInst(int i) {
        MethodInfo mi = peekMethod();
        return mi.removeInst(i);
    }
    void incSlot(Class type) {
        MethodInfo mi = peekMethod();
        mi.incSlot(type);
    }
    void decSlot() {
        MethodInfo mi = peekMethod();
        mi.decSlot();
    }
    Object curSlot() {
        MethodInfo mi = peekMethod();
        return mi.curSlot();
    }
    Object maxSlot() {
        MethodInfo mi = peekMethod();
        return mi.maxSlot();
    }
    Object pushGlobalVar(Symbol var, Env env) {
        MethodInfo mi = peekMethod();
        return mi.pushGlobalVar(var, env);
    }
    Object pushTempVar(Class type) {
        MethodInfo mi = peekMethod();
        return mi.pushTempVar(type);
    }
    Object popVar() {
        MethodInfo mi = peekMethod();
        return mi.popVar();
    }
    Symbol genTag(String id) {
        MethodInfo mi = peekMethod();
        return mi.genTag(id);
    }
    BlockInfo pushBlock(int type, Symbol startTag, Symbol endTag,
                        Object decls, Object vars, Env env) {
        MethodInfo mi = peekMethod();
        return mi.pushBlock(type, startTag, endTag, decls, vars, env);
    }
    BlockInfo peekBlock() {
        MethodInfo mi = peekMethod();
        return mi.peekBlock();
    }
    BlockInfo popBlock() {
        MethodInfo mi = peekMethod();
        return mi.popBlock();
    }
    void addBlockTag(Symbol tag) {
        MethodInfo mi = peekMethod();
        mi.addBlockTag(tag);
    }
    Symbol getBlockTag(Symbol tag) {
        MethodInfo mi = peekMethod();
        return mi.getBlockTag(tag);
    }
    Object getBlockTags() {
        MethodInfo mi = peekMethod();
        return mi.getBlockTags();
    }
    Symbol getBlockStartTag() {
        MethodInfo mi = peekMethod();
        return mi.getBlockStartTag();
    }
    Symbol getBlockEndTag() {
        MethodInfo mi = peekMethod();
        return mi.getBlockEndTag();
    }
    Object getEnvVar() {
        MethodInfo mi = peekMethod();
        return mi.getEnvVar();
    }
    DeclInfo getDecl() {
        MethodInfo mi = peekMethod();
        return mi.getDecl();
    }
    Object getVars() {
        MethodInfo mi = peekMethod();
        return mi.getVars();
    }
    Object getVar(Symbol var, Env env) {
        MethodInfo mi = peekMethod();
        return mi.getVar(var, env);
    }
    TypeInfo pushType(Object exp, Class expected,
                      boolean canConsumeValues) {
        MethodInfo mi = peekMethod();
        return mi.pushType(exp, expected, canConsumeValues);
    }
    TypeInfo peekType() {
        MethodInfo mi = peekMethod();
        return mi.peekType();
    }
    TypeInfo popType() {
        MethodInfo mi = peekMethod();
        return mi.popType();
    }
    Symbol getProgBlockEndTag() {
        MethodInfo mi = peekMethod();
        return mi.getProgBlockEndTag();
    }
    TypeInfo peekProgReturnType() {
        MethodInfo mi = peekMethod();
        return mi.peekProgReturnType();
    }
    boolean isSpecial(Symbol var) {
        return getDecl().isSpecial(var);
    }
    Object findSpecial(Object vars) {
        return getDecl().findSpecial(vars);
    }
    Class javaTypeOfVar(Symbol var, boolean allowPrimitive) {
        return getDecl().javaTypeOfVar(var, allowPrimitive);
    }
    Class javaRetTypeOfFunc(boolean allowPrimitive) {
        return getDecl().javaRetTypeOfFunc(allowPrimitive);
    }
    Object javaParamTypesOfFunc(boolean allowPrimitive) {
        return getDecl().javaParamTypesOfFunc(allowPrimitive);
    }
    MethodInfo findMethod(int nargs, boolean implCallable, Env env) {
        LambdaList ll = expr.lambdaList();
        int min = ll.reqCount();
        int max = ll.reqCount()+ll.optCount();
        boolean rest = ll.isRest() || ll.keyCount() > 0;
        if (ll.isWhole() || ll.isNested() ||
            Callables.getMaxArgCount() < max) {
            if (Logger.debuglevelp(env))
                Logger.debug("[findMethod] ll=~S nargs=-1",
                             Lists.list(ll.params()), env);
            for (Object l = methods; !Lists.isEnd(l); l = Lists.cdr(l)) {
                MethodInfo mi = (MethodInfo) Lists.car(l);
                if (mi.nargs == -1 &&
                    mi.implCallable == implCallable)
                    return mi;
            }
        }
        else if (rest && max <= nargs) {
            if (Logger.debuglevelp(env))
                Logger.debug("[findMethod] ll=~S nargs="+nargs+" rest=true",
                             Lists.list(ll.params()), env);
            for (Object l = methods; !Lists.isEnd(l); l = Lists.cdr(l)) {
                MethodInfo mi = (MethodInfo) Lists.car(l);
                if (mi.nargs == max && mi.rest &&
                    mi.implCallable == implCallable)
                    return mi;
            }
        }
        else if (min <= nargs && nargs <= max) {
            if (Logger.debuglevelp(env))
                Logger.debug("[findMethod] ll=~S nargs="+nargs,
                             Lists.list(ll.params()), env);
            for (Object l = methods; !Lists.isEnd(l); l = Lists.cdr(l)) {
                MethodInfo mi = (MethodInfo) Lists.car(l);
                if (mi.nargs == nargs &&
                    mi.implCallable == implCallable)
                    return mi;
            }
        }
        throw new ProgramException
            ("wrong number of arguments: ~S: ll=~S, nargs="+nargs,
             Lists.list(classname, ll.params()));
    }
    void convertForms(Env env) {
        _convertInitforms(expr.lambdaList(), env);
        _convertExprBody(expr.body(), env);
    }
    private void _convertInitforms(LambdaList ll, Env env) {
        // opt
        for (int i = 0; i < ll.optCount(); i++) {
            Object var = ll.optVar(i);
            Object initform = ll.optInitform(i);
            Object converted = FormConverter.convertForm(initform, env);
            Object old = formMap.put(var, converted);
            if (old != null) {
                Logger.warn("[conv-form:opt:~S] entry duplicated."+
                            "form ~S overwrites previous form: ~S",
                            Lists.list(var, initform, old), env);
            }
            if (Logger.debuglevelp(env)) {
                Logger.debug("[conv-form:opt:~S] ~S => ~S",
                             Lists.list(var, initform, converted), env);
            }
            if (Data.isLambdaList(var)) {
                _convertInitforms(Data.lambdaList(var), env);
            }
        }
        // key
        for (int i = 0; i < ll.keyCount(); i++) {
            Object var = ll.keyVar(i);
            Object initform = ll.keyInitform(i);
            Object converted = FormConverter.convertForm(initform, env);
            Object old = formMap.put(var, converted);
            if (old != null) {
                Logger.warn("[conv-form:key:~S] entry duplicated."+
                            "form ~S overwrites previous form: ~S",
                            Lists.list(var, initform, old), env);
            }
            if (Logger.debuglevelp(env)) {
                Logger.debug("[conv-form:key:~S] ~S => ~S",
                             Lists.list(var, initform, converted), env);
            }
            if (Data.isLambdaList(var)) {
                _convertInitforms(Data.lambdaList(var), env);
            }
        }
        // aux
        for (int i = 0; i < ll.auxCount(); i++) {
            Object var = ll.auxVar(i);
            Object initform = ll.auxInitform(i);
            Object converted = FormConverter.convertForm(initform, env);
            Object old = formMap.put(var, converted);
            if (old != null) {
                Logger.warn("[conv-form:aux:~S] entry duplicated."+
                            "form ~S overwrites previous form: ~S",
                            Lists.list(var, initform, old), env);
            }
            if (Logger.debuglevelp(env)) {
                Logger.debug("[conv-form:aux:~S] ~S => ~S",
                             Lists.list(var, initform, converted), env);
            }
            if (Data.isLambdaList(var)) {
                _convertInitforms(Data.lambdaList(var), env);
            }
        }
    }
    private void _convertExprBody(Object body, Env env) {
        Object form = Forms.progn(body);
        Object converted = FormConverter.convertForm(form, env);
        Object old = formMap.put(EXPR_BODY, converted);
        if (old != null) {
            Logger.warn("[conv-form:expr-body] entry duplicated."+
                        "form ~S overwrites previous form: ~S",
                         Lists.list(form, old), env);
        }
        if (Logger.debuglevelp(env)) {
            Logger.debug("[conv-form:expr-body] ~S => ~S",
                         Lists.list(form, converted), env);
        }
    }
    void initMethod(Env env) {
        LambdaList ll = expr.lambdaList();
        int min = ll.reqCount();
        int max = ll.reqCount()+ll.optCount();
        boolean rest = ll.isRest() || ll.keyCount() > 0;
        if (ll.isWhole() || ll.isNested() ||
            Callables.getMaxArgCount() < max) {
            // push method: Callable#call
            if (Logger.debuglevelp(env))
                Logger.debug("[method] nargs=-1 implCallable=true", env);
            pushMethod(-1, true, true, env);
            if (Logger.debuglevelp(env))
                Logger.debug("[method] nargs=-1 implCallable=false", env);
            pushMethod(-1, true, false, env);
        }
        else {
            for (int i = min; i <= max; i++) {
                // push method: Callable[i]#call[i] or Callable[i]r#call[i]
                boolean r = rest && i == max;
                if (Logger.debuglevelp(env))
                    Logger.debug("[method] nargs="+i+", rest="+r+
                                 ", implCallable=true", env);
                pushMethod(i, r, true, env);
                if (Logger.debuglevelp(env))
                    Logger.debug("[method] nargs="+i+", rest="+r+
                                 ", implCallable=false", env);
                pushMethod(i, r, false, env);
            }
        }
    }
    void dumpInsts(Object stream, Env env) {
        // print name
        Printer.format("name: ~A~%",
                       Lists.list(name), stream, env);
        // print classname
        Printer.format("class: ~A~%",
                       Lists.list(classname), stream, env);
        // print args
        Printer.format("args: ~S~%",
                       Lists.list(expr.lambdaList().params()), stream, env);
        // dump methodInfo
        for (Iterator it = methodIterator(); it.hasNext();) {
            MethodInfo mi = (MethodInfo) it.next();
            mi.dumpInsts(stream, env);
        }
        // dump children
        for (Iterator it = children.iterator(); it.hasNext();) {
            Context child = (Context) it.next();
            child.dumpInsts(stream, env);
        }
    }
    void generateByteCode(Env env) {
        // bcg throws ByteCodeGenerator.Exception, which is
        // a subclass of Compiler.Exception.
        ByteCodeGenerator bcg = ByteCodeGenerator.getInstance(this, env);
        this.byteCode = bcg.generate(env);
        // process children
        for (Iterator it = children.iterator(); it.hasNext();) {
            Context child = (Context) it.next();
            child.generateByteCode(env);
        }
    }
    void registCompiledExpr(Env env) {
        if (byteCode == null)
            throw new NotReachedException
                ("byteCode is null", Symbols.NIL);
        try {
            Subr subr = Loader.toSubr(name, classname, byteCode, env);
            env.lisp().defun(name, Symbols.SUBR, subr);
        } catch (java.lang.Exception ex) {
            String msg = "failed to regist compiled expr.";
            throw new Compiler.Exception(msg, ex).push(name);
        } catch (java.lang.Error err) {
            // Error, such as VerifyError, might be caught here
            Logger.fatal("fatal error occurred!: ~S",
                         Lists.list(name), err, env);
            throw err;
        }
        // process children
        for (Iterator it = children.iterator(); it.hasNext();) {
            Context child = (Context) it.next();
            child.registCompiledExpr(env);
        }
    }
    void writeCompiledExprClass(Env env) {
        if (byteCode == null)
            throw new NotReachedException
                ("byteCode is null", Symbols.NIL);
        try {
            _writeCompiledExprClass(env);
        } catch (java.lang.Exception ex) {
            String msg = "error occurred " +
                "while writing byte code to class file.";
            throw new Compiler.Exception(msg, ex).push(name);
        }
        // process children
        for (Iterator it = children.iterator(); it.hasNext();) {
            Context child = (Context) it.next();
            child.writeCompiledExprClass(env);
        }
    }
    private void _writeCompiledExprClass(Env env) throws IOException {
        File dir = IO.dir(Symbols.COMPILE_CLASS_DIR, env);

        char fileSep = File.separatorChar;
        String classFile = classname.replace('.', fileSep)+".class";

        File outFile = new File(dir, classFile);
        if (Logger.debuglevelp(env))
            Logger.debug("[writeClass] outFile=~S",
                         Lists.list(outFile), env);

        OutputStream outputStream = null;
        try {
            outputStream = IO.openOutputStream(outFile);
            outputStream.write(byteCode);
            outputStream.flush();
        }
        finally {
            IO.close(outputStream);
        }
    }
    Object classInfoList(boolean needsClassinfo, Env env) {
        Object args = _classInfoList(Symbols.NIL, needsClassinfo, env);
        return Lists.nreverse(args);
    }
    private Object _classInfoList
        (Object args, boolean needsClassinfo, Env env) {
        Object bytes;
        if (needsClassinfo) {
            try {
                bytes = Loader.toString(byteCode);
            } catch (java.lang.Exception ex) {
                String msg = "error occurred " +
                    "while encoding byte code.";
                throw new Compiler.Exception(msg, ex).push(name);
            }
        } else {
            bytes = Symbols.NIL;
        }
        args = Lists.cons(Lists.list(name, classname, bytes), args);
        // process children
        for (Iterator it = children.iterator(); it.hasNext();) {
            Context child = (Context) it.next();
            args = child._classInfoList(args, needsClassinfo, env);
        }
        return args;
    }
    public String toString() {
        return "[context:classname="+classname+"]";
    }
}

