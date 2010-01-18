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
package lapin.eval;
import lapin.eval.Evaluator;
import lapin.function.Expr;
import lapin.io.Reader;
import lapin.lang.Data;
import lapin.lang.Env;
import lapin.lang.ProgramException;
import lapin.lang.Lists;
import lapin.lang.Obarray;
import lapin.lang.Package;
import lapin.lang.Symbol;
import lapin.lang.Symbols;
import lapin.lang.Values;

/**
 * Functions used in the definition forms.
 */
public final class Def {
    /**
     * Executes DEFPROP.
     * The definition of the DEFPROP form is 
     * <code>(DEFPROP name val indicator)</code>.
     * @param args CDR of DEFPROP form
     * @param env
     * @return Symbol for which the property is defined
     */
    static public Object defprop(Object args, Env env) {
        int len = Lists.length(args);
        if (len != 3)
            throw new ProgramException
                ("args.length is not 3: ~S.", Lists.list(args));
        Symbol name = Data.symbol(Lists.car(args));
        Object val = Lists.cadr(args);
        Symbol indicator = Data.symbol(Lists.caddr(args));
        env.lisp().setProp(name, indicator, val);
        return name;
    }
    /**
     * Executes DEFUN.
     * The definition of the DEFUN form is 
     * <code>(DEFUN name [type] lambdaList . body)</code>.
     * When <code>type</code> is omitted,
     * <code>EXPR</code> is used as default value.
     * @param args CDR of DEFUN form
     * @param env
     * @return Symbol which represents the name of the function
     */
    static public Object defun(Object args, Env env) {
        Values mv = makeExpr(args, env);
        Symbol name = Data.symbol(mv.nth(0));
        Expr fun = Data.expr(mv.nth(1));
        Symbol type = Data.symbol(mv.nth(2));
        // regist function
        env.lisp().defun(name, type, fun);
        return name;
    }
    static private Values makeExpr(Object args, Env env) {
        Object arg;
        Symbol name;
        Symbol type;
        Expr fun;

        Object l = args;
        if (Lists.isEnd(l))
            throw new ProgramException
                ("cannot define a function: argument is empty.",
                 Symbols.NIL);

        // retrieve name
        arg = Lists.car(l);
        if (!Data.isSymbol(arg))
            throw new ProgramException
                ("name must be symbol: ~S.", Lists.list(arg));
        name = Data.symbol(arg);
        l = Lists.cdr(l);
        if (Lists.isEnd(l))
            throw new ProgramException
                ("function is missing a lambda list: ~S.",
                 Lists.list(args));

        // retrieve type
        arg = Lists.car(l);
        if (arg == Symbols.EXPR ||
            arg == Symbols.FEXPR ||
            arg == Symbols.MACRO) {
            type = Data.symbol(arg);
            l = Lists.cdr(l);
            if (Lists.isEnd(l))
                throw new ProgramException
                    ("function is missing a lambda list: ~S.",
                     Lists.list(args));
        } else {
            type = Symbols.EXPR;
        }

        arg = Lists.car(l);

        if (type == Symbols.FEXPR) {
            // retrieve vars (fexpr must be 1 argument)
            if (!Data.isList(arg))
                throw new ProgramException
                    ("lambdaList must be list: ~S.", Lists.list(arg));
            if (Lists.length(arg) != 1)
                throw new ProgramException
                    ("length of variables must be 1: ~S.",
                     Lists.list(arg));
            if (!Data.isSymbol(Lists.car(arg)))
                throw new ProgramException
                    ("variable must be symbol: ~S.", Lists.list(arg));
            Object vars = arg;
            // retrieve body
            Object body = Lists.cdr(l);
            // create fexpr
            fun = new Expr("FEXPR", vars, body, env);
        }
        else if (type == Symbols.MACRO) {
            // retrieve var (macro must be 1 argument)
            if (!Data.isList(arg))
                throw new ProgramException
                    ("lambdaList must be list: ~S.", Lists.list(arg));
            if (Lists.length(arg) != 1)
                throw new ProgramException
                    ("length of variables must be 1: ~S.",
                     Lists.list(arg));
            if (!Data.isSymbol(Lists.car(arg)))
                throw new ProgramException
                    ("variable must be symbol: ~S.", Lists.list(arg));
            Object vars = Lists.list(Symbols.LK_WHOLE,
                                     Data.symbol(Lists.car(arg)));
            // retrieve body
            Object body = Lists.cdr(l);
            // create macro
            fun = new Expr("MACRO", vars, body, env);
        }
        else {
            // retrieve vars (lambdaList)
            if (!Data.isList(arg) && !Data.isSymbol(arg))
                throw new ProgramException
                    ("lambdaList must be list or symbol: ~S.",
                     Lists.list(arg));
            Object vars = arg;
            // retrieve body
            Object body = Lists.cdr(l);
            // create expr
            fun = new Expr("EXPR", vars, body, env);
        }
        // return (values name fun type)
        return Values.currentValues().push(name).push(fun).push(type);
    }
    /**
     * Executes DEFMACRO.
     * The definition of the DEFMACRO form is 
     * <code>(DEFMACRO name lambdaList . body)</code>.
     * @param args CDR of DEFMACRO form
     * @param env
     * @return Symbol which represents the name of the macro
     */
    static public Object defmacro(Object args, Env env) {
        return defmacro(args, Symbols.MACRO, env);
    }
    /**
     * Executes DEFINE-COMPILER-MACRO.
     * The definition of the DEFINE-COMPILER-MACRO form is 
     * <code>(DEFINE-COMPILER-MACRO name lambdaList . body)</code>.
     * @param args CDR of DEFINE-COMPILER-MACRO form
     * @param env
     * @return Symbol which represents the name of the macro
     */
    static public Object defcmacro(Object args, Env env) {
        return defmacro(args, Symbols.CMACRO, env);
    }
    static private Object defmacro(Object args, Symbol type, Env env) {
        Values mv = makeMacro(args, env);
        Symbol name = Data.symbol(mv.nth(0));
        Expr fun = Data.expr(mv.nth(1));
        // regist macro
        env.lisp().defun(name, type, fun);
        return name;
    }
    static private Values makeMacro(Object args, Env env) {
        Object l = args;
        if (Lists.isEnd(l))
            throw new ProgramException
                ("cannot define a macro: argument is empty.",
                 Symbols.NIL);
        Object arg;
        Symbol name;
        Object vars;
        Object body;
        Expr fun;
        // retrieve name
        arg = Lists.car(l);
        if (!Data.isSymbol(arg))
            throw new ProgramException
                ("name must be symbol: ~S.", Lists.list(arg));
        name = Data.symbol(arg);
        l = Lists.cdr(l);
        if (Lists.isEnd(l))
            throw new ProgramException
                ("macro is missing a lambda list: ~S.",
                 Lists.list(args));
        // retrieve vars (lambdaList)
        arg = Lists.car(l);
        if (!Data.isList(arg) && !Data.isSymbol(arg))
            throw new ProgramException
                ("lambdaList must be list or symbol: ~S.",
                 Lists.list(arg));
        // (MACRO-NAME is a dummy variable, which is bound to the macro name)
        if (Data.isList(arg) && Lists.car(arg) == Symbols.LK_WHOLE) {
            if (Lists.length(arg) == 1) {
                throw new ProgramException
                    ("lambdaList is illegal: ~S.", Lists.list(arg));
            }
            vars = Lists.list2(Symbols.LK_WHOLE,
                               Lists.cadr(arg),
                               Symbol.gensym("MACRO-NAME"),
                               Lists.cddr(arg));
        }
        else {
            vars = Lists.cons(Symbol.gensym("MACRO-NAME"), arg);
        }
        // retrieve body
        body = Lists.cdr(l);
        // create macro
        fun = new Expr("MACRO", vars, body, env);
        // returns (values name fun)
        return Values.currentValues().push(name).push(fun);
    }
    /**
     * Executes DEFVAR.
     * The definition of the DEFVAR form is 
     * <code>(DEFVAR sym [initform])</code>.
     * @param args CDR of DEFVAR form
     * @param env
     * @return Symbol which represents the variable
     */
    static public Object defvar(Object args, Env env) {
        int len = Lists.length(args);
        Symbol sym = Symbols.NIL;
        Object initform = Symbols.NIL;
        switch (len) {
        case 1:
            sym = Data.symbol(Lists.car(args));
            env.lisp().defvar(sym);
            break;
        case 2:
            sym = Data.symbol(Lists.car(args));
            initform = Lists.cadr(args);
            if (env.lisp().getEnv().isBound(sym))
                env.lisp().defvar(sym);
            else
                env.lisp().defvar(sym, Evaluator.eval(initform, env));
            break;
        default:
            throw new ProgramException
                ("args.length is neither 1 nor 2: ~S.",
                 Lists.list(args));
        }
        return sym;
    }
    /**
     * Executes DEFCONST.
     * The definition of the DEFCONST form is 
     * <code>(DEFCONST sym initform)</code>.
     * @param args CDR of DEFCONST form.
     * @param env
     * @return Package being defined
     */
    static public Object defconst(Object args, Env env) {
        int len = Lists.length(args);
        Symbol sym = Symbols.NIL;
        Object initform = Symbols.NIL;
        switch (len) {
//        case 1:
//            sym = Data.symbol(Lists.car(args));
//            env.lisp().defconst(sym);
//            break;
        case 2:
            sym = Data.symbol(Lists.car(args));
            initform = Lists.cadr(args);
            env.lisp().defconst(sym, Evaluator.eval(initform, env));
            break;
        default:
            throw new ProgramException
                ("args.length is neither 1 nor 2: ~S.",
                 Lists.list(args));
        }
        return sym;
    }
    /**
     * Executes DEFPACKAGE.
     * The definition of the DEFPACKAGE form is 
     * <code>(DEFPACKAGE pkgname {option}*)</code>.
     * @param args CDR of DEFPACKAGE form.
     * @param env
     * @return Symbol which represents the variable
     */
    static public Object defpackage(Object args, Env env) {
        if (Lists.isEnd(args))
            throw new ProgramException
                ("cannot define a package: argument is empty.",
                 Symbols.NIL);

        // obarray
        Obarray oa = env.lisp().getObarray();

        // retrieve pkgname
        Object pkgname = Lists.car(args);

        // retrieve options
        Object use = Symbols.NIL;
        Object imp = Symbols.NIL;
        Object exp = Symbols.NIL;
        Object intern = Symbols.NIL;
        for (Object l = Lists.cdr(args);
             !Lists.isEnd(l); l = Lists.cdr(l)) {
            Object opt = Lists.car(l);
            Symbol key = Data.symbol(Lists.car(opt));
            if (key == Symbols.KW_USE) {
                // :use
                for (Object m = Lists.cdr(opt);
                     !Lists.isEnd(m); m = Lists.cdr(m)) {
                    use = Lists.cons(Data.string(Lists.car(m)), use);
                }
            }
            else if (key == Symbols.KW_IMPORT_FROM) {
                // :import-from
                String impPkgname = Data.string(Lists.cadr(opt));
                for (Object m = Lists.cddr(opt);
                     !Lists.isEnd(m); m = Lists.cdr(m)) {
                    String impPname = Data.string(Lists.car(m));
                    Symbol sym = Data.symbol
                        (Reader.readFromString
                         (impPkgname+":"+impPname, // external symbol
                          Symbols.T, Symbols.NIL, Symbols.NIL, env));
                    imp = Lists.cons(sym, imp);
                }
            }
            else if (key == Symbols.KW_INTERN) {
                // :intern
                for (Object m = Lists.cdr(opt);
                     !Lists.isEnd(m); m = Lists.cdr(m)) {
                    intern = Lists.cons(Data.string(Lists.car(m)), intern);
                }
            }
            else if (key == Symbols.KW_EXPORT) {
                // :export
                for (Object m = Lists.cdr(opt);
                     !Lists.isEnd(m); m = Lists.cdr(m)) {
                    exp = Lists.cons(Data.string(Lists.car(m)), exp);
                }
            }
            else {
                throw new ProgramException
                    ("unknown key specified: ~S.", Lists.list(opt));
            }
        }

        // reverse lists
        use = Lists.nreverse(use);
        imp = Lists.nreverse(imp);
        exp = Lists.nreverse(exp);
        intern = Lists.nreverse(intern);

        // make new package if not exists
        Package pkg = oa.findPkg(pkgname);
        if (pkg == null)
            pkg = oa.mkPkg(pkgname, Symbols.NIL);

        // use-package
        for (Object l = use; !Lists.isEnd(l); l = Lists.cdr(l)) {
            oa.use(pkg, Lists.car(l));
        }

        // import
        for (Object l = imp; !Lists.isEnd(l); l = Lists.cdr(l)) {
            oa.imp(pkg, Data.symbol(Lists.car(l)));
        }

        // intern
        for (Object l = intern; !Lists.isEnd(l); l = Lists.cdr(l)) {
            oa.intern(pkg, Data.string(Lists.car(l)));
        }

        // export
        for (Object l = exp; !Lists.isEnd(l); l = Lists.cdr(l)) {
            Values mv = oa.intern(pkg, Data.string(Lists.car(l)));
            Object v0 = mv.nth(0);
            Symbol sym = Data.symbol(v0);
            oa.exp(pkg, sym);
        }

        return pkg;
    }
    private Def() {}
}
