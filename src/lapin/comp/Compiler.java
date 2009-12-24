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
import lapin.eval.Evaluator;
import lapin.eval.Macro;
import lapin.function.Expr;
import lapin.io.IO;
import lapin.io.Reader;
import lapin.io.Printer;
import lapin.lang.Data;
import lapin.lang.Env;
import lapin.lang.Lists;
import lapin.lang.Package;
import lapin.lang.SimpleException;
import lapin.lang.Symbol;
import lapin.lang.Symbols;
import lapin.lang.SysSymbols;
import lapin.lang.Values;
import lapin.load.Loader;
import lapin.util.Logger;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import lapin.util.TracableException;
import java.util.ArrayList;
import java.util.Date;
import java.util.StringTokenizer;

/**
 * Compiler functions.
 */
public final class Compiler {
    /** Exception thrown by the compiler. */
    static public class Exception extends TracableException {
        Exception(String msg, Throwable cause) {
            super(msg, cause);
        }
    }

    /*
     * entry point for each pass
     */

    static void doPass1(Env env) {
        Context ctx = Context.get(env);
        // expand macro
        ctx.convertForms(env);
        // analyze lambdalist and make methodinfo
        ctx.initMethod(env);
    }
    static void doPass2(Env env) {
        Context ctx = Context.get(env);
        // build intermediate code
        while (ctx.hasMethod()) {
            if (ctx.implCallable()) {
                InstsBuilder.compileMethod0(env);
            }
            else {
                if (ctx.nargs() < 0)
                    InstsBuilder.compileMethod1(env);
                else /* 0 <= mi.nargs() */
                    InstsBuilder.compileMethod2(env);
            }
            InstsBuilder.optimize(env);
            ctx.nextMethod();
        }
    }
    static void doPass3(Env env) {
        Context ctx = Context.get(env);
        // Dumps an intermediate code.
        if (ctx.group().dumpInsts) {
            ctx.dumpInsts(Symbols.NIL, env);
        }
        // Compiles the intermediate code and
        // create a CompiledExpr.
        if (ctx.group().genByteCode) {
            ctx.generateByteCode(env);
        }
        // Writes byte code into class file.
        // This method is called from COMPILE-FILE function
        // with :generate-class-file option.
        if (ctx.group().genClassFile) {
            ctx.writeCompiledExprClass(env);
        }
        // Loads the CompiledExpr class and
        // creates a new instance of the subr class.
        // This method is called from COMPILE function.
        if (ctx.group().registCompiledExpr) {
            // bind Loader.MyClassLoader
            Loader.bindInternalClassLoader(env);
            ctx.registCompiledExpr(env);
        }
    }

    /*
     * utilities for compiler
     */

    /**
     * Macroexpands the specified form.
     * @param form form to be expanded
     * @param env
     * @return 2 values; expanded form and flag
     */
    static public Values macroexpandForm(Object form, Env env) {
        return FormConverter.expandForm(form, env);
    }
    /**
     * Converts the specified form.
     * The converted form can be passed to
     * {@link #compileForm(Object,Env) compileForm} method.
     * @param form Form to be converted.
     * @param env
     * @return Converted form
     */
    static public Object convertForm(Object form, Env env) {
        return FormConverter.convertForm(form, env);
    }
    /**
     * Compiles the specified form and build intermediate code
     * for the form.
     * This method works only in the compilation context.
     * @param form Form to be compiled
     * @param env
     * @return T
     */
    static public Object compileForm(Object form, Env env) {
        InstsBuilder.compileForm(form, env);
        return Symbols.T;
    }
    /**
     * Compiles a function call for the specified symbol and arguments.
     * This method works only in the compilation context.
     * @param sym Symbol which represents the name of a function
     * @param args List of forms which represent the arguments
     *        passed to the function specified by <code>sym</code>
     * @param env
     * @return T
     */
    static public Object compileCall(Symbol sym, Object args, Env env) {
        InstsBuilder.compileCall(sym, args, env);
        return Symbols.T;
    }
    /**
     * Calculates the type of arguments.
     * This method works only in the compilation context.
     * @param args List of forms
     * @return List of class objects
     */
    static public Object calcArgTypes(Object args, Env env) {
        return TypeCalculator.typeOfArgs(args, env);
    }
    /**
     * Returns true if the specified <code>obj</code> is the quoted form.
     */
    static public boolean isConstant(Object obj, Env env) {
        return Forms.isConstant(obj);
    }
    /**
     * Returns the data part (CADR) of a quoted form.
     * @param obj Quoted form
     * @return Data part of the quoted form
     */
    static public Object constantValue(Object obj, Env env) {
        if (!Forms.isConstant(obj))
            throw new SimpleException
                ("not a constant form: ~S", Lists.list(obj));
        return Lists.cadr(obj);
    }

    /*
     * on-the-fly-compiler
     */

    /**
     * Compiles the specified expr and loads a resulting
     * {@link lapin.function.CompiledExpr}.
     * This method is called from the on-the-fly function-compiler.
     * @param name Function name
     * @param expr Expr to be compiled
     * @param dryrun Flag; if any object other than NIL is specified,
     *        then the {@link lapin.function.CompiledExpr}
     *        is NOT generated.
     * @param print Flag; if any object other than NIL is specified,
     *        then the intermediate code is printed.
     * @param optimize Flag; if any object other than NIL is specified,
     *        then the optimization is performed.
     * @param env
     * @return 2 values; function name and class name of
     *         {@link lapin.function.CompiledExpr}.
     * @throws Compiler.Exception
     */
    static public Values compile(Symbol name, Expr expr,
                                 Object dryrun, Object print,
                                 Object optimize, Env env) {
        Env newenv = env.child();
        ContextGroup cg = new ContextGroup();
        cg.javapkg = "DEFAULT";
        cg.funId = incFunId(env);
        cg.dumpInsts =Data.toBoolean(print);
        cg.genByteCode = Data.isNot(dryrun);
        cg.genClassFile = false;
        cg.registCompiledExpr = Data.isNot(dryrun);
        cg.optimize = Data.toBoolean(optimize);
        ContextGroup.bind(cg, newenv);
        // create context and compile compile the expr
        Context ctx = cg.genContext(name, expr, env);
        Context.bind(ctx, newenv);
        Compiler.doPass1(newenv);
        Compiler.doPass2(newenv);
        Compiler.doPass3(newenv);
        // return the name and the classname assigned to the expr
        Object v0 = ctx.name();
        Object v1 = ctx.classname();
        return Values.currentValues().push(v0).push(v1);
    }
    static private int incFunId(Env env) {
        int funId = Data.toInt(env.get(SysSymbols.FUN_ID));
        env.set(SysSymbols.FUN_ID, Data.toFixnum(funId+1));
        return funId;
    }

    /*
     * file-compiler
     */

    /**
     * Compiles a LISP file specified by <code>in</code>
     * and generates a FASL file spcified by <code>out</code>.
     * Note that <code>in</code> cannot contain any letter that is not
     * allowed to be included in the token of the java programming language
     * (for example, '-'), since <code>in</code> is used to create a
     * package (not for lisp, but for java) of the class that represents
     * the compiled function being a subclass of {@link lapin.function.CompiledExpr}.
     * @param in Name of LISP file.
     *        If a file extension is not ".lisp", then the extension
     *        is appended to the end of <code>in</code>.
     * @param out Name of FASL file.
     *        If null is specified, then the name of FASL file is
     *        generated from <code>in</code>, by replacing a file
     *        extension ".lisp" with ".fasl".
     * @param inFileEnc Character encoding for the LISP file.
     *        If NIL is specified, then the platform's default
     *        character encoding is used.
     * @param outFileEnc Character encoding for the FASL file.
     *        If NIL is specified, then the platform's default
     *        character encoding is used.
     * @param genClassFile Flag for the class file generation.
     *        If NIL is specified, then the byte code is emitted into
     *        the FASL file. If any object other than NIL is specified,
     *        then the class file is generated.
     * @param print Flag; if any object other than NIL is specified,
     *        then the intermediate code is printed.
     * @param optimize Flag; if any object other than NIL is specified,
     *        then the optimization is performed.
     * @param env
     * @return Name of FASL file
     * @throws Compiler.Exception
     * @throws lapin.io.FileException
     * @throws lapin.io.StreamException
     */
    static public Object compileFile(String in, String out,
                                     Object inFileEnc, Object outFileEnc,
                                     Object genClassFile, Object print,
                                     Object optimize, Env env) {
        /*
         * inFile
         */
        //if (!in.endsWith(".lisp"))
        //    throw new UnexpectedPathnameException("inFile", ".lisp", in);
        if (!in.endsWith(".lisp"))
            in = in+".lisp";

        File compileInputDir = IO.dir(Symbols.COMPILE_INPUT_DIR, env);

        File inFile = new File(compileInputDir, in);
        if (Logger.debuglevelp(env))
            Logger.debug("[compile] inFile=~S",
                         Lists.list(inFile), env);

        /*
         * outFile
         */
        if (out == null)
            out = trimExt(in)+".fasl";

        if (!out.endsWith(".fasl"))
            throw new UnexpectedPathnameException("outFile", ".fasl", out);

        File compileOutputDir = IO.dir(Symbols.COMPILE_OUTPUT_DIR, env);

        File outFile = new File(compileOutputDir, out);
        if (Logger.debuglevelp(env))
            Logger.debug("[compile] outFile=~S",
                         Lists.list(outFile), env);

        /*
         * javapkg
         */
        String javapkg = toJavapkg(in);

        _compileFile(inFile, outFile, inFileEnc, outFileEnc,
                     javapkg, genClassFile, print, optimize,
                     env);

        return out;
    }
    static private String toJavapkg(String in) {
        File file = new File(trimExt(in));
        if (file.isAbsolute() ||
            file.getPath().indexOf(".") != -1) {
            throw new UnexpectedPathnameException(file);
        }
        String javapkg
            = file.getPath().replace(File.separatorChar, '.');
        if (javapkg.indexOf("..") != -1) {
            throw new UnexpectedPathnameException(javapkg);
        }
        StringTokenizer st = new StringTokenizer(javapkg, ".");
        while (st.hasMoreTokens()) {
            String s = st.nextToken();
            if (!javaIdp(s)) {
                throw new UnexpectedPathnameException(javapkg);
            }
        }
        return javapkg;
    }
    static private String trimExt(String path) {
        int pos = path.lastIndexOf(".");
        if (pos == -1)
            return path;
        return path.substring(0, pos);
    }
    static private boolean javaIdp(String id) {
        // check length
        if (id.length() <= 0)
            return false;
        // check first char
        if (!Character.isJavaIdentifierStart(id.charAt(0)))
            return false;
        // check rest chars
        for (int i = 1; i < id.length(); i++) {
            if (!Character.isJavaIdentifierPart(id.charAt(i)))
                return false;
        }
        return true;
    }
    static private Object _compileFile(File inFile, File outFile, 
                                       Object inFileEnc, Object outFileEnc, 
                                       String javapkg, Object genClassFile,
                                       Object print, Object optimize,
                                       Env env) {
        InputStream inputStream = null;
        OutputStream outputStream = null;
        try {
            inputStream = IO.openInputStream(inFile);
            outputStream = IO.openOutputStream(outFile);
            java.io.Reader r;
            java.io.Writer w;
            r = IO.toReader(inputStream, inFileEnc);
            r = IO.wrapReader(r, Symbols.T);
            w = IO.toWriter(outputStream, outFileEnc);
            w = IO.wrapWriter(w, Symbols.T);
            Object bcClassName = env.get
                (SysSymbols.BYTE_CODE_GENERATOR_CLASSNAME, Symbols.NIL);
            Printer.format(";; This file is generated by lapin. ~%"+
                           ";; src file: ~A~%"+
                           ";; gen date: ~A~%"+
                           ";; bytecode generator: ~A~%",
                           Lists.list(inFile, new Date(), bcClassName),
                           w, env);
            Package currpkg = Package.get(env);
            {
                Env newenv = env.child();
                // preserve current package
                newenv.bind(Symbols.PACKAGE, currpkg);
                ContextGroup cg = new ContextGroup();
                cg.javapkg = javapkg;
                cg.funId = 0;
                cg.dumpInsts = Data.toBoolean(print);
                cg.genByteCode = true;
                cg.genClassFile = Data.toBoolean(genClassFile);
                cg.registCompiledExpr = false;
                cg.optimize = Data.toBoolean(optimize);
                ContextGroup.bind(cg, newenv);
                ArrayList forms = new ArrayList();
                Object exp;
                // init: pass1
                while (true) {
                    exp = Reader.read(r, Symbols.NIL, IO.EOF,
                                      Symbols.NIL, newenv);
                    if (exp == IO.EOF) {
                        break;
                    }
                    initTopLevel(exp, forms, newenv);
                }
                // compile: pass2 and pass3
                for (int i = 0; i < forms.size(); i++) {
                    exp = forms.get(i);
                    compileAndWriteTopLevel(exp, w, newenv);
                    IO.flush(w);
                }
                return Symbols.T;
            }
        }
        finally {
            IO.close(inputStream);
            IO.close(outputStream);
        }
    }
    static private void initTopLevel
        (Object exp, ArrayList forms, Env env) {
        if (macroDefinitionp(exp, env) ||
            functionDefinitionp(exp, Symbols.MACRO, env)) {
            if (Logger.debuglevelp(env))
                Logger.debug("[pass1] defmacro: ~S",
                             Lists.list(exp), env);
            /*
             * macro definition -> eval & copy
             */
            Evaluator.eval(exp, env);
            forms.add(exp);
        }
        else if (functionDefinitionp(exp, Symbols.EXPR, env)) {
            if (Logger.debuglevelp(env))
                Logger.debug("[pass1] defun (EXPR): ~S",
                             Lists.list(exp), env);
            /*
             * function definition (EXPR) -> call Compiler#doPass1
             */
            ContextGroup cg = ContextGroup.get(env);
            Symbol name = Data.symbol(Evaluator.eval(exp, env));
            Expr expr = Data.expr(env.lisp().getProp(name, Symbols.EXPR));
            Context ctx = cg.genContext(name, expr, env);
            {
                Env newenv = env.child();
                Context.bind(ctx, newenv);
                Compiler.doPass1(newenv);
            }
            forms.add(ctx);
        }
        else if (declareFormp(exp, env)) {
            if (Logger.debuglevelp(env))
                Logger.debug("[pass1] declare form: ~S",
                             Lists.list(exp), env);
            /*
             * declare-form: (DECLARE (SPECIAL v0 v1 ...))
             * -> push v0, v1, ... to the list of special vars
             * declare-form: (DECLARE (FIXNUM v0 v1 ...))
             * -> push v0, v1, ... to the list of fixnum vars
             * declare-form: (DECLARE (FLONUM v0 v1 ...))
             * -> push v0, v1, ... to the list of flonum vars
             * declare-from (other) -> eval
             */
            ContextGroup cg = ContextGroup.get(env);
            Object body = Lists.cdr(exp);
            for (Object l = body; !Lists.isEnd(l); l = Lists.cdr(l)) {
                Object form = Lists.car(l);
                if (declVarTypeFormp(Symbols.SPECIAL, form, env))
                    cg.pushVars(Symbols.SPECIAL, Lists.cdr(form));
                else if (declVarTypeFormp(Symbols.FIXNUM, form, env))
                    cg.pushVars(Symbols.FIXNUM, Lists.cdr(form));
                else if (declVarTypeFormp(Symbols.FLONUM, form, env))
                    cg.pushVars(Symbols.FLONUM, Lists.cdr(form));
                else
                    Evaluator.eval(form, env);
            }
        }
        else if (macroFormp(exp, env)) {
            if (Logger.debuglevelp(env))
                Logger.debug("[pass1] macro form: ~S",
                             Lists.list(exp), env);
            /*
             * macro-form -> expand and init
             */
            Object expanded = Values.singleValue(Macro.expand(exp, env));
            initTopLevel(expanded, forms, env);
        }
        else if (prognFormp(exp, env)) {
            if (Logger.debuglevelp(env))
                Logger.debug("[pass1] progn form: ~S",
                             Lists.list(exp), env);
            /*
             * progn form: (PROGN 'COMPILE ...)
             *  -> init each of the remaining elements
             */
            Object body = Lists.cddr(exp);
            for (Object l = body; !Lists.isEnd(l); l = Lists.cdr(l)) {
                initTopLevel(Lists.car(l), forms, env);
            }
        }
        else if (evalWhenFormp(exp, env)) {
            if (Logger.debuglevelp(env))
                Logger.debug("[pass1] eval-when form: ~S",
                             Lists.list(exp), env);
            /*
             * (EVAL-WHEN (COMPILE) ...) -> eval
             * (EVAL-WHEN (LOAD) ...) -> pass1
             */
            Object timeList = Lists.cadr(exp);
            Object body = Lists.cddr(exp);
            if (Lists.memq(Symbols.COMPILE, timeList) != Symbols.NIL) {
                for (Object l = body; !Lists.isEnd(l); l = Lists.cdr(l)) {
                    Evaluator.eval(Lists.car(l), env);
                }
            }
            if (Lists.memq(Symbols.LOAD, timeList) != Symbols.NIL) {
                for (Object l = body; !Lists.isEnd(l); l = Lists.cdr(l)) {
                    initTopLevel(Lists.car(l), forms, env);
                }
            }
        }
        else if (defSpecialFormp(exp, env)) {
            if (Logger.debuglevelp(env))
                Logger.debug("[pass1] defSpecial form: ~S",
                             Lists.list(exp), env);
            /*
             * (DEFVAR ...) or (DEFCONST ...) -> copy
             */
            forms.add(exp);

            ContextGroup cg = ContextGroup.get(env);
            Symbol var = Data.symbol(Lists.cadr(exp));
            cg.pushVars(Symbols.SPECIAL, Lists.list(var));
        }
        else {
            if (Logger.debuglevelp(env))
                Logger.debug("[pass1] random form: ~S",
                             Lists.list(exp), env);
            /*
             * random form -> copy
             */
            forms.add(exp);
        }
    }
    static private void compileAndWriteTopLevel
        (Object exp, java.io.Writer out, Env env) {
        if (exp instanceof Context) {
            if (Logger.debuglevelp(env))
                Logger.debug("[pass2] context: ~S",
                             Lists.list(exp), env);
            /*
             * compilation context -> compile and generate subr
             */
            Context ctx = (Context) exp;
            {
                Env newenv = env.child();
                Context.bind(ctx, newenv);
                Compiler.doPass2(newenv);
                Compiler.doPass3(newenv);
            }
            Object args = ctx.classInfoList(!ctx.group().genClassFile, env);
            Object form = Lists.list(SysSymbols.LOAD_SUBRS,
                                     Lists.list(Symbols.QUOTE, args));
            Printer.prin1(form, out, env);
            Printer.terpri(out, env);
        }
        else {
            /*
             * other obj -> print
             */
            if (Logger.debuglevelp(env))
                Logger.debug("[pass2] form: ~S",
                             Lists.list(exp), env);
            Printer.prin1(exp, out, env);
            Printer.terpri(out, env);
        }
    }
    static private boolean macroDefinitionp(Object exp, Env env) {
        if (Data.isAtom(exp))
            return false;
        Object car = Lists.car(exp);
        return car == Symbols.DEFMACRO;
    }
    static private boolean functionDefinitionp
        (Object exp, Symbol indicator, Env env) {
        if (Data.isAtom(exp))
            return false;
        Object car = Lists.car(exp);
        if (car != Symbols.DEFUN)
            return false;
        Object caddr = Lists.caddr(exp);
        if (Data.isSymbol(caddr) && caddr == indicator)
            return true;
        else if (Data.isList(caddr) && indicator == Symbols.EXPR)
            return true;
        else
            return false;
    }
    static private boolean declareFormp(Object exp, Env env) {
        if (Data.isAtom(exp))
            return false;
        Object car = Lists.car(exp);
        return car == Symbols.DECLARE;
    }
    static private boolean declVarTypeFormp(Symbol type, Object exp, Env env) {
        if (Data.isAtom(exp))
            return false;
        Object car = Lists.car(exp);
        return car == type;
    }
    static private boolean macroFormp(Object exp, Env env) {
        if (Data.isAtom(exp))
            return false;
        Object car = Lists.car(exp);
        if (!Data.isSymbol(car))
            return false;
        Symbol sym = Data.symbol(car);
        Object prop = env.lisp().getProp(sym, Symbols.MACRO);
        return Data.isExpr(prop);
    }
    static private boolean prognFormp(Object exp, Env env) {
        if (Data.isAtom(exp))
            return false;
        Object car = Lists.car(exp);
        Object cadr = Lists.cadr(exp);
        return car == Symbols.PROGN
            && Data.isList(cadr)
            && Lists.car(cadr) == Symbols.QUOTE
            && Lists.cadr(cadr) == Symbols.COMPILE;
    }
    static private boolean evalWhenFormp(Object exp, Env env) {
        if (Data.isAtom(exp))
            return false;
        Object car = Lists.car(exp);
        return car == Symbols.EVAL_WHEN;
    }
    static private boolean defSpecialFormp(Object exp, Env env) {
        if (Data.isAtom(exp))
            return false;
        Object car = Lists.car(exp);
        return car == Symbols.DEFVAR
            || car == Symbols.DEFCONST;
    }

    private Compiler() {}
}
