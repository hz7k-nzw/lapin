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
import lapin.eval.Funcall;
import lapin.eval.Macro;
import lapin.eval.NonLocalExit;
import lapin.function.Expr;
import lapin.function.Function;
import lapin.function.LambdaList;
import lapin.lang.Data;
import lapin.lang.Env;
import lapin.lang.List;
import lapin.lang.Lists;
import lapin.lang.NotReachedException;
import lapin.lang.Package;
import lapin.lang.Pair;
import lapin.lang.ProgramException;
import lapin.lang.Symbol;
import lapin.lang.Symbols;
import lapin.lang.SysSymbols;
import lapin.lang.Values;
import lapin.load.Loader;
import lapin.util.Logger;
import lapin.util.TracableException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Iterator;

final class InstsBuilder {

    static void compileMethod0(Env env) {
        Context ctx = Context.get(env);
        Context.MethodInfo mi = ctx.peekMethod();
        Context.MethodInfo mi2 = null;
        for (Iterator it = ctx.methodIterator();
             it.hasNext();) {
            mi2 = (Context.MethodInfo) it.next();
            if (mi2.nargs() == mi.nargs() &&
                mi2.rest() == mi.rest() &&
                mi2.implCallable() == false) {
                break;
            }
            mi2 = null;
        }
        if (mi2 == null) {
            // assertion error
            throw new NotReachedException
                ("method info not found: ~S.", Lists.list(mi));
        }
        compileFormWithExpectedType
            (Callables.LOCAL_SLOT_THIS, Function.class, false, env);
        if (mi.nargs() < 0) {
            /* invoke _call(args,env) */
            compileFormWithExpectedType(Callables.LOCAL_SLOT_ARGS,
                                        mi2.paramTypes()[0], false, env);
            compileFormWithExpectedType(Callables.LOCAL_SLOT_ENV,
                                        mi2.paramTypes()[1], false, env);
        }
        else if (mi.rest()) {
            /* invoke _call<N>(arg1,...arg<N>,rest,env) */
            int nargs = mi.nargs();
            for (int i = 0; i < nargs; i++) {
                compileFormWithExpectedType(Callables.getArgSlot(i),
                                            mi2.paramTypes()[i], false, env);
            }
            compileFormWithExpectedType(Callables.LOCAL_SLOT_ARGS,
                                        mi2.paramTypes()[nargs], false, env);
            compileFormWithExpectedType(Callables.LOCAL_SLOT_ENV,
                                        mi2.paramTypes()[nargs+1], false, env);
        }
        else {
            /* invoke _call<N>(arg1,...arg<N>,env) */
            int nargs = mi.nargs();
            for (int i = 0; i < nargs; i++) {
                compileFormWithExpectedType(Callables.getArgSlot(i),
                                            mi2.paramTypes()[i], false, env);
            }
            compileFormWithExpectedType(Callables.LOCAL_SLOT_ENV,
                                        mi2.paramTypes()[nargs], false, env);
        }
        ctx.addInst(Lists.list(Insts.CALL_DIRECT, mi2));
        compileTypeMapping
            (null, mi.retType(), mi2.retType(), true, true, env);
        ctx.addInst(Lists.list(Insts.RETURN, mi.retType()));
    }
    static void compileMethod1(Env env) {
        Context ctx = Context.get(env);
        Object envVar = ctx.getEnvVar();
        LambdaList ll = ctx.expr().lambdaList();
        Object argsVar = ctx.getVar(Callables.LOCAL_SLOT_ARGS, env);
        Class retType = ctx.peekMethod().retType();
        Object exprBody = ctx.exprBody();

        // speVarFound: true if specail var found in ll.vars()
        boolean speVarFound
            = ctx.findSpecial(ll.vars()) != Symbols.NIL;

        if (speVarFound) {
            // switch env to child
            // -> assign new environment to current block
            ctx.addInst(Lists.list(Insts.ENV_CHILD, envVar, envVar));
        }

        // bind args
        compileBind1(ll, argsVar, env);

        // compile body
        compileFormWithExpectedType(exprBody, retType, true, env);

        // emit return instruction
        ctx.addInst(Lists.list(Insts.RETURN, retType));
    }
    static void compileMethod2(Env env) {
        Context ctx = Context.get(env);
        Object envVar = ctx.getEnvVar();
        LambdaList ll = ctx.expr().lambdaList();
        int nargs = ctx.peekMethod().nargs();
        boolean rest = ctx.peekMethod().rest();
        Class retType = ctx.peekMethod().retType();
        Object exprBody = ctx.exprBody();

        Object argVarList = Symbols.NIL;
        for (int i = 0; i < nargs; i++) {
            Object argVar = ctx.getVar(Callables.getArgSlot(i), env);
            argVarList = Lists.cons(argVar, argVarList);
        }

        // reverse argVarList
        argVarList = Lists.nreverse(argVarList);

        Object restVar = rest
            ? ctx.getVar(Callables.LOCAL_SLOT_ARGS, env)
            : Symbols.NIL;

        // speVarFound: true if specail var found in ll.vars()
        boolean speVarFound
            = ctx.findSpecial(ll.vars()) != Symbols.NIL;

        if (speVarFound) {
            // switch env to child
            // -> assign new environment to current block
            ctx.addInst(Lists.list(Insts.ENV_CHILD, envVar, envVar));
        }

        // bind args
        compileBind2(ll, argVarList, restVar, env);

        // compile body
        compileFormWithExpectedType(exprBody, retType, true, env);

        // emit return instruction
        ctx.addInst(Lists.list(Insts.RETURN, retType));
    }
    /**
     * emit code to bind args, which is used to compile
     * Callable#call.
     */
    static private void compileBind1
        (LambdaList ll, Object argsVar, Env env) {
        /*
         * argsVar is local variable in which values passed
         * to the function are stored.
         * compileBind1 is used when the all parameters
         * are passed to the function in the form of list.
         */
        // whole
        compileBindWhole(ll, argsVar, env);
        // req and opt
        compileBindReqAndOpt1(ll, argsVar, env);
        // rest and key
        compileBindRestAndKey(ll, argsVar, env);
        // aux
        compileBindAux(ll, env);
    }
    /**
     * emit code to bind args, which is used to compile
     * Callable[n]#call[n] or Callable[n]r#call[n].
     */
    static private void compileBind2
        (LambdaList ll, Object argVarList, Object restVar, Env env) {
        /*
         * argVarList is a list of local variables in which
         * value passed to the function (for req and/or opt
         * parameters) is stored.
         * restVar is local variable in which values passed
         * to the function (for rest and/or key parameters)
         * are stored.
         * compileBind2 is used when the req and opt parameters
         * passed to the function as the method parameters.
         */
        if (ll.isWhole() || ll.isNested())
            // assertion error
            throw new NotReachedException
                ("unacceptable lambdaList: ~S.",
                 Lists.list(ll.params()));

        int nargs = Lists.length(argVarList);
        if (nargs < ll.reqCount() ||
            ll.reqCount() + ll.optCount() < nargs)
            // assertion error
            throw new NotReachedException
                ("nargs error:"
                 +" reqCount="+ll.reqCount()
                 +" optCount="+ll.optCount()
                 +" nargs="+nargs,
                 Symbols.NIL);

        // req and opt
        compileBindReqAndOpt2(ll, argVarList, env);
        // rest and key
        if (restVar == Symbols.NIL) {
            if (ll.isRest() || ll.keyCount() > 0) {
                Context ctx = Context.get(env);
                Object tmpVar = ctx.pushTempVar(Object.class);
                compileFormWithExpectedType
                    (Forms.NIL, Object.class, false, env);
                ctx.addInst(Lists.list(Insts.STORE, tmpVar));
                compileBindRestAndKey(ll, tmpVar, env);
                ctx.popVar();
            }
            else {
                // no code emitted for rest and key
            }
        }
        else {
            if (ll.isRest() || ll.keyCount() > 0) {
                compileBindRestAndKey(ll, restVar, env);
            }
            else {
                // assertion error
                throw new NotReachedException
                    ("rest flag must be NIL: ~S.",
                     Lists.list(ll.params()));
            }
        }
        // aux
        compileBindAux(ll, env);
    }
    static private void compileBindWhole
        (LambdaList ll, Object argsVar, Env env) {
        Context ctx = Context.get(env);
        Object envVar = ctx.getEnvVar();

        // whole
        if (ll.isWhole()) {
            Object var = ll.wholeVar(); // Symbol or LambdaList
            compileBind(var, Lists.car(argsVar), env);
        }
    }
    static private void compileBindReqAndOpt1
        (LambdaList ll, Object argsVar, Env env) {
        Context ctx = Context.get(env);
        Object envVar = ctx.getEnvVar();

        // req
        for (int i = 0; i < ll.reqCount(); i++) {
            Object var = ll.reqVar(i); // Symbol or LambdaList
            ctx.addInst(Lists.list(Insts.LAMBDA_LIST, ll));
            ctx.addInst(Lists.list(Insts.LOAD, argsVar));
            ctx.addInst(Lists.list(Insts.LOAD, envVar));
            ctx.addInst(Lists.list(Insts.INVOKE, Classes.CHECK_MIN_ARGS));
            ctx.addInst(Lists.list(Insts.POP, Object.class));
            compileBind(var, Forms.car(Lists.car(argsVar)), env);
            ctx.addInst(Lists.list(Insts.LOAD, argsVar));
            ctx.addInst(Lists.list(Insts.INVOKE, Classes.CDR));
            ctx.addInst(Lists.list(Insts.STORE, argsVar));
        }

        // opt
        for (int i = 0; i < ll.optCount(); i++) {
            Object var = ll.optVar(i); // Symbol or LambdaList
            Object initform = ctx.initform(var);
            Symbol svar = ll.optSvar(i);
            Symbol tagOk = ctx.genTag("OPT-IF-OK");
            Symbol tagNg = ctx.genTag("OPT-IF-NG");
            Symbol tagDone = ctx.genTag("OPT-IF-DONE");
            // (if args-var
            //   (bind var (car args-var) svar t)
            //   (bind var initform svar nil))
            ctx.addInst(Lists.list(Insts.LOAD, argsVar));
            ctx.addInst(Lists.list(Insts.INVOKE, Classes.OBJ_TO_BOOL));
            ctx.addInst(Lists.list(Insts.IFEQ, tagNg));
            ctx.addInst(tagOk);
            compileBind(var, Forms.car(Lists.car(argsVar)), env);
            if (svar != LambdaList.NOT_SPECIFIED) {
                compileBind(svar, Forms.T, env);
            }
            ctx.addInst(Lists.list(Insts.GOTO, tagDone));
            ctx.addInst(tagNg);
            compileBind(var, initform, env);
            if (svar != LambdaList.NOT_SPECIFIED) {
                compileBind(svar, Forms.NIL, env);
            }
            ctx.addInst(tagDone);
            ctx.addInst(Lists.list(Insts.LOAD, argsVar));
            ctx.addInst(Lists.list(Insts.INVOKE, Classes.CDR));
            ctx.addInst(Lists.list(Insts.STORE, argsVar));
        }
    }
    static private void compileBindReqAndOpt2
        (LambdaList ll, Object argVarList, Env env) {
        Context ctx = Context.get(env);

        // req
        for (int i = 0; i < ll.reqCount(); i++) {
            if (Lists.isEnd(argVarList)) {
                throw new NotReachedException
                    ("argVarList.length is less than reqCount: ~S.",
                     Lists.list(ll.params()));
            }
            Symbol var = Data.symbol(ll.reqVar(i)); // Symbol expected
            Object argVar = Lists.car(argVarList);
            compileBind(var, Lists.car(argVar), env);
            argVarList = Lists.cdr(argVarList);
        }

        // opt
        for (int i = 0; i < ll.optCount(); i++) {
            Symbol var = Data.symbol(ll.optVar(i)); // Symbol expected
            Object argVar = !Lists.isEnd(argVarList)
                ? Lists.car(argVarList) : Symbols.NIL;
            Object initform = ctx.initform(var);
            Symbol svar = ll.optSvar(i);
            if (!Lists.isEnd(argVarList)) {
                compileBind(var, Lists.car(argVar), env);
                if (svar != LambdaList.NOT_SPECIFIED) {
                    compileBind(svar, Forms.T, env);
                }
            }
            else {
                compileBind(var, initform, env);
                if (svar != LambdaList.NOT_SPECIFIED) {
                    compileBind(svar, Forms.NIL, env);
                }
            }
            argVarList = Lists.cdr(argVarList);
        }
    }
    static private void compileBindRestAndKey
        (LambdaList ll, Object argsVar, Env env) {
        Context ctx = Context.get(env);
        Object envVar = ctx.getEnvVar();

        // rest
        if (ll.isRest()) {
            Object var = ll.restVar(); // Symbol or LambdaList
            compileBind(var, Lists.car(argsVar), env);
        }
        else {
            if (ll.keyCount() <= 0) {
                ctx.addInst(Lists.list(Insts.LAMBDA_LIST, ll));
                ctx.addInst(Lists.list(Insts.LOAD, argsVar));
                ctx.addInst(Lists.list(Insts.LOAD, envVar));
                ctx.addInst(Lists.list(Insts.INVOKE, Classes.CHECK_MAX_ARGS));
                ctx.addInst(Lists.list(Insts.POP, Object.class));
            }
        }

        // key
        if (ll.keyCount() > 0) {
            ctx.addInst(Lists.list(Insts.LAMBDA_LIST, ll));
            ctx.addInst(Lists.list(Insts.LOAD, argsVar));
            ctx.addInst(Lists.list(Insts.LOAD, envVar));
            ctx.addInst(Lists.list(Insts.INVOKE, Classes.CHECK_KEY_ARGS));
            ctx.addInst(Lists.list(Insts.POP, Object.class));

            // allocate tmpVar
            Object tmpVar = ctx.pushTempVar(Object.class);

            for (int i = 0; i < ll.keyCount(); i++) {
                Object var = ll.keyVar(i); // Symbol or LambdaList
                Symbol key = ll.keyKey(i);
                Object initform = ctx.initform(var);
                Symbol svar = ll.keySvar(i);
                Symbol tagOk = ctx.genTag("KEY-IF-OK");
                Symbol tagNg = ctx.genTag("KEY-IF-NG");
                Symbol tagDone = ctx.genTag("KEY-IF-DONE");
                // (let ((tmp-var2 (memq key tmp-var1)))
                //  (if tmp-var2
                //   (bind var (cadr tmp-var2) svar t)
                //   (bind var initform svar nil)))
                compileFormWithExpectedType
                    (Forms.quote(key), Object.class, false, env);
                ctx.addInst(Lists.list(Insts.LOAD, argsVar));
                ctx.addInst(Lists.list(Insts.INVOKE, Classes.MEMQ));
                ctx.addInst(Lists.list(Insts.STORE, tmpVar));
                ctx.addInst(Lists.list(Insts.LOAD, tmpVar));
                ctx.addInst(Lists.list(Insts.INVOKE, Classes.OBJ_TO_BOOL));
                ctx.addInst(Lists.list(Insts.IFEQ, tagNg));
                ctx.addInst(tagOk);
                compileBind(var, Forms.car(Forms.cdr(Lists.car(tmpVar))), env);
                if (svar != LambdaList.NOT_SPECIFIED) {
                    compileBind(svar, Forms.T, env);
                }
                ctx.addInst(Lists.list(Insts.GOTO, tagDone));
                ctx.addInst(tagNg);
                compileBind(var, initform, env);
                if (svar != LambdaList.NOT_SPECIFIED) {
                    compileBind(svar, Forms.NIL, env);
                }
                ctx.addInst(tagDone);
            }
            // release tmpVar
            ctx.popVar();
        }
    }
    static private void compileBindAux(LambdaList ll, Env env) {
        Context ctx = Context.get(env);

        // aux
        for (int i = 0; i < ll.auxCount(); i++) {
            Object var = ll.auxVar(i); // Symbol or LambdaList
            Object initform = ctx.initform(var);
            compileBind(var, initform, env);
        }
    }
    static private void compileBind(Object var, Object form, Env env) {
        if (Data.isSymbol(var)) {
            // var is Symbol
            _compileBind(Data.symbol(var), form, env);
        }
        else {
            // var must be LambdaList
            _compileBind(Data.lambdaList(var), form, env);
        }
    }
    static private void _compileBind(Symbol var, Object form, Env env) {
        Context ctx = Context.get(env);
        Object envVar = ctx.getEnvVar();
        Object vi = ctx.getVar(var, env);
        Class type = Data.javaClass(Lists.caddr(vi));
        if (Logger.tracelevelp(env)) {
            Logger.trace("[comp:bind] var =~S",
                         Lists.list(var), env);
            Logger.trace("[comp:bind] varinfo=~S",
                         Lists.list(vi), env);
            Logger.trace("[comp:bind] form=~S",
                         Lists.list(form), env);
            Logger.trace("[comp:bind] env =~S",
                         Lists.list(envVar), env);
        }
        if (Lists.cadr(vi) != Symbols.NIL) {
            /* var is local */
            compileFormWithExpectedType(form, type, false, env);
            ctx.addInst(Lists.list(Insts.STORE, vi));
        }
        else {
            /* var is special */
            ctx.addInst(Lists.list(Insts.LOAD, envVar));
            ctx.addInst(Lists.list(Insts.VAR, var));
            compileFormWithExpectedType(form, type, false, env);
            ctx.addInst(Lists.list(Insts.ENV_BIND, type));
        }
    }
    static private void _compileBind(LambdaList ll, Object form, Env env) {
        Context ctx = Context.get(env);
        Object tmpVar = ctx.pushTempVar(Object.class);
        if (Logger.tracelevelp(env)) {
            Logger.trace("[comp:bind] ll =~S",
                         Lists.list(ll.params()), env);
            Logger.trace("[comp:bind] form=~S",
                         Lists.list(form), env);
        }
        compileFormWithExpectedType(form, Object.class, false, env);
        ctx.addInst(Lists.list(Insts.STORE, tmpVar));
        compileBind1(ll, tmpVar, env);
        ctx.popVar();
    }
    static void compileForm(Object exp, Env env) {
        if (Logger.tracelevelp(env)) {
            Logger.trace("[comp:form] exp=~S",
                         Lists.list(exp), env);
        }
        try {
            _compileForm(exp, env);
        } catch (Compiler.Exception e) {
            throw e.push(exp);
        } catch (TracableException e) {
            throw e;
        } catch (java.lang.Exception e) {
            throw new Compiler.Exception("compile error", e).push(exp);
        }
    }
    static private void _compileForm(Object exp, Env env)
        throws java.lang.Exception {
        // for symbols
        if (Data.isSymbol(exp)) {
            compileSymbol(Data.symbol(exp), env);
        }
        // for other atoms
        else if (Data.isAtom(exp)) {
            compileAtom(exp, env);
        }
        // for lists
        else if (Data.isList(exp)) {
            Object car = Lists.car(exp);
            Object cdr = Lists.cdr(exp);
            if (car == Symbols.IF) {
                compileIf(cdr, env);
            }
            else if (car == Symbols.AND) {
                compileAnd(cdr, env);
            }
            else if (car == Symbols.OR) {
                compileOr(cdr, env);
            }
            else if (car == Symbols.SETQ) {
                compileSetq(cdr, env);
            }
            else if (car == Symbols.PSETQ) {
                compilePsetq(cdr, env);
            }
            else if (car == Symbols.QUOTE) {
                compileQuote(cdr, env);
            }
            else if (car == Symbols.FUNCTION) {
                compileFunction(cdr, env);
            }
            else if (car == Symbols.PROGN) {
                compileProgn(cdr, env);
            }
            else if (car == Symbols.PROG) {
                compileProg(cdr, env);
            }
            else if (car == Symbols.RETURN) {
                compileReturn(cdr, env);
            }
            else if (car == Symbols.GO) {
                compileGo(cdr, env);
            }
            else if (car == Symbols.THROW) {
                compileThrow(cdr, env);
            }
            else if (car == Symbols.CATCH) {
                compileCatch(cdr, env);
            }
            else if (car == Symbols.UNWIND_PROTECT) {
                compileUnwindProtect(cdr, env);
            }
            else if (car == Symbols.EVAL_WHEN) {
                compileEvalWhen(cdr, env);
            }
            else if (car == Symbols.DECLARE) {
                compileDeclare(cdr, env);
            }
            else if (car == Symbols.MULTIPLE_VALUE_BIND) {
                compileMultipleValueBind(cdr, env);
            }
            else if (car == Symbols.MULTIPLE_VALUE_LIST) {
                compileMultipleValueList(cdr, env);
            }
            else if (car == Symbols.MULTIPLE_VALUE_SETQ) {
                compileMultipleValueSetq(cdr, env);
            }
            else if (car == Symbols.NTH_VALUE) {
                compileNthValue(cdr, env);
            }
            else if (car == Symbols.LIST) {
                compileList(cdr, env);
            }
            else if (car == Symbols.VALUES) {
                compileValues(cdr, env);
            }
            else if (Data.isSymbol(car)) {
                Symbol sym = Data.symbol(car);
                Symbol handlerType = SysSymbols.C_HANDLER_BUILD_INSTS;
                // C-HANDLER for pass 2
                if (env.lisp().getProp(sym, handlerType)
                    != Symbols.NIL) {
                    Function handler = Data.function
                        (env.lisp().getProp(sym, handlerType));
                    if (Logger.tracelevelp(env))
                        Logger.trace("[compile] type=~S handler=~S",
                                     Lists.list(handlerType, handler),
                                     env);
                    Funcall.funcall1(handler, exp, env);
                }
                else {
                    _compileCall(sym, cdr, env);
                }
            }
            else if (Data.isList(car)) {
                Object caar = Lists.car(car);
                // LAMBDA expression: create EXPR
                // and compile interpreted function
                if (caar == Symbols.LAMBDA) {
                    // car: lambda -> compile expr as block
                    compileLambdaAsBlock(car, cdr, env);
                }
                else {
                    throw new ProgramException
                        ("illegal function name: ~S.", Lists.list(caar));
                }
            }
            else {
                throw new ProgramException
                    ("unknown exp: ~S.", Lists.list(exp));
            }
        }
        else {
            // for others
            throw new ProgramException
                ("unknown exp: ~S.", Lists.list(exp));
        }
    }
    static void compileCall(Symbol sym, Object cdr, Env env) {
        if (Logger.tracelevelp(env)) {
            Logger.trace("[comp:call] func=~S",
                         Lists.list(sym), env);
        }
        try {
            _compileCall(sym, cdr, env);
        } catch (Compiler.Exception e) {
            throw e.push(sym);
        } catch (TracableException e) {
            throw e;
        } catch (java.lang.Exception e) {
            throw new Compiler.Exception("compile error", e).push(sym);
        }
    }
    static private void _compileCall(Symbol sym, Object cdr, Env env)
        throws java.lang.Exception {
        Object plist = env.lisp().functions(sym);
        Object type = Lists.car(plist);
        Object func = Lists.cadr(plist);
        // FSUBR: compile combination for special form
        if (type == Symbols.FSUBR) {
            Logger.warn
                ("FSUBR ~S is used in the body of the function.",
                 Lists.list(sym), env);
            compileCall_FSubr(sym, cdr, env);
        }
        // FEXPR: compile combination for interpreted special form
        else if (type == Symbols.FEXPR) {
            Logger.warn
                ("FEXPR ~S is used in the body of the function.",
                 Lists.list(sym), env);
            compileCall_FExpr(sym, cdr, env);
        }
        // MACRO: error!
        else if (type == Symbols.MACRO) {
            throw new NotReachedException
                ("Unprocessed macro detected: ~S. "+
                 "Macro must be expanded at pass1.",
                 Lists.list(sym));
        }
        else {
            // normal function call (SUBR, EXPR or ARRAY)
            compileCall_Default(sym, func, cdr, env);
        }
    }
    static private void compileSymbol(Symbol sym, Env env) {
        Context ctx = Context.get(env);
        Object envVar = ctx.getEnvVar();

        if (sym.isSelfeval()) {
            throw new NotReachedException
                ("selfeval symbol must be quoted: ~S",
                 Lists.list(sym));
        }
        if (ctx.peekType().expected == null) {
            // return value can be discarded
            return;
        }
        Object vi = ctx.getVar(sym, env);
        Class type = Data.javaClass(Lists.caddr(vi));
        if (Lists.cadr(vi) != Symbols.NIL) {
            /* var is local */
            ctx.addInst(Lists.list(Insts.LOAD, vi));
        }
        else {
            /* var is special */
            ctx.addInst(Lists.list(Insts.LOAD, envVar));
            ctx.addInst(Lists.list(Insts.VAR, sym));
            ctx.addInst(Lists.list(Insts.ENV_GET, type));
        }
        // type handling
        compileTypeMapping(type, false, env);
    }
    static private void compileAtom(Object atom, Env env) {
        throw new NotReachedException
            ("atom must be quoted : ~S", Lists.list(atom));
    }
    static private void compileIf(Object cdr, Env env) {
        Context ctx = Context.get(env);
        Object test = Lists.car(cdr);
        Object  con = Lists.cadr(cdr);
        Object  alt = Lists.caddr(cdr);
        if (Forms.isNil(test)) {
            // test is NIL
            compileForm(alt, env);
        }
        else if (Forms.isConstant(test)) {
            // test is a constant (not a variable)
            compileForm(con, env);
        }
        else if (Classes.alwaysTrue(TypeCalculator.typeOf(test, env))) {
            // test is a form which never returns NIL.
            compileFormWithExpectedType(test, null, false, env);
            compileForm(con, env);
        }
        else {
            Symbol tagOk = ctx.genTag("IF-OK");
            Symbol tagNg = ctx.genTag("IF-NG");
            Symbol tagDone = ctx.genTag("IF-DONE");
            compileFormWithExpectedType
                (test, boolean.class, false, env);
            ctx.addInst(Lists.list(Insts.IFEQ, tagNg));
            ctx.addInst(tagOk);
            compileForm(con, env);
            ctx.addInst(Lists.list(Insts.GOTO, tagDone));
            ctx.addInst(tagNg);
            compileForm(alt, env);
            ctx.addInst(tagDone);
        }
    }
    static private void compileAnd(Object cdr, Env env) {
        Context ctx = Context.get(env);
        switch (Lists.length(cdr)) {
        case 0:
            compileForm(Forms.T, env);
            break;
        case 1:
            compileForm(Lists.car(cdr), env);
            break;
        default:
            Symbol tagNg = ctx.genTag("AND-NG");
            Symbol tagDone = ctx.genTag("AND-DONE");
            boolean tagNgUsed = false;
            for (Object l = cdr; !Lists.isEnd(l); l = Lists.cdr(l)) {
                Object exp = Lists.car(l);
                if (Lists.isEnd(Lists.cdr(l))) {
                    compileForm(exp, env);
                    ctx.addInst(Lists.list(Insts.GOTO, tagDone));
                }
                else if (Forms.isNil(exp)) {
                    // exp is NIL
                    compileForm(exp, env);
                    ctx.addInst(Lists.list(Insts.GOTO, tagDone));
                    break;
                }
                else if (Forms.isConstant(exp)) {
                    // exp is CONSTANT
                    compileFormWithExpectedType(exp, null, false, env);
                }
                else if (Classes.alwaysTrue(TypeCalculator.typeOf(exp, env))) {
                    // exp never returns NIL
                    compileFormWithExpectedType(exp, null, false, env);
                }
                else {
                    compileFormWithExpectedType
                        (exp, boolean.class, false, env);
                    ctx.addInst(Lists.list(Insts.IFEQ, tagNg));
                    tagNgUsed = true;
                }
            }
            if (tagNgUsed) {
                ctx.addInst(tagNg);
                compileForm(Forms.NIL, env);
            }
            ctx.addInst(tagDone);
            break;
        }
    }
    static private void compileOr(Object cdr, Env env) {
        Context ctx = Context.get(env);
        switch (Lists.length(cdr)) {
        case 0:
            compileForm(Forms.NIL, env);
            break;
        case 1:
            compileForm(Lists.car(cdr), env);
            break;
        default:
            Symbol tagOk = ctx.genTag("OR-OK");
            Symbol tagDone = ctx.genTag("OR-DONE");
            boolean tagOkUsed = false;
            Class type = ctx.peekType().expected;
            boolean retvalRequired = (type != null);
            Object tmpVar = retvalRequired
                ? ctx.pushTempVar(type) : Symbols.NIL;
            for (Object l = cdr; !Lists.isEnd(l); l = Lists.cdr(l)) {
                Object exp = Lists.car(l);
                if (Lists.isEnd(Lists.cdr(l))) {
                    compileForm(exp, env);
                    ctx.addInst(Lists.list(Insts.GOTO, tagDone));
                }
                else if (Forms.isNil(exp)) {
                    // exp is NIL
                    compileFormWithExpectedType(exp, null, false, env);
                }
                else if (Forms.isConstant(exp)) {
                    // exp is CONSTANT
                    compileForm(exp, env);
                    ctx.addInst(Lists.list(Insts.GOTO, tagDone));
                    break;
                }
                else if (Classes.alwaysTrue(TypeCalculator.typeOf(exp, env))) {
                    // exp never returns NIL
                    compileForm(exp, env);
                    ctx.addInst(Lists.list(Insts.GOTO, tagDone));
                    break;
                }
                else {
                    if (retvalRequired) {
                        compileFormWithExpectedType(exp, type, false, env);
                        ctx.addInst(Lists.list(Insts.STORE, tmpVar));
                        //compileFormWithExpectedType
                        //    (Lists.car(tmpVar), boolean.class, false, env);
                        ctx.addInst(Lists.list(Insts.LOAD, tmpVar));
                        compileTypeMapping
                            (exp, boolean.class, type, false, false, env);
                        ctx.addInst(Lists.list(Insts.IFNE, tagOk));
                    }
                    else {
                        // return value can be discarded
                        compileFormWithExpectedType
                            (exp, boolean.class, false, env);
                        ctx.addInst(Lists.list(Insts.IFNE, tagOk));
                    }
                    tagOkUsed = true;
                }
            }
            if (tagOkUsed) {
                ctx.addInst(tagOk);
                if (retvalRequired) {
                    ctx.addInst(Lists.list(Insts.LOAD, tmpVar));
                    compileTypeMapping(type, false, env);
                }
            }
            ctx.addInst(tagDone);
            if (retvalRequired) {
                // clean-up
                ctx.popVar();
            }
            break;
        }
    }
    static private void compileSetq(Object cdr, Env env) {
        int len = Lists.length(cdr);
        if (len%2 == 1) {
            throw new ProgramException
                ("odd number of arguments: ~S.", Lists.list(cdr));
        }
        else if (len == 0) {
            compileForm(Forms.NIL, env);
            return;
        }

        Context ctx = Context.get(env);
        Object envVar = ctx.getEnvVar();

        Symbol retVar = Symbols.NIL;
        for (Object l = cdr; !Lists.isEnd(l); l = Lists.cddr(l)) {
            Symbol var = Data.symbol(Lists.car(l));
            Object form = Lists.cadr(l);
            compileSet(var, form, env);
            if (Lists.isEnd(Lists.cddr(l))) {
                retVar = var;
            }
        }
        compileForm(retVar, env);
    }
    static private void compilePsetq(Object cdr, Env env) {
        int len = Lists.length(cdr);
        if (len%2 == 1) {
            throw new ProgramException
                ("odd number of arguments: ~S.", Lists.list(cdr));
        }
        else if (len == 0) {
            compileForm(Forms.NIL, env);
            return;
        }

        Context ctx = Context.get(env);
        Object envVar = ctx.getEnvVar();

        // vars and vals
        Object varList = Symbols.NIL;
        Object tmpVarList = Symbols.NIL;
        for (Object l = cdr; !Lists.isEnd(l); l = Lists.cddr(l)) {
            Symbol var = Data.symbol(Lists.car(l));
            Object val = Lists.cadr(l);
            Class type = ctx.javaTypeOfVar(var, true);
            Object tmpVar = ctx.pushTempVar(type);
            compileFormWithExpectedType(val, type, false, env);
            ctx.addInst(Lists.list(Insts.STORE, tmpVar));
            varList = Lists.cons(var, varList);
            tmpVarList = Lists.cons(tmpVar, tmpVarList);
        }

        // reverse varList and tmpVarList
        varList = Lists.nreverse(varList);
        tmpVarList = Lists.nreverse(tmpVarList);

        // set vals to vars
        Symbol retVar = Symbols.NIL;
        for (Object l1 = varList, l2 = tmpVarList;
             !Lists.isEnd(l1) && !Lists.isEnd(l2);
             l1 = Lists.cdr(l1), l2 = Lists.cdr(l2)) {
            Symbol var = Data.symbol(Lists.car(l1));
            Object tmpVar = Lists.car(l2);
            compileSet(var, Lists.car(tmpVar), env);
            if (Lists.isEnd(Lists.cdr(l1))) {
                retVar = var;
            }
        }
        compileForm(retVar, env);
        // clean up
        for (int i = 0, max = len/2; i < max; i++) {
            ctx.popVar();
        }
    }

    static private void compileSet(Symbol var, Object form, Env env) {
        Context ctx = Context.get(env);
        Object envVar = ctx.getEnvVar();
        Object vi = ctx.getVar(var, env);
        Class type = Data.javaClass(Lists.caddr(vi));
        if (Logger.tracelevelp(env)) {
            Logger.trace("[comp:set] var =~S",
                         Lists.list(var), env);
            Logger.trace("[comp:set] varinfo=~S",
                         Lists.list(vi), env);
            Logger.trace("[comp:set] form=~S",
                         Lists.list(form), env);
            Logger.trace("[comp:set] env =~S",
                         Lists.list(envVar), env);
        }
        if (Lists.cadr(vi) != Symbols.NIL) {
            /* var is local */
            compileFormWithExpectedType(form, type, false, env);
            ctx.addInst(Lists.list(Insts.STORE, vi));
        }
        else {
            /* var is special */
            ctx.addInst(Lists.list(Insts.LOAD, envVar));
            ctx.addInst(Lists.list(Insts.VAR, var));
            compileFormWithExpectedType(form, type, false, env);
            ctx.addInst(Lists.list(Insts.ENV_SET, type));
        }
    }

    static private void compileQuote(Object cdr, Env env) {
        if (Lists.length(cdr) != 1)
            throw new ProgramException
                ("args.length must be 1: ~S.", Lists.list(cdr));
        Context ctx = Context.get(env);
        Object atom = Lists.car(cdr);
        if (ctx.peekType().expected == null) {
            // return value can be discarded
            return;
        }
        Class typeExpected = ctx.peekType().expected;
        Class typeActual;
        if (typeExpected.equals(boolean.class)) {
            boolean b = Data.toBoolean(atom);
            ctx.addInst(Lists.list(Insts.PUSH, Boolean.valueOf(b)));
            typeActual = boolean.class;
        }
        else if (typeExpected.equals(int.class)) {
            ctx.addInst(Lists.list(Insts.PUSH, Data.fixnum(atom)));
            typeActual = int.class;
        }
        else if (typeExpected.equals(double.class)) {
            ctx.addInst(Lists.list(Insts.PUSH, Data.flonum(atom)));
            typeActual = double.class;
        }
        else if (typeExpected.equals(char.class)) {
            ctx.addInst(Lists.list(Insts.PUSH, Data.character(atom)));
            typeActual = char.class;
        }
        else if (Data.isString(atom)) {
            ctx.addInst(Lists.list(Insts.PUSH, Data.string(atom)));
            typeActual = String.class;
        }
        else {
            ctx.addInst(Lists.list(Insts.CONST, atom));
            /*
             * Since an atom are stored in the instance field
             * of the CompiledExpr with type java.lang.Object,
             * typeActual must be java.lang.Object.
             */
            //typeActual = (atom == null)
            //    ? Object.class : atom.getClass(); // wrong!
            typeActual = Object.class; // right!
        }
        // type handling
        compileTypeMapping(typeActual, false, env);
    }
    static private void compileFunction(Object cdr, Env env)
        throws java.lang.Exception {
        if (Lists.length(cdr) != 1)
            throw new ProgramException
                ("args.length must be 1: ~S.", Lists.list(cdr));
        Context ctx = Context.get(env);
        Object envVar = ctx.getEnvVar();

        if (ctx.peekType().expected == null) {
            // return value can be discarded
            return;
        }
        Object obj = Lists.car(cdr);
        if (Data.isSymbol(obj)) {
            Symbol sym = Data.symbol(obj);
            Object plist = env.lisp().functions(sym);
            Object type = Lists.car(plist);
            Object func = Lists.cadr(plist);

            // analyzes the function object
            // and collects compilation informations.
            Values mv = Subrs.analyzeSubr(sym, func, -1, ctx.group(), env);
            // how-to-compile-this-function-call
            Object howToCompile = mv.nth(0);
            // target-method
            Object tm = mv.nth(1); // must be null
            // subr-field
            Object sf = mv.nth(2);
            // callable-method
            Object cm = mv.nth(3); // must be null
            // can-produce-values-p
            Object cpv = mv.nth(4); // must be null
            // context-of-compiled-expr
            Object ce = mv.nth(5);

            if (howToCompile == Subrs.USE_TARGET_METHOD) {
                throw new NotReachedException
                    ("Why reached here?", Symbols.NIL);
            }
            else if (howToCompile == Subrs.USE_SUBR_FIELD) {
                // push function
                ctx.addInst(Lists.list(Insts.GET, Data.javaField(sf)));
            }
            else if (howToCompile == Subrs.USE_CONTEXT) {
                Context ctx2 = (Context) ce;
                if (ctx2 == ctx) {
                    Object tmpVar = ctx.getVar(Callables.LOCAL_SLOT_THIS, env);
                    // push function
                    ctx.addInst(Lists.list(Insts.LOAD, tmpVar));
                }
                else {
                    String classname = ctx2.classname();
                    // push function
                    ctx.addInst(Lists.list(Insts.COMPILED_EXPR, classname));
                }
            }
            else {
                // push function
                ctx.addInst(Lists.list(Insts.CONST, obj));
                ctx.addInst(Lists.list(Insts.LOAD, envVar));
                ctx.addInst(Lists.list(Insts.INVOKE, Classes.FUNCTION));
            }
        }
        else if (Data.isPair(obj) &&
                 Lists.car(obj) == Symbols.LAMBDA) {
            compileLambda(obj, env);
            Context subctx = ctx.getLastChild();
            String classname = subctx.classname(); // XXX
            if (Logger.tracelevelp(env)) {
                Logger.trace("[comp:func] func =~S",
                             Lists.list(obj), env);
                Logger.trace("[comp:func] class=~S",
                             Lists.list(classname), env);
            }
            // push function
            ctx.addInst(Lists.list(Insts.COMPILED_EXPR, classname));
        }
        else {
            throw new ProgramException
                ("unexpected argument type: ~S.", Lists.list(obj));
        }
        // type handling
        compileTypeMapping(Function.class, false, env);
    }
    static private void compileProgn(Object cdr, Env env) {
        Context ctx = Context.get(env);
        if (cdr == Symbols.NIL) {
            compileForm(Forms.NIL, env);
        }
        else {
            for (Object l = cdr; !Lists.isEnd(l); l = Lists.cdr(l)) {
                if (Lists.isEnd(Lists.cdr(l))) {
                    compileForm(Lists.car(l), env);
                }
                else {
                    Class type = null; // retval will be discarded
                    compileFormWithExpectedType
                        (Lists.car(l), type, true, env);
                }
            }
        }
    }
    static private void compileProg(Object cdr, Env env) {
        if (Lists.length(cdr) <= 0)
            throw new ProgramException
                ("args.length must be greater than 0: ~S.",
                 Lists.list(cdr));
        /*
         * [expected cdr]
         * cdr  := (vars . body)
         * vars := (var0 var1 ... varN)
         * body := (form0 form1 ... formN)
         */
        Context ctx = Context.get(env);
        Object vars = Lists.car(cdr);
        Object declsAndBody = Lists.cdr(cdr);
        Object decls, body;
        {
            Values mv = DeclInfo.findDecls(declsAndBody);
            decls = mv.nth(0);
            body = mv.nth(1);
        }
        Symbol tagStart = ctx.genTag("PROG-START");
        Symbol tagDone = ctx.genTag("PROG-DONE");

        // prepare new block
        ctx.pushBlock(Context.BlockInfo.PROG,
                      tagStart, tagDone, decls, vars, env);

        // lambdaList
        LambdaList ll = new LambdaList(vars, env);
        if (ll.optCount() > 0 || ll.isRest() ||
            ll.keyCount() > 0 || ll.auxCount() > 0 ||
            ll.isWhole() || ll.isNested())
            // assertion error
            throw new NotReachedException
                ("unacceptable lambdaList: ~S.",
                 Lists.list(ll.params()));

        // req
        for (int i = 0; i < ll.reqCount(); i++) {
            Symbol var = Data.symbol(ll.reqVar(i));
            Class type = ctx.javaTypeOfVar(var, true);
            Object init;
            if (type.equals(int.class)) {
                init = Forms.quote(0);
            }
            else if (type.equals(double.class)) {
                init = Forms.quote(0.0);
            }
            else {
                init = Forms.NIL;
            }
            compileBind(var, init, env);
        }

        // compile body (first pass: gather tags)
        for (Object l = body; !Lists.isEnd(l); l = Lists.cdr(l)) {
            Object exp = Lists.car(l);
            if (Data.isSymbol(exp)) {
                Symbol tag = Data.symbol(exp);
                ctx.addBlockTag(tag);
            }
            else {
                /* ignore */
            }
        }

        // set TypeInfo#inProg flag true
        // -> this ti will be used as prog-return-type
        //    (type of non-local-exit).
        Context.TypeInfo ti = ctx.peekType();
        ti.inProg = true;

        // compile body (second pass: compile statements)
        for (Object l = body; !Lists.isEnd(l); l = Lists.cdr(l)) {
            Object exp = Lists.car(l);
            if (Data.isSymbol(exp)) {
                Symbol tag = Data.symbol(exp);
                ctx.addInst(ctx.getBlockTag(tag));
            }
            else {
                Class type = null; // retval will be discarded
                compileFormWithExpectedType(exp, type, true, env);
            }
        }

        // set TypeInfo#inProg flag false
        ti.inProg = false;

        // prog always returns nil
        compileForm(Forms.NIL, env);

        // clean up...
        ctx.popBlock();
    }
    static private void compileReturn(Object cdr, Env env) {
        if (Lists.length(cdr) != 1)
            throw new ProgramException
                ("args.length must be 1: ~S.", Lists.list(cdr));
        Context ctx = Context.get(env);
        Symbol tagDone = ctx.getProgBlockEndTag();
        Context.TypeInfo ti = ctx.peekProgReturnType();
        Class typeExpected = ti.expected;
        boolean consume = ti.canConsumeValues;
        compileFormWithExpectedType
            (Lists.car(cdr), typeExpected, consume, env);
        ctx.addInst(Lists.list(Insts.GOTO, tagDone));
    }
    static private void compileGo(Object cdr, Env env) {
        if (Lists.length(cdr) != 1)
            throw new ProgramException
                ("args.length must be 1: ~S.", Lists.list(cdr));
        Context ctx = Context.get(env);
        Symbol tag = ctx.getBlockTag(Data.symbol(Lists.car(cdr)));
        ctx.addInst(Lists.list(Insts.GOTO, tag));
    }
    static private void compileThrow(Object cdr, Env env) {
        if (Lists.length(cdr) != 2)
            throw new ProgramException
                ("args.length must be 2: ~S.", Lists.list(cdr));
        Context ctx = Context.get(env);
        Object envVar = ctx.getEnvVar();
        Object tag = Lists.car(cdr);
        Object form = Lists.cadr(cdr);
        compileFormWithExpectedType(tag, Symbol.class, false, env);
        compileFormWithExpectedType(form, Object.class, true, env);
        ctx.addInst(Lists.list(Insts.LOAD, envVar));
        ctx.addInst(Lists.list(Insts.INVOKE, Classes.EXIT_WITH_THROW));
        compileTypeMapping(Object.class, false, env);
    }
    static private void compileCatch(Object cdr, Env env) {
        if (Lists.length(cdr) <= 0)
            throw new ProgramException
                ("args.length must be greater than 0: ~S.",
                 Lists.list(cdr));
        Context ctx = Context.get(env);
        Object envVar = ctx.getEnvVar();
        Object tagspec = Lists.car(cdr);
        Object forms = Forms.progn(Lists.cdr(cdr));

        Symbol tagStart = ctx.genTag("CATCH-START");
        Symbol tagEnd = ctx.genTag("CATCH-END");
        Symbol tagFrom1 = ctx.genTag("FROM1");
        Symbol tagTo1 = ctx.genTag("TO1");
        Symbol tagHandler = ctx.genTag("HANDLER");

        // tmpVar0 for tagspec
        Class type0 = Object.class;
        Object tmpVar0 = ctx.pushTempVar(type0);
        // tmpVar1 for forms
        Class type1 = ctx.peekType().expected;
        Object tmpVar1 = (type1 != null)
            ? ctx.pushTempVar(type1) : Symbols.NIL;
        // tmpVar2 for exception
        Class type2 = NonLocalExit.THROW.class;
        Object tmpVar2 = ctx.pushTempVar(type2);

        compileFormWithExpectedType(tagspec, type0, false, env);
        ctx.addInst(Lists.list(Insts.STORE, tmpVar0));
        ctx.addInst(tagStart);
        ctx.addInst(Lists.list(Insts.CATCH_FROM, tagFrom1));
        compileForm(forms, env);
        if (type1 != null) {
            ctx.addInst(Lists.list(Insts.STORE, tmpVar1));
        }
        ctx.addInst(Lists.list(Insts.CATCH_TO, tagTo1));
        ctx.addInst(Lists.list(Insts.GOTO, tagEnd));
        ctx.addInst(Lists.list(Insts.CATCH_HANDLER, tagHandler));
        ctx.addInst(Lists.list(Insts.STORE, tmpVar2));
        ctx.addInst(Lists.list(Insts.LOAD, tmpVar2));
        ctx.addInst(Lists.list(Insts.LOAD, tmpVar0));
        ctx.addInst(Lists.list(Insts.LOAD, envVar));
        ctx.addInst(Lists.list(Insts.INVOKE, Classes.VAL_OF_THROW));
        compileTypeMapping(Object.class, true, env);
        if (type1 != null) {
            ctx.addInst(Lists.list(Insts.STORE, tmpVar1));
        }
        ctx.addInst(Lists.list(Insts.CATCH, tagFrom1, tagTo1,
                               tagHandler, NonLocalExit.THROW.class));
        ctx.addInst(tagEnd);
        if (type1 != null) {
            ctx.addInst(Lists.list(Insts.LOAD, tmpVar1));
            //compileTypeMapping(type1, true, env);
        }

        // clean-up
        ctx.popVar();
        if (type1 != null) {
            ctx.popVar();
        }
        ctx.popVar();
    }
    static private void compileUnwindProtect(Object cdr, Env env) {
        if (Lists.length(cdr) <= 0)
            throw new ProgramException
                ("args.length must be greater than 0: ~S.",
                 Lists.list(cdr));
        Context ctx = Context.get(env);
        Object form = Lists.car(cdr);
        Object cleanupforms = Forms.progn(Lists.cdr(cdr));

        Symbol tagStart = ctx.genTag("UNWIND-PROTECT-START");
        Symbol tagEnd = ctx.genTag("UNWIND-PROTECT-END");
        Symbol tagFrom1 = ctx.genTag("FROM1");
        Symbol tagTo1 = ctx.genTag("TO1");
        Symbol tagFrom2 = ctx.genTag("FROM2");
        Symbol tagTo2 = ctx.genTag("TO2");
        Symbol tagHandler = ctx.genTag("HANDLER");

        // tmpVar1 for form
        Class type1 = ctx.peekType().expected;
        Object tmpVar1 = (type1 != null)
            ? ctx.pushTempVar(type1) : Symbols.NIL;
        // tmpVar2 for exception
        Class type2 = Throwable.class;
        Object tmpVar2 = ctx.pushTempVar(type2);

        ctx.addInst(tagStart);
        ctx.addInst(Lists.list(Insts.CATCH_FROM, tagFrom1));
        compileForm(form, env);
        if (type1 != null) {
            ctx.addInst(Lists.list(Insts.STORE, tmpVar1));
        }
        ctx.addInst(Lists.list(Insts.CATCH_TO, tagTo1));
        // expectedType is null -> return value will be discarded
        compileFormWithExpectedType(cleanupforms, null, true, env);
        ctx.addInst(Lists.list(Insts.GOTO, tagEnd));
        ctx.addInst(Lists.list(Insts.CATCH_HANDLER, tagHandler));
        ctx.addInst(Lists.list(Insts.CATCH_FROM, tagFrom2));
        ctx.addInst(Lists.list(Insts.STORE, tmpVar2));
        ctx.addInst(Lists.list(Insts.CATCH_TO, tagTo2));
        // expectedType is null -> return value will be discarded
        compileFormWithExpectedType(cleanupforms, null, true, env);
        ctx.addInst(Lists.list(Insts.LOAD, tmpVar2));
        ctx.addInst(Lists.list(Insts.THROW));
        ctx.addInst(Lists.list(Insts.CATCH, tagFrom1, tagTo1, tagHandler));
        ctx.addInst(Lists.list(Insts.CATCH, tagFrom2, tagTo2, tagHandler));
        ctx.addInst(tagEnd);
        if (type1 != null) {
            ctx.addInst(Lists.list(Insts.LOAD, tmpVar1));
            //compileTypeMapping(type1, true, env);
        }
        // clean-up
        ctx.popVar();
        if (type1 != null) {
            ctx.popVar();
        }
    }
    static private void compileEvalWhen(Object cdr, Env env) {
        if (Lists.length(cdr) <= 0)
            throw new ProgramException
                ("args.length must be greater than 0: ~S.",
                 Lists.list(cdr));
        Object timeList = Lists.car(cdr);
        Object body = Lists.cdr(cdr);
        if (Lists.memq(Symbols.EVAL, timeList) != Symbols.NIL) {
            compileProgn(body, env);
        }
        else {
            compileForm(Forms.NIL, env);
        }
    }
    static private void compileDeclare(Object cdr, Env env) {
        Logger.warn("declare is ignored: ~S",
                    Lists.list(cdr), env);
        compileForm(Forms.quote(Symbols.DECLARE), env);
    }
    static private void compileMultipleValueBind(Object cdr, Env env) {
        if (Lists.length(cdr) <= 1)
            throw new ProgramException
                ("args.length must be greater than 1: ~S.",
                 Lists.list(cdr));
        Context ctx = Context.get(env);
        Object vars = Lists.car(cdr);
        Object mvform = Lists.cadr(cdr);
        Object declsAndBody = Lists.cddr(cdr);
        Object decls, body;
        {
            Values mv = DeclInfo.findDecls(declsAndBody);
            decls = mv.nth(0);
            body = mv.nth(1);
        }

        // tmpVar for MV
        Class typeMv = Values.class;
        Object tmpVarMv = ctx.pushTempVar(typeMv);

        // compile MV
        compileFormWithExpectedType(mvform, Object.class, true, env);
        ctx.addInst(Lists.list(Insts.INVOKE, Classes.MULTIPLE_VALUE));
        ctx.addInst(Lists.list(Insts.STORE, tmpVarMv));

        // assign values to tmpVarList
        Object tmpVarList = Symbols.NIL;
        for (int i = 0, j = Lists.length(vars); i < j; i++) {
            Object tmpVar = ctx.pushTempVar(Object.class);
            ctx.addInst(Lists.list(Insts.LOAD, tmpVarMv));
            compileFormWithExpectedType
                (Forms.quote(i), int.class, false, env);
            ctx.addInst(Lists.list(Insts.INVOKE, Classes.NTH_VALUE));
            ctx.addInst(Lists.list(Insts.STORE, tmpVar));
            tmpVarList = Lists.cons(tmpVar, tmpVarList);
        }
        tmpVarList = Lists.nreverse(tmpVarList);

        Symbol tagStart = ctx.genTag("MV-BIND-START");
        Symbol tagDone = ctx.genTag("MV-BIND-DONE");

        // prepare new block
        ctx.pushBlock(Context.BlockInfo.MV_BIND,
                      tagStart, tagDone, decls, vars, env);

        // lambdaList
        LambdaList ll = new LambdaList(vars, env);
        if (ll.optCount() > 0 || ll.isRest() ||
            ll.keyCount() > 0 || ll.auxCount() > 0 ||
            ll.isWhole() || ll.isNested())
            // assertion error
            throw new NotReachedException
                ("unacceptable lambdaList: ~S.",
                 Lists.list(ll.params()));

        // req
        for (int i = 0; i < ll.reqCount(); i++) {
            Symbol var = Data.symbol(ll.reqVar(i));
            Object tmpVar = Lists.car(tmpVarList);
            compileBind(var, Lists.car(tmpVar), env);
            tmpVarList = Lists.cdr(tmpVarList);
        }

        // compile body
        compileProgn(body, env);

        // clean up...
        ctx.popBlock();
        for (int i = 0, j = Lists.length(vars); i < j; i++) {
            ctx.popVar();
        }
        ctx.popVar();
    }
    static private void compileMultipleValueList(Object cdr, Env env) {
        if (Lists.length(cdr) != 1)
            throw new ProgramException
                ("args.length must be 1: ~S.", Lists.list(cdr));
        Context ctx = Context.get(env);
        Object mvform = Lists.car(cdr);

        // emit code for invoking Values#toList
        compileFormWithExpectedType(mvform, Object.class, true, env);
        ctx.addInst(Lists.list(Insts.INVOKE, Classes.MULTIPLE_VALUE));
        ctx.addInst(Lists.list(Insts.INVOKE, Classes.MV_TO_LIST));
        compileTypeMapping(List.class, false, env);
    }
    static private void compileMultipleValueSetq(Object cdr, Env env) {
        if (Lists.length(cdr) != 2)
            throw new ProgramException
                ("args.length must be 2: ~S.", Lists.list(cdr));
        Context ctx = Context.get(env);
        Object vars = Lists.car(cdr);
        Object mvform = Lists.cadr(cdr);

        if (Lists.length(vars) <= 0) {
            compileForm(Forms.NIL, env);
            return;
        }

        // tmpVar for MV
        Class typeMv = Values.class;
        Object tmpVarMv = ctx.pushTempVar(typeMv);

        // compile MV
        compileFormWithExpectedType(mvform, Object.class, true, env);
        ctx.addInst(Lists.list(Insts.INVOKE, Classes.MULTIPLE_VALUE));
        ctx.addInst(Lists.list(Insts.STORE, tmpVarMv));

        // assign values to tmpVarList
        Object tmpVarList = Symbols.NIL;
        for (int i = 0, j = Lists.length(vars); i < j; i++) {
            Object tmpVar = ctx.pushTempVar(Object.class);
            ctx.addInst(Lists.list(Insts.LOAD, tmpVarMv));
            compileFormWithExpectedType
                (Forms.quote(i), int.class, false, env);
            ctx.addInst(Lists.list(Insts.INVOKE, Classes.NTH_VALUE));
            ctx.addInst(Lists.list(Insts.STORE, tmpVar));
            tmpVarList = Lists.cons(tmpVar, tmpVarList);
        }
        tmpVarList = Lists.nreverse(tmpVarList);

        // compile setq
        Object envVar = ctx.getEnvVar();
        Symbol retVar = Symbols.NIL;
        {
            int i = 0;
            for (Object l = vars; !Lists.isEnd(l); l = Lists.cdr(l)) {
                Symbol var = Data.symbol(Lists.car(l));
                Object tmpVar = Lists.car(tmpVarList);
                compileSet(var, Lists.car(tmpVar), env);
                if (i == 0) {
                    retVar = var;
                }
                i++;
            }
        }
        compileForm(retVar, env);
        // clean-up
        for (int i = 0, j = Lists.length(vars); i < j; i++) {
            ctx.popVar();
        }
        ctx.popVar();
    }
    static private void compileNthValue(Object cdr, Env env) {
        if (Lists.length(cdr) != 2)
            throw new ProgramException
                ("args.length must be 2: ~S.", Lists.list(cdr));
        Context ctx = Context.get(env);
        Object n = Lists.car(cdr);
        Object mvform = Lists.cadr(cdr);

        // tmpVar for N
        Class typeN = int.class;
        Object tmpVarN = ctx.pushTempVar(typeN);

        // tmpVar for MV
        Class typeMv = Values.class;
        Object tmpVarMv = ctx.pushTempVar(typeMv);

        // compile N
        compileFormWithExpectedType(n, typeN, false, env);
        ctx.addInst(Lists.list(Insts.STORE, tmpVarN));

        // compile MV
        compileFormWithExpectedType(mvform, Object.class, true, env);
        ctx.addInst(Lists.list(Insts.INVOKE, Classes.MULTIPLE_VALUE));
        ctx.addInst(Lists.list(Insts.STORE, tmpVarMv));

        // compile nth-value
        ctx.addInst(Lists.list(Insts.LOAD, tmpVarMv));
        ctx.addInst(Lists.list(Insts.LOAD, tmpVarN));
        ctx.addInst(Lists.list(Insts.INVOKE, Classes.NTH_VALUE));
        compileTypeMapping(Object.class, false, env);

        // clean up...
        ctx.popVar();
        ctx.popVar();
    }
    static private void compileList(Object cdr, Env env) {
        Context ctx = Context.get(env);
        int nargs = Lists.length(cdr);
        // push arguments
        for (Object l = cdr; !Lists.isEnd(l); l = Lists.cdr(l)) {
            compileFormWithExpectedType
                (Lists.car(l), Object.class, false, env);
        }
        // make list
        compileList(nargs, env);
        // type handling
        compileTypeMapping(List.class, false, env);
    }
    static private void compileList(int nargs, Env env) {
        Context ctx = Context.get(env);
        compileFormWithExpectedType
            (Forms.NIL, Object.class, false, env);
        for (int i = 0; i < nargs; i++) {
            ctx.addInst(Lists.list(Insts.INVOKE, Classes.CONS));
        }
    }
    static private void compileValues(Object cdr, Env env) {
        Context ctx = Context.get(env);
        int nargs = Lists.length(cdr);
        Object argVarList = Symbols.NIL;
        Class type = Object.class;

        // args
        for (Object l = cdr; !Lists.isEnd(l); l = Lists.cdr(l)) {
            compileFormWithExpectedType
                (Lists.car(l), type, false, env);
            Object argVar = ctx.pushTempVar(type);
            ctx.addInst(Lists.list(Insts.STORE, argVar));
            argVarList = Lists.cons(argVar, argVarList);
        }
        // reverse argVarList
        argVarList = Lists.nreverse(argVarList);
        // push current mv
        ctx.addInst(Lists.list(Insts.INVOKE, Classes.CURRENT_MV));
        // push arguments
        for (Object l = argVarList; !Lists.isEnd(l); l = Lists.cdr(l)) {
            Object argVar = Lists.car(l);
            ctx.addInst(Lists.list(Insts.LOAD, argVar));
            ctx.addInst(Lists.list(Insts.INVOKE, Classes.PUSH_VALUE));
        }
        // type handling
        compileTypeMapping(Values.class, true, env);
        // clean up...
        for (int i = 0; i < nargs; i++) {
            ctx.popVar();
        }
    }
    static private void compileLambda(Object car, Env env) {
        // create expr
        Object params = Lists.cadr(car);
        Object body = Lists.cddr(car);
        Expr expr = new Expr("EXPR", params, body, env);
        compileExpr(expr, env);
    }
    static private void compileExpr(Expr expr, Env env) {
        // retrieve current context
        Context ctx = Context.get(env);
        {
            // make child env
            Env newenv = env.child();
            // make child context
            Context newctx = ctx.child(expr, newenv);
            // bind the child context to the child env
            Context.bind(newctx, newenv);
            // start compile
            Compiler.doPass1(newenv);
            Compiler.doPass2(newenv);
        }
    }
    static private void compileLambdaAsBlock(Object car, Object cdr, Env env) {
        // create expr
        Object params = Lists.cadr(car);
        Object body = Lists.cddr(car);
        Expr expr = new Expr("EXPR", params, body, env);
        compileExprAsBlock(expr, cdr, env);
    }
    static private void compileExprAsBlock(Expr expr, Object cdr, Env env) {
        Context ctx = Context.get(env);
        LambdaList ll = expr.lambdaList();
        if (ll.isWhole() || ll.isNested())
            compileExprAsBlock1(expr, cdr, env);
        else
            compileExprAsBlock2(expr, cdr, env);
    }
    static private void compileExprAsBlock1(Expr expr, Object cdr, Env env) {
        Context ctx = Context.get(env);
        LambdaList ll = expr.lambdaList();
        Object decls = expr.decls();
        Object body = expr.body();
        Symbol tagStart = ctx.genTag("EXPR-BLOCK-START");
        Symbol tagDone = ctx.genTag("EXPR-BLOCK-DONE");
        int nargs = Lists.length(cdr);

        // args
        for (Object l = cdr; !Lists.isEnd(l); l = Lists.cdr(l)) {
            compileFormWithExpectedType
                (Lists.car(l), Object.class, false, env);
        }
        Object argsVar = ctx.pushTempVar(Object.class);
        compileList(nargs, env);
        ctx.addInst(Lists.list(Insts.STORE, argsVar));

        // prepare new block
        ctx.pushBlock(Context.BlockInfo.LAMBDA,
                      tagStart, tagDone, decls, ll.vars(), env);

        // bind args
        compileBind1(ll, argsVar, env);

        // compile body
        compileProgn(body, env);

        // clean up...
        ctx.popBlock();
        ctx.popVar();
    }
    static private void compileExprAsBlock2(Expr expr, Object cdr, Env env) {
        Context ctx = Context.get(env);
        LambdaList ll = expr.lambdaList();
        Object decls = expr.decls();
        Object body = expr.body();
        Symbol tagStart = ctx.genTag("EXPR-BLOCK-START");
        Symbol tagDone = ctx.genTag("EXPR-BLOCK-DONE");
        int nargs = Lists.length(cdr);
        int min = ll.reqCount();
        int max = ll.reqCount() + ll.optCount();

        if (nargs < min) {
            throw new ProgramException
                ("too few args specified: ~S, expected: ~S",
                 Lists.list(cdr, ll.params()));
        }

        if (max < nargs && !ll.isRest() && ll.keyCount() <= 0) {
            throw new ProgramException
                ("too many args specified: ~S, expected: ~S",
                 Lists.list(cdr, ll.params()));
        }

        Object argVarList = Symbols.NIL;
        {
            // XXX: to calc type of var, decls in expr is required.
            DeclInfo di = ctx.getDecl().child().pushDecls(decls);
            int i = 0;
            for (Object l = cdr; !Lists.isEnd(l); l = Lists.cdr(l)) {
                Object arg = Lists.car(l);
                if (i < max) {
                    Symbol var = Data.symbol(ll.var(i));
                    Class type = di.javaTypeOfVar(var, true);
                    Object argVar = ctx.pushTempVar(type);
                    compileFormWithExpectedType(arg, type, false, env);
                    ctx.addInst(Lists.list(Insts.STORE, argVar));
                    argVarList = Lists.cons(argVar, argVarList);
                }
                else { // max <= i
                    Class type = Object.class;
                    compileFormWithExpectedType(arg, type, false, env);
                }
                i++;
            }
        }

        // reverse argVarList
        argVarList = Lists.nreverse(argVarList);

        Object restVar = (ll.isRest() || ll.keyCount() > 0)
            ? ctx.pushTempVar(Object.class) : Symbols.NIL;
        if (restVar != Symbols.NIL) {
            if (max < nargs) {
                compileList(nargs - max, env);
            }
            else {
                compileFormWithExpectedType
                    (Forms.NIL, Object.class, false, env);
            }
            ctx.addInst(Lists.list(Insts.STORE, restVar));
        }

        // prepare new block
        ctx.pushBlock(Context.BlockInfo.LAMBDA,
                      tagStart, tagDone, decls, ll.vars(), env);

        // bind args
        compileBind2(ll, argVarList, restVar, env);

        // compile body
        compileProgn(body, env);

        // clean up...
        ctx.popBlock();
        if (restVar != Symbols.NIL) {
            ctx.popVar();
        }
        for (int i = 0, j = Math.min(max, nargs); i < j; i++) {
            ctx.popVar();
        }
    }
    static private void compileCall_FSubr(Symbol sym, Object cdr, Env env) {
        Context ctx = Context.get(env);
        Object envVar = ctx.getEnvVar();
        ctx.addInst(Lists.list(Insts.CONST, sym));
        ctx.addInst(Lists.list(Insts.LOAD, envVar));
        ctx.addInst(Lists.list(Insts.INVOKE, Classes.FUNCTION));
        // fsubr must be 1 argument function
        ctx.addInst(Lists.list(Insts.CONST, cdr));
        // do funcall
        compileFuncall(1, env);
    }
    static private void compileCall_FExpr(Symbol sym, Object cdr, Env env) {
        Context ctx = Context.get(env);
        Object envVar = ctx.getEnvVar();
        ctx.addInst(Lists.list(Insts.CONST, sym));
        ctx.addInst(Lists.list(Insts.LOAD, envVar));
        ctx.addInst(Lists.list(Insts.INVOKE, Classes.FUNCTION));
        // fexpr must be 1 argument function
        ctx.addInst(Lists.list(Insts.CONST, cdr));
        // do funcall
        compileFuncall(1, env);
    }
    static private void compileCall_Default(Symbol sym, Object func,
                                            Object cdr, Env env)
        throws java.lang.Exception {
        Context ctx = Context.get(env);
        int nargs = Lists.length(cdr);

        // analyzes the function object
        // and collects compilation informations.
        Values mv = Subrs.analyzeSubr(sym, func, nargs, ctx.group(), env);
        // how-to-compile-this-function-call
        Object howToCompile = mv.nth(0);
        // target-method
        Object tm = mv.nth(1);
        // subr-field
        Object sf = mv.nth(2);
        // callable-method
        Object cm = mv.nth(3);
        // can-produce-values-p
        Object cpv = mv.nth(4);
        // context-of-compiled-expr
        Object ce = mv.nth(5);

        if (howToCompile == Subrs.USE_TARGET_METHOD) {
            /* call static method returned by
               SystemSubr#getTargetMethod(Object) */
            compileCall_TargetMethod(Data.javaMethod(tm),
                                     Data.toBoolean(cpv),
                                     cdr, env);
        }
        else if (howToCompile == Subrs.USE_SUBR_FIELD) {
            /* get function by accessing a static field XxxSubrs.YYY */
            compileCall_Callable(Data.javaField(sf),
                                 Data.javaMethod(cm),
                                 Data.toBoolean(cpv),
                                 cdr, env);
        }
        else if (howToCompile == Subrs.USE_CONTEXT) {
            Context ctx2 = (Context) ce;
            if (ctx == ctx2) {
                /* requested function is myself */
                Object tmpVar = ctx.getVar(Callables.LOCAL_SLOT_THIS, env);
                Context.MethodInfo mi = ctx2.findMethod(nargs, false, env);
                // push function
                ctx.addInst(Lists.list(Insts.LOAD, tmpVar));
                // do funcall
                compileCall_Direct(mi, cdr, env);
            }
            else {
                /* get function by accessing a static field ClassName.SELF */
                String classname = ctx2.classname();
                Context.MethodInfo mi = ctx2.findMethod(nargs, false, env);
                // push function
                ctx.addInst(Lists.list(Insts.COMPILED_EXPR, classname));
                // do funcall
                compileCall_Direct(mi, cdr, env);
            }
        }
        else {
            /* resolve function by calling (FUNCTION sym) at runtime */
            // push function
            Object envVar = ctx.getEnvVar();
            ctx.addInst(Lists.list(Insts.CONST, sym));
            ctx.addInst(Lists.list(Insts.LOAD, envVar));
            ctx.addInst(Lists.list(Insts.INVOKE, Classes.FUNCTION));
            // push arguments
            for (Object l = cdr; !Lists.isEnd(l); l = Lists.cdr(l)) {
                compileFormWithExpectedType
                    (Lists.car(l), Object.class, false, env);
            }
            // do funcall
            compileFuncall(nargs, env);
        }
    }
    static private void compileFuncall(int nargs, Env env) {
        Context ctx = Context.get(env);
        Object envVar = ctx.getEnvVar();
        // push env and execute funcall
        if (Callables.getMaxArgCount() < nargs) {
            /* invoke Evaluator.funcall(args, env) */
            compileList(nargs, env);
            ctx.addInst(Lists.list(Insts.LOAD, envVar));
            ctx.addInst(Lists.list(Insts.CALL, Data.toFixnum(-1)));
        }
        else {
            /* invoke Evaluator.funcall[nargs](arg0,...,env) */
            ctx.addInst(Lists.list(Insts.LOAD, envVar));
            ctx.addInst(Lists.list(Insts.CALL, Data.toFixnum(nargs)));
        }
        // type handling
        compileTypeMapping(Object.class, true, env);
    }
    static private void compileCall_TargetMethod
        (Method tm, boolean cpv, Object cdr, Env env) {
        Context ctx = Context.get(env);
        int pCount = 0;
        Class[] pTypes = tm.getParameterTypes();
        Class rType = tm.getReturnType();
        if (!Classes.isStatic(tm)) {
            /* For nonstatic method, declaring class of
               the method (type of reciever object)
               is inserted at the head of pTypes. */
            Class[] pTypes2 = new Class[pTypes.length+1];
            pTypes2[0] = tm.getDeclaringClass();
            System.arraycopy(pTypes, 0, pTypes2, 1, pTypes.length);
            pTypes = pTypes2;
        }
        // push arguments
        for (Object l = cdr; !Lists.isEnd(l); l = Lists.cdr(l)) {
            Object arg = Lists.car(l);
            compileFormWithExpectedType
                (arg, pTypes[pCount++], false, env);
        }
        // do funcall
        ctx.addInst(Lists.list(Insts.INVOKE, tm));
        // type handling
        compileTypeMapping(rType, cpv, env);
    }
    static private void compileCall_Callable
        (Field f, Method m, boolean cpv, Object cdr, Env env) {
        Context ctx = Context.get(env);
        Object envVar = ctx.getEnvVar();
        int nargs = Lists.length(cdr);
        Values mv = Callables.getTypeInfo(m.getDeclaringClass());
        Object v0 = mv.nth(0);
        Object v1 = mv.nth(1);
        int n = Data.fixnum(v0).intValue();
        boolean r = Data.toBoolean(v1);
        // push function
        ctx.addInst(Lists.list(Insts.GET, f));
        ctx.addInst(Lists.list(Insts.CHECKCAST, m.getDeclaringClass()));
        // push arguments
        for (Object l = cdr; !Lists.isEnd(l); l = Lists.cdr(l)) {
            compileFormWithExpectedType
                (Lists.car(l), Object.class, false, env);
        }
        if (n == -1) {
            /*
             * declaring class of m is lapin.function.Callable
             * -> invoke Callable.call(args, env)
             */
            compileList(nargs, env);
            ctx.addInst(Lists.list(Insts.LOAD, envVar));
            ctx.addInst(Lists.list(Insts.INVOKE, m));
        }
        else if (r == true) {
            /*
             * declaring class of m is lapin.function.Callable[n]r
             * -> invoke Callable[n]r.call(arg0,...,rest,env)
             */
            compileList(nargs - n, env);
            ctx.addInst(Lists.list(Insts.LOAD, envVar));
            ctx.addInst(Lists.list(Insts.INVOKE, m));
        }
        else {
            /*
             * declaring class of m is lapin.function.Callable[nargs]
             * -> invoke Callable[nargs].call[nargs](arg0,...,env)
             */
            ctx.addInst(Lists.list(Insts.LOAD, envVar));
            ctx.addInst(Lists.list(Insts.INVOKE, m));
        }
        // type handling
        compileTypeMapping(Object.class, cpv, env);
    }
    static private void compileCall_Direct
        (Context.MethodInfo mi, Object cdr, Env env) {
        Context ctx = Context.get(env);
        Object envVar = ctx.getEnvVar();
        int nargs = Lists.length(cdr);
        Class[] paramTypes = mi.paramTypes();
        Class retType = mi.retType();
        boolean cpv = retType.isAssignableFrom(Values.class);
        // push arguments
        {
            int i = 0;
            for (Object l = cdr; !Lists.isEnd(l); l = Lists.cdr(l)) {
                Object form = Lists.car(l);
                Class paramType = (i < mi.nargs())
                    ? paramTypes[i] : Object.class;
                compileFormWithExpectedType
                    (form, paramType, false, env);
            }
        }
        if (mi.nargs() == -1) {
            /* invoke classname._call(args, env) */
            compileList(nargs, env);
            ctx.addInst(Lists.list(Insts.LOAD, envVar));
            ctx.addInst(Lists.list(Insts.CALL_DIRECT, mi));
        }
        else if (mi.rest() == true) {
            /* invoke classname._call[max](arg0,..., rest, env) */
            compileList(nargs - mi.nargs(), env);
            ctx.addInst(Lists.list(Insts.LOAD, envVar));
            ctx.addInst(Lists.list(Insts.CALL_DIRECT, mi));
        }
        else {
            /* invoke classname._call[nargs](arg0,..., env) */
            ctx.addInst(Lists.list(Insts.LOAD, envVar));
            ctx.addInst(Lists.list(Insts.CALL_DIRECT, mi));
        }
        // type handling
        compileTypeMapping(retType, cpv, env);
    }

    static private void compileFormWithExpectedType
        (Object exp, Class typeExpected, boolean canConsumeValues, Env env) {
        if (Logger.tracelevelp(env)) {
            Logger.trace("[comp:form-with-type] exp =~S",
                         Lists.list(exp), env);
            Logger.trace("[comp:form-with-type] type=~S",
                         Lists.list(typeExpected), env);
            Logger.trace("[comp:form-with-type] consume=~S",
                         Lists.list(Data.toPredicate(canConsumeValues)),
                         env);
        }
        Context ctx = Context.get(env);
        ctx.pushType(exp, typeExpected, canConsumeValues);
        compileForm(exp, env);
        if (Logger.tracelevelp(env)) {
            int n = ctx.peekType().count;
            if (n != 1) {
                Logger.trace
                    ("[comp:form-with-type] count is ~A: ~S",
                     Lists.list(Data.toFixnum(n), exp), env);
            }
        }
        ctx.popType();
    }
    static private void compileTypeMapping
        (Class typeActual, boolean canProduceValues, Env env) {
        if (Logger.tracelevelp(env)) {
            Logger.trace("[comp:actual-type] type=~S",
                         Lists.list(typeActual), env);
            Logger.trace("[comp:actual-type] produce=~S",
                         Lists.list(Data.toPredicate(canProduceValues)),
                         env);
        }
        Context ctx = Context.get(env);
        Context.TypeInfo ti = ctx.peekType();
        ti.count++;
        Object exp = ti.exp;
        Class typeExpected = ti.expected;
        boolean canConsumeValues = ti.canConsumeValues;
        compileTypeMapping(exp, typeExpected, typeActual,
                           canConsumeValues, canProduceValues, env);
    }
    static private void compileTypeMapping
        (Object exp, Class typeExpected, Class typeActual,
         boolean canConsumeValues, boolean canProduceValues, Env env) {
        Context ctx = Context.get(env);
        if (Logger.tracelevelp(env)) {
            Logger.trace("[comp:type] exp     =~S",
                         Lists.list(exp), env);
            Logger.trace("[comp:type] expected=~S",
                         Lists.list(typeExpected), env);
            Logger.trace("[comp:type] actual  =~S",
                         Lists.list(typeActual), env);
            Logger.trace("[comp:type] consume =~S",
                         Lists.list(Data.toPredicate(canConsumeValues)),
                         env);
            Logger.trace("[comp:type] produce =~S",
                         Lists.list(Data.toPredicate(canProduceValues)),
                         env);
        }
        /*
         * S: typeActual (Source type)
         * T: typeExpected (Target type)
         *
         * void: NOT SUPPORTED
         * boolean: SPECIALLY TREATED
         *  (used as return value of PREDICATE)
         * int: supported (used as FIXNUM)
         * double: supported (used as FLONUM)
         * char: supported
         * other primitive: NOT SUPPORTED
         * java.lang.Void: NOT SUPPORTED
         * java.lang.Boolean: UNDEFINED
         * java.lang.Integer: supported (used as FIXNUM)
         * java.lang.Double: supported (used as FLONUM)
         * Character: supported
         * other primitive wrapper: UNDEFINED
         * array: NOT SUPPORTED
         * interface: supported
         * lapin.lang.Values: SPECIALLY TREATED
         *  (used as register for MULTIPLE-VALUE)
         * java.math.BigInteger: supported (used as BIGNUM)
         * class: supported
         */
        if (typeExpected == null) {
            /*
             * typeExpected: null
             * => return value must be discarded
             */
            if (typeActual.equals(void.class)) {
                // S: void
                // T: null
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            else if (typeActual.equals(boolean.class) ||
                     typeActual.equals(int.class) ||
                     typeActual.equals(double.class) ||
                     typeActual.equals(char.class)) {
                // S: boolean or int or double or char
                // T: null
                ctx.addInst(Lists.list(Insts.POP, typeActual));
            }
            else if (typeActual.isPrimitive()) {
                // S: primitive
                // T: null
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            else if (typeActual.isArray()) {
                // S: array
                // T: null
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            else if (typeActual.isInterface()) {
                // S: interface
                // T: null
                ctx.addInst(Lists.list(Insts.POP, typeActual));
            }
            else if (typeActual.equals(Void.class)) {
                // S: Void
                // T: null
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            //else if (typeActual.equals(Boolean.class)) {
            //    // S: Boolean
            //    // T: null
            //    throw new UnexpectedDataTypeException
            //        (exp, typeExpected, typeActual);
            //}
            else {
                // S: class
                // T: null
                ctx.addInst(Lists.list(Insts.POP, typeActual));
            }
        }
        else if (typeExpected.equals(void.class)) {
            /*
             * typeExpected: void
             */
            throw new UnsupportedDataTypeException(exp, typeExpected);
        }
        else if (typeExpected.equals(boolean.class)) {
            /*
             * typeExpected: boolean
             */
            if (typeActual.equals(void.class)) {
                // S: void
                // T: boolean
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            else if (typeActual.equals(boolean.class)) {
                // S: boolean
                // T: boolean
                // => no cast/coerse required
            }
            else if (typeActual.equals(int.class) ||
                     typeActual.equals(double.class) ||
                     typeActual.equals(char.class)) {
                // S: int or double or char
                // T: boolean
                ctx.addInst(Lists.list(Insts.POP, typeActual));
                ctx.addInst(Lists.list(Insts.PUSH, Boolean.TRUE));
            }
            else if (typeActual.isPrimitive()) {
                // S: primitive
                // T: boolean
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            else if (typeActual.isArray()) {
                // S: array
                // T: boolean
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            else if (typeActual.isInterface()) {
                // S: interface
                // T: boolean
                ctx.addInst(Lists.list(Insts.INVOKE, Classes.OBJ_TO_BOOL));
            }
            else if (typeActual.equals(Void.class)) {
                // S: Void
                // T: boolean
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            //else if (typeActual.equals(Boolean.class)) {
            //    // S: Boolean
            //    // T: boolean
            //    throw new UnexpectedDataTypeException
            //        (exp, typeExpected, typeActual);
            //}
            else {
                // S: class
                // T: boolean
                if (canProduceValues/* && !canConsumeValues*/) {
                    // S: (maybe) Values
                    // T: boolean
                    ctx.addInst(Lists.list(Insts.INVOKE, Classes.SINGLE_VALUE));
                    ctx.addInst(Lists.list(Insts.INVOKE, Classes.OBJ_TO_BOOL));
                }
                else {
                    // S: class
                    // T: boolean
                    ctx.addInst(Lists.list(Insts.INVOKE, Classes.OBJ_TO_BOOL));
                }
            }
        }
        else if (typeExpected.equals(int.class)) {
            /*
             * typeExpected: int
             */
            if (typeActual.equals(void.class)) {
                // S: void
                // T: int
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            else if (typeActual.equals(int.class)) {
                // S: int
                // T: int
                // => no cast/coerse required
            }
            else if (typeActual.isPrimitive()) {
                // S: primitive
                // T: int
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            else if (typeActual.isArray()) {
                // S: array
                // T: int
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            else if (typeActual.isInterface()) {
                // S: interface
                // T: int
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            else if (typeActual.equals(Void.class)) {
                // S: Void
                // T: int
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            //else if (typeActual.equals(Boolean.class)) {
            //    // S: Boolean
            //    // T: int
            //    throw new UnexpectedDataTypeException
            //        (exp, typeExpected, typeActual);
            //}
            else {
                // S: class
                // T: int
                if (canProduceValues/* && !canConsumeValues*/) {
                    // S: (maybe) Values
                    // T: int
                    ctx.addInst(Lists.list(Insts.INVOKE, Classes.SINGLE_VALUE));
                    ctx.addInst(Lists.list(Insts.INVOKE, Classes.OBJ_TO_INT));
                }
                else if (typeActual.isAssignableFrom(Integer.class)) {
                    // S: class in [Integer, Object]
                    // T: int
                    ctx.addInst(Lists.list(Insts.INVOKE, Classes.OBJ_TO_INT));
                }
                else {
                    // S: class
                    // T: int
                    throw new UnexpectedDataTypeException
                        (exp, typeExpected, typeActual);
                }
            }
        }
        else if (typeExpected.equals(double.class)) {
            /*
             * typeExpected: double
             */
            if (typeActual.equals(void.class)) {
                // S: void
                // T: double
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            else if (typeActual.equals(double.class)) {
                // S: double
                // T: double
                // => no cast/coerse required
            }
            else if (typeActual.isPrimitive()) {
                // S: primitive
                // T: double
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            else if (typeActual.isArray()) {
                // S: array
                // T: double
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            else if (typeActual.isInterface()) {
                // S: interface
                // T: double
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            else if (typeActual.equals(Void.class)) {
                // S: Void
                // T: double
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            //else if (typeActual.equals(Boolean.class)) {
            //    // S: Boolean
            //    // T: double
            //    throw new UnexpectedDataTypeException
            //        (exp, typeExpected, typeActual);
            //}
            else {
                // S: class
                // T: double
                if (canProduceValues/* && !canConsumeValues*/) {
                    // S: (maybe) Values
                    // T: double
                    ctx.addInst(Lists.list(Insts.INVOKE, Classes.SINGLE_VALUE));
                    ctx.addInst(Lists.list(Insts.INVOKE, Classes.OBJ_TO_DOUBLE));
                }
                else if (typeActual.isAssignableFrom(Double.class)) {
                    // S: class in [Double, Object]
                    // T: double
                    ctx.addInst(Lists.list(Insts.INVOKE, Classes.OBJ_TO_DOUBLE));
                }
                else {
                    // S: class
                    // T: double
                    throw new UnexpectedDataTypeException
                        (exp, typeExpected, typeActual);
                }
            }
        }
        else if (typeExpected.equals(char.class)) {
            /*
             * typeExpected: char
             */
            if (typeActual.equals(void.class)) {
                // S: void
                // T: char
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            else if (typeActual.equals(char.class)) {
                // S: char
                // T: char
                // => no cast/coerse required
            }
            else if (typeActual.isPrimitive()) {
                // S: primitive
                // T: char
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            else if (typeActual.isArray()) {
                // S: array
                // T: char
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            else if (typeActual.isInterface()) {
                // S: interface
                // T: char
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            else if (typeActual.equals(Void.class)) {
                // S: Void
                // T: char
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            //else if (typeActual.equals(Boolean.class)) {
            //    // S: Boolean
            //    // T: char
            //    throw new UnexpectedDataTypeException
            //        (exp, typeExpected, typeActual);
            //}
            else {
                // S: class
                // T: char
                if (canProduceValues/* && !canConsumeValues*/) {
                    // S: (maybe) Values
                    // T: char
                    ctx.addInst(Lists.list(Insts.INVOKE, Classes.SINGLE_VALUE));
                    ctx.addInst(Lists.list(Insts.INVOKE, Classes.OBJ_TO_CHAR));
                }
                else if (typeActual.isAssignableFrom(Character.class)) {
                    // S: class in [Character, Object]
                    // T: char
                    ctx.addInst(Lists.list(Insts.INVOKE, Classes.OBJ_TO_CHAR));
                }
                else {
                    // S: class
                    // T: char
                    throw new UnexpectedDataTypeException
                        (exp, typeExpected, typeActual);
                }
            }
        }
        else if (typeExpected.isPrimitive()) {
            /*
             * typeExpected: primitive
             * (other than boolean, int, double, char)
             */
            throw new UnsupportedDataTypeException(exp, typeExpected);
        }
        else if (typeExpected.isArray()) {
            /*
             * typeExpected: array
             */
            throw new UnsupportedDataTypeException(exp, typeExpected);
        }
        else if (typeExpected.isInterface()) {
            /*
             * typeExpected: interface
             */
            if (typeActual.equals(void.class)) {
                // S: void
                // T: interface
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            else if (typeActual.isPrimitive()) {
                // S: primitive
                // T: interface
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            else if (typeActual.isArray()) {
                // S: array
                // T: interface
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            else if (typeActual.isInterface()) {
                // S: interface
                // T: interface
                if (typeExpected.isAssignableFrom(typeActual)) {
                    // S: interface which is a same or subinterface of T
                    // T: interface
                    // => no cast/coerse required
                }
                else if (typeActual.isAssignableFrom(typeExpected)) {
                    // S: interface which is a superinterface of T
                    // T: interface
                    ctx.addInst(Lists.list(Insts.CHECKCAST, typeExpected));
                }
                else {
                    // S: interface
                    // T: interface
                    // XXX: method signature check is omitted
                    throw new UnexpectedDataTypeException
                        (exp, typeExpected, typeActual);
                }
            }
            else if (typeActual.equals(Void.class)) {
                // S: Void
                // T: interface
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            //else if (typeActual.equals(Boolean.class)) {
            //    // S: Boolean
            //    // T: interface
            //    throw new UnexpectedDataTypeException
            //        (exp, typeExpected, typeActual);
            //}
            else {
                // S: class
                // T: interface
                if (canProduceValues/* && !canConsumeValues*/) {
                    // S: (maybe) Values
                    // T: interface
                    ctx.addInst(Lists.list(Insts.INVOKE, Classes.SINGLE_VALUE));
                    ctx.addInst(Lists.list(Insts.CHECKCAST, typeExpected));
                }
                else if (typeExpected.isAssignableFrom(typeActual)) {
                    // S: class which implements T
                    // T: interface
                    // => no cast/coerse required
                }
                else if (!Classes.isFinal(typeActual)) {
                    // S: class whose subclass may implement T
                    // T: interface
                    ctx.addInst(Lists.list(Insts.CHECKCAST, typeExpected));
                }
                else {
                    // S: class
                    // T: interface
                    throw new UnexpectedDataTypeException
                        (exp, typeExpected, typeActual);
                }
            }
        }
        else if (typeExpected.equals(Void.class)) {
            /*
             * typeExpected: Void
             */
            throw new UnsupportedDataTypeException(exp, typeExpected);
        }
        //else if (typeExpected.equals(Boolean.class)) {
        //    /*
        //     * typeExpected: Boolean
        //     */
        //    throw new UnsupportedDataTypeException(exp, typeExpected);
        //}
        else if (typeExpected.equals(Values.class)) {
            /*
             * typeExpected: Values
             */
            throw new UnsupportedDataTypeException(exp, typeExpected);
        }
        else {
            /*
             * typeExpected: class
             */
            if (typeActual.equals(void.class)) {
                // S: void
                // T: class
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            else if (typeExpected.equals(Object.class) &&
                     typeActual.equals(boolean.class)) {
                // S: boolean
                // T: Object
                ctx.addInst(Lists.list(Insts.INVOKE, Classes.BOOL_TO_OBJ));
            }
            else if (typeExpected.isAssignableFrom(Integer.class) &&
                     typeActual.equals(int.class)) {
                // S: int
                // T: class in [Integer, Object]
                ctx.addInst(Lists.list(Insts.INVOKE, Classes.INT_TO_OBJ));
            }
            else if (typeExpected.isAssignableFrom(Double.class) &&
                     typeActual.equals(double.class)) {
                // S: double
                // T: class in [Double, Object]
                ctx.addInst(Lists.list(Insts.INVOKE, Classes.DOUBLE_TO_OBJ));
            }
            else if (typeExpected.isAssignableFrom(Character.class) &&
                     typeActual.equals(char.class)) {
                // S: char
                // T: class in [Character, Object]
                ctx.addInst(Lists.list(Insts.INVOKE, Classes.CHAR_TO_OBJ));
            }
            else if (typeActual.isPrimitive()) {
                // S: primitive
                // T: class
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            else if (typeActual.isArray()) {
                // S: array
                // T: class
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            else if (typeActual.isInterface()) {
                // S: interface
                // T: class
                if (!Classes.isFinal(typeExpected)) {
                    // S: interface which may be implemented by a subclass of T
                    // T: class
                    ctx.addInst(Lists.list(Insts.CHECKCAST, typeExpected));
                }
                else if (typeActual.isAssignableFrom(typeExpected)) {
                    // S: interface which is implemented by T
                    // T: class
                    ctx.addInst(Lists.list(Insts.CHECKCAST, typeExpected));
                }
                else {
                    // S: interface
                    // T: class
                    throw new UnexpectedDataTypeException
                        (exp, typeExpected, typeActual);
                }
            }
            else if (typeActual.equals(Void.class)) {
                // S: Void
                // T: class
                throw new UnexpectedDataTypeException
                    (exp, typeExpected, typeActual);
            }
            //else if (typeActual.equals(Boolean.class)) {
            //    // S: Boolean
            //    // T: class
            //    throw new UnexpectedDataTypeException
            //        (exp, typeExpected, typeActual);
            //}
            else {
                // S: class
                // T: class
                if (canProduceValues && !canConsumeValues) {
                    // S: (maybe) Values
                    // T: class
                    ctx.addInst(Lists.list(Insts.INVOKE, Classes.SINGLE_VALUE));
                    if (typeExpected.equals(Object.class)) {
                        // S: (maybe) Values
                        // T: Object
                        // => no cast/coerse required
                    }
                    else {
                        // S: (maybe) Values
                        // T: class which is a subclass of Object
                        ctx.addInst(Lists.list(Insts.CHECKCAST, typeExpected));
                    }
                }
                else if (typeExpected.isAssignableFrom(typeActual)) {
                    // S: class which is a same or subclass of T
                    // T: class
                    // => no cast/coerse required
                }
                else if (typeActual.isAssignableFrom(typeExpected)) {
                    // S: class which is a superclass of T
                    // T: class
                    ctx.addInst(Lists.list(Insts.CHECKCAST, typeExpected));
                }
                else {
                    // S: class
                    // T: class
                    throw new UnexpectedDataTypeException
                        (exp, typeExpected, typeActual);
                }
            }
        }
    }
    static void optimize(Env env) {
        /*
         * exec simple peephole optimization
         */
        Context ctx = Context.get(env);
        if (!ctx.group().optimize) {
            return;
        }
        int round = 1;
        while (true) {
            if (Logger.debuglevelp(env))
                Logger.debug("[optimize] round: "+round, env);
            boolean modified1 = _doOptimize1(env);
            boolean modified2 = _doOptimize2(env);
            if (modified1 || modified2)
                round++; // goto next round
            else
                break; // nothing changed, break main loop

        }/* end of WHILE loop */
    }
    static private boolean _doOptimize1(Env env) {
        /*
         * [doOptimize1]
         * Removes unused labels.
         */
        Context ctx = Context.get(env);
        boolean debuglevelp = Logger.debuglevelp(env);
        boolean tracelevelp = Logger.tracelevelp(env);
        boolean modified = false;
        /*
         * labelMap: (label info)
         * info: (pos . list)
         * pos : index of the label
         * list: (pos1 pos2 ... posk)
         * posK: index of jump instruction which points to the label
         */
        HashMap labelMap = new HashMap();
        for (int i = 0; i < ctx.instLen(); i++) {
            Object inst = ctx.getInst(i);
            if (Data.isSymbol(inst)) {
                if (tracelevelp)
                    Logger.trace
                        ("[optimize1] ~A:\tlabel: ~S",
                         Lists.list(Data.toFixnum(i), inst),
                         env);
                Symbol label = Data.symbol(inst);
                Object info = labelMap.get(label);
                if (info == null) {
                    info = Lists.cons(Symbols.NIL, Symbols.NIL);
                    labelMap.put(label, info);
                }
                if (Lists.car(info) == Symbols.NIL) {
                    Lists.rplaca(info, Data.toFixnum(i));
                }
                if (!Data.isEqual(Lists.car(info), Data.toFixnum(i))) {
                    throw new NotReachedException
                        ("duplicated label detected at ~A: ~S.",
                         Lists.list(Data.toFixnum(i), label));
                }
                continue;
            }
            Object id = Lists.car(inst);
            if (id == Insts.GOTO ||
                id == Insts.IFEQ ||
                id == Insts.IFNE) {
                if (tracelevelp)
                    Logger.trace
                        ("[optimize1] ~A:\tjump: ~S",
                         Lists.list(Data.toFixnum(i), inst),
                         env);
                Symbol label = Data.symbol(Lists.cadr(inst));
                Object info = labelMap.get(label);
                if (info == null) {
                    info = Lists.cons(Symbols.NIL, Symbols.NIL);
                    labelMap.put(label, info);
                }
                Object lst = Lists.cdr(info);
                Lists.rplacd(info, Lists.cons(Data.toFixnum(i), lst));
            }
        }
        for (Iterator it = labelMap.keySet().iterator(); it.hasNext();) {
            Symbol label = Data.symbol(it.next());
            Object info = labelMap.get(label);
            if (debuglevelp)
                Logger.debug
                    ("[optimize1] ~S: info=~S",
                     Lists.list(label, info),
                     env);
            Object pos = Lists.car(info);
            Object lst = Lists.cdr(info);
            if (lst == Symbols.NIL) {
                if (debuglevelp)
                    Logger.debug
                        ("[optimize1] ~A:\tremove unused label: ~S",
                         Lists.list(pos, label),
                         env);
                for (int i = 0; i < ctx.instLen(); i++) {
                    Object inst = ctx.getInst(i);
                    if (label == inst)
                        ctx.removeInst(i);
                }
                modified = true;
            }
        }
        return modified;
    }
    static private boolean _doOptimize2(Env env) {
        /*
         * [doOptimize2]
         * Removes instructions between goto and label.
         * ...
         * (goto L1)    ; remove if Lx == L1
         * (some-inst1) ; remove
         * (some-inst2) ; remove
         * ...
         * (some-instN) ; remove
         * Lx
         * ...
         */
        Context ctx = Context.get(env);
        boolean debuglevelp = Logger.debuglevelp(env);
        boolean tracelevelp = Logger.tracelevelp(env);
        boolean modified = false;
        int lastGoto = -1;
        int len = ctx.instLen();
        for (int i = 0; i < len; i++) {
            Object inst = ctx.getInst(i);
            if (Data.isSymbol(inst)) {
                if (lastGoto >= 0) {
                    if (lastGoto == i-1 &&
                        inst == Lists.cadr(ctx.getInst(lastGoto))) {
                        /*
                         * remove lastGoto
                         * ... (go L) L ... => ... L ...
                         */
                        if (debuglevelp)
                            Logger.debug
                                ("[optimize2] ~A:\tremove inst: ~S",
                                 Lists.list(Data.toFixnum(i),
                                            ctx.getInst(lastGoto)),
                                 env);
                        ctx.removeInst(lastGoto);
                        i--; len--; modified = true;
                    }
                    if (tracelevelp)
                        Logger.trace
                            ("[optimize2] ~A:\tOUT goto: ~S",
                             Lists.list(Data.toFixnum(i),
                                        ctx.getInst(lastGoto)), env);
                    lastGoto = -1;
                }
                continue;
            }
            Object id = Lists.car(inst);
            if (id == Insts.GOTO) {
                if (lastGoto >= 0) {
                    /*
                     * remove goto:
                     * ... (go L) ... (go L1) ... L => ... (go L) ... ... L
                     */
                    if (debuglevelp)
                        Logger.debug
                            ("[optimize2] ~A\tremove inst: ~S",
                             Lists.list(Data.toFixnum(i),
                                        ctx.getInst(i)), env);
                    ctx.removeInst(i);
                    i--; len-- ; modified = true;
                }
                else {
                    /*
                     * set lastGoto:
                     * ... (go L1) ... => set lastGoto = (go L1)
                     */
                    lastGoto = i;
                    if (tracelevelp)
                        Logger.trace
                            ("[optimize2] ~A:\tIN goto: ~S",
                             Lists.list(Data.toFixnum(i),
                                        ctx.getInst(lastGoto)), env);
                }
            }
            else if (id == Insts.CATCH ||
                     id == Insts.CATCH_FROM ||
                     id == Insts.CATCH_TO ||
                     id == Insts.CATCH_HANDLER) {
                /*
                 * clear lastGoto:
                 */
                if (lastGoto >= 0) {
                    if (tracelevelp)
                        Logger.trace
                            ("[optimize2] ~A:\t"+"OUT goto: ~S",
                             Lists.list(Data.toFixnum(i),
                                        ctx.getInst(lastGoto)), env);
                    lastGoto = -1;
                }
            }
            else {
                /*
                 * remove inst:
                 * ... (go L) ... XXX ... L => ... (go L) ... ... L
                 */
                if (lastGoto >= 0) {
                    if (debuglevelp)
                        Logger.debug
                            ("[optimize2] ~A:\tremove inst: ~S",
                             Lists.list(Data.toFixnum(i),
                                        ctx.getInst(i)), env);
                    ctx.removeInst(i);
                    i--; len--; modified = true;
                }
            }
        }
        return modified;
    }

    private InstsBuilder() {}
}
