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
import lapin.function.Callable;
import lapin.function.Callable0;
import lapin.function.Callable1;
import lapin.function.Callable2;
import lapin.function.Callable3;
import lapin.function.Callable4;
import lapin.function.Callable5;
import lapin.function.Callable0r;
import lapin.function.Callable1r;
import lapin.function.Callable2r;
import lapin.function.Callable3r;
import lapin.function.Callable4r;
import lapin.function.Callable5r;
import lapin.function.Function;
import lapin.function.Expr;
import lapin.function.LambdaList;
import lapin.function.Subr;
import lapin.function.UndefinedFunctionException;
import lapin.io.StreamException;
import lapin.lang.CellException;
import lapin.lang.ControlException;
import lapin.lang.Data;
import lapin.lang.Env;
import lapin.lang.Lists;
import lapin.lang.Plists;
import lapin.lang.ProgramException;
import lapin.lang.SimpleException;
import lapin.lang.Symbol;
import lapin.lang.Symbols;
import lapin.lang.TypeException;
import lapin.lang.Values;
import lapin.util.ListBuilder;
import lapin.util.TracableException;
//import lapin.util.Logger;

/**
 * Evaluator functions.
 */
public final class Evaluator {
    /** Exception thrown by the evaluator. */
    static public class Exception extends TracableException {
        Exception(String msg, Throwable cause) {
            super(msg, cause);
        }
    }

    /*
     * eval methods
     */

    /**
     * Evaluates the specified form.
     * @param exp form to be evaluated
     * @param env
     * @return Result of the evaluation, which may be the
     *         {@link Values multiple-value}.
     * @throws Evaluator.Exception
     */
    static public Object eval(Object exp, Env env) {
        try {
            return _eval(exp, env);
        } catch (NonLocalExit e) {
            throw e;
        } catch (Evaluator.Exception e) {
            throw e.push(exp);
        }
        catch (TracableException e) {
            throw e;
        } catch (java.lang.Exception e) {
            throw new Evaluator.Exception("eval error", e).push(exp);
        }
    }
    static private Object _eval(Object exp, Env env)
        throws java.lang.Exception {
        while (true) {
            // for symbols
            if (Data.isSymbol(exp)) {
                Symbol sym = Data.symbol(exp);
                if (sym.isSelfeval())
                    return sym;
                else
                    return env.get(sym);
            }
            // for other atoms
            else if (Data.isAtom(exp)) {
                return exp;
            }
            // for lists
            else if (Data.isList(exp)) {
                Object car = Lists.car(exp);
                Object cdr = Lists.cdr(exp);
                if (car == Symbols.IF) {
                    return evalIf(cdr, env);
                }
                else if (car == Symbols.AND) {
                    return evalAnd(cdr, env);
                }
                else if (car == Symbols.OR) {
                    return evalOr(cdr, env);
                }
                else if (car == Symbols.SETQ) {
                    return evalSetq(cdr, env);
                }
                else if (car == Symbols.PSETQ) {
                    return evalPsetq(cdr, env);
                }
                else if (car == Symbols.QUOTE) {
                    return evalQuote(cdr, env);
                }
                else if (car == Symbols.FUNCTION) {
                    return evalFunction(cdr, env);
                }
                else if (car == Symbols.PROGN) {
                    return evalProgn(cdr, env);
                }
                else if (car == Symbols.PROG) {
                    return evalProg(cdr, env);
                }
                else if (car == Symbols.RETURN) {
                    return evalReturn(cdr, env);
                }
                else if (car == Symbols.GO) {
                    return evalGo(cdr, env);
                }
                else if (car == Symbols.THROW) {
                    return evalThrow(cdr, env);
                }
                else if (car == Symbols.CATCH) {
                    return evalCatch(cdr, env);
                }
                else if (car == Symbols.UNWIND_PROTECT) {
                    return evalUnwindProtect(cdr, env);
                }
                else if (car == Symbols.EVAL_WHEN) {
                    return evalEvalWhen(cdr, env);
                }
                else if (car == Symbols.DECLARE) {
                    return evalDeclare(cdr, env);
                }
                else if (car == Symbols.MULTIPLE_VALUE_BIND) {
                    return evalMultipleValueBind(cdr, env);
                }
                else if (car == Symbols.MULTIPLE_VALUE_LIST) {
                    return evalMultipleValueList(cdr, env);
                }
                else if (car == Symbols.MULTIPLE_VALUE_SETQ) {
                    return evalMultipleValueSetq(cdr, env);
                }
                else if (car == Symbols.NTH_VALUE) {
                    return evalNthValue(cdr, env);
                }
                else if (Data.isSymbol(car)) {
                    Symbol sym = Data.symbol(car);
                    Object plist = env.lisp().functions(sym);
                    Object type = Lists.car(plist);
                    Object func = Lists.cadr(plist);
                    // SUBR: call built-in function
                    if (type == Symbols.SUBR) {
                        return evalSubr(Data.subr(func), cdr, env);
                    }
                    // EXPR: execute interpreted function
                    else if (type == Symbols.EXPR) {
                        return evalExpr(Data.expr(func), cdr, env);
                    }
                    // MACRO: call macro expansion function
                    else if (type == Symbols.MACRO) {
                        exp = evalMacro(Data.expr(func), exp, env);
                        continue;
                    }
                    // ARRAY: call array like built-in function
                    if (type == Symbols.ARRAY) {
                        return evalSubr(Data.subr(func), cdr, env);
                    }
                    // FSUBR: call special form
                    else if (type == Symbols.FSUBR) {
                        // fsubr must be 1 argument function
                        return evalFSubr(Data.subr(func), cdr, env);
                    }
                    // FEXPR: execute interpreted special form
                    else if (type == Symbols.FEXPR) {
                        // fexpr must be 1 argument function
                        return evalFexpr(Data.expr(func), cdr, env);
                    }
                    else {
                        throw new UndefinedFunctionException(sym);
                    }
                }
                else if (Data.isList(car)) {
                    Object caar = Lists.car(car);
                    // LAMBDA expression: create EXPR
                    // and execute interpreted function
                    if (caar == Symbols.LAMBDA) {
                        return evalLambda(car, cdr, env);
                    }
                    else {
                        throw new ProgramException
                            ("illegal function name: ~S.",
                             Lists.list(caar));
                    }
                }
                else {
                    throw new ProgramException
                        ("unknown exp: ~S.", Lists.list(exp));
                }
            }
            else {
                throw new ProgramException
                    ("unknown exp: ~S.", Lists.list(exp));
            }
        } /* end of while loop */
    }
    static private Object evalIf(Object cdr, Env env) {
        Object test = Lists.car(cdr);
        Object  con = Lists.cadr(cdr);
        Object  alt = Lists.caddr(cdr);
        if (!Data.isNot(Values.singleValue(eval(test, env))))
            return eval(con, env);
        else
            return eval(alt, env);
    }
    static private Object evalAnd(Object cdr, Env env) {
        for (Object l = cdr; !Lists.isEnd(l); l = Lists.cdr(l)) {
            Object obj = Lists.car(l);
            if (Lists.isEnd(Lists.cdr(l)))
                return eval(obj, env);
            Object ret = Values.singleValue(eval(obj, env));
            if (Data.isNot(ret))
                return ret;
        }
        return Symbols.T;
    }
    static private Object evalOr(Object cdr, Env env) {
        for (Object l = cdr; !Lists.isEnd(l); l = Lists.cdr(l)) {
            Object obj = Lists.car(l);
            if (Lists.isEnd(Lists.cdr(l)))
                return eval(obj, env);
            Object ret = Values.singleValue(eval(obj, env));
            if (!Data.isNot(ret))
                return ret;
        }
        return Symbols.NIL;
    }
    static private Object evalSetq(Object cdr, Env env) {
        int len = Lists.length(cdr);
        if (len%2 == 1)
            throw new ProgramException
                ("odd number of arguments: ~S.", Lists.list(cdr));
        Symbol var = Symbols.NIL;
        Object val = Symbols.NIL;
        for (Object l = cdr; !Lists.isEnd(l); l = Lists.cddr(l)) {
            var = Data.symbol(Lists.car(l));
            val = Values.singleValue(eval(Lists.cadr(l), env));
            env.set(var, val);
        }
        return val;
    }
    static private Object evalPsetq(Object cdr, Env env) {
        int len = Lists.length(cdr);
        if (len%2 == 1)
            throw new ProgramException
                ("odd number of arguments: ~S.", Lists.list(cdr));
        Object vars = Symbols.NIL;
        Object vals = Symbols.NIL;
        Symbol var = Symbols.NIL;
        Object val = Symbols.NIL;
        for (Object l = cdr; !Lists.isEnd(l); l = Lists.cddr(l)) {
            var = Data.symbol(Lists.car(l));
            val = Values.singleValue(eval(Lists.cadr(l), env));
            vars = Lists.cons(var, vars);
            vals = Lists.cons(val, vals);
        }
        for (Object l1 = vars, l2 = vals;
             !Lists.isEnd(l1) && !Lists.isEnd(l2);
             l1 = Lists.cdr(l1), l2 = Lists.cdr(l2)) {
            var = Data.symbol(Lists.car(l1));
            val = Lists.car(l2);
            env.set(var, val);
        }
        return val;
    }
    static private Object evalQuote(Object cdr, Env env) {
        if (Lists.length(cdr) != 1)
            throw new ProgramException
                ("args.length must be 1: ~S.", Lists.list(cdr));
        return Lists.car(cdr);
    }
    static private Object evalFunction(Object cdr, Env env) {
        if (Lists.length(cdr) != 1)
            throw new ProgramException
                ("args.length must be 1: ~S.", Lists.list(cdr));
        return function(Lists.car(cdr), env);
    }
    static private Object evalProgn(Object cdr, Env env) {
        return evalBody(cdr, env);
    }
    static private Object evalProg(Object cdr, Env env) {
        if (Lists.length(cdr) < 1)
            throw new ProgramException
                ("args.length must be greater than 0: ~S.", Lists.list(cdr));
        Object vars = Lists.car(cdr);
        Object body = Lists.cdr(cdr);
        //Object decls,body;
        //if (Data.isList(Lists.cadr(cdr)) &&
        //    Lists.caadr(cdr) == Symbols.DECLARE) {
        //    decls = Lists.cdadr(cdr);
        //    body = Lists.cddr(cdr);
        //}
        //else {
        //    decls = Symbols.NIL;
        //    body = Lists.cdr(cdr);
        //}
        {
            Env newenv = env.child();
            for (Object l = vars; !Lists.isEnd(l); l = Lists.cdr(l)) {
                Symbol var = Data.symbol(Lists.car(l));
                newenv.bind(var, Symbols.NIL);
            }
            for (Object l = body; !Lists.isEnd(l); l = Lists.cdr(l)) {
                Object exp = Lists.car(l);
                //if (Logger.tracelevelp(newenv))
                //    Logger.trace("[prog:BODY] exp=~S",
                //                 Lists.list(exp), null, newenv);
                if (Data.isAtom(exp))
                    continue;
                try {
                    eval(exp, newenv);
                } catch (NonLocalExit.GO e) {
                    Object tag = e.tag();
                    //if (Logger.tracelevelp(newenv))
                    //    Logger.trace("[prog:GO] tag=~S",
                    //                 Lists.list(tag), null, newenv);
                    l = Lists.memq(tag, body);
                    //if (Logger.tracelevelp(newenv))
                    //    Logger.trace("[prog:GO] l=~S",
                    //                 Lists.list(l), null, newenv);
                    if (l == Symbols.NIL)
                        throw e;
                } catch (NonLocalExit.RETURN e) {
                    Object val = e.val();
                    //if (Logger.tracelevelp(newenv))
                    //    Logger.trace("[prog:RETURN] val=~S",
                    //                 Lists.list(val), null, newenv);
                    return val;
                }
            }
        }
        /*
         * retval of PROG is NIL unless RETURN form exists
         * in the body of PROG.
         */
        return Symbols.NIL;
    }
    static private Object evalGo(Object cdr, Env env) {
        if (Lists.length(cdr) != 1)
            throw new ProgramException
                ("args.length must be 1: ~S.", Lists.list(cdr));
        Object tag = Lists.car(cdr);
        if (Data.isAtom(tag))
            throw new NonLocalExit.GO(tag);
        else
            throw new NonLocalExit.GO(eval(tag, env));
    }
    static private Object evalReturn(Object cdr, Env env) {
        if (Lists.length(cdr) != 1)
            throw new ProgramException
                ("args.length must be 1: ~S.", Lists.list(cdr));
        Object val = Lists.car(cdr);
        throw new NonLocalExit.RETURN(eval(val, env));
    }
    static private Object evalThrow(Object cdr, Env env) {
        if (Lists.length(cdr) != 2)
            throw new ProgramException
                ("args.length must be 2: ~S.", Lists.list(cdr));
        Object tag = Values.singleValue(eval(Lists.car(cdr), env));
        Object form = Lists.cadr(cdr);
        return exitWithThrow(Data.symbol(tag), eval(form, env), env);
    }
    static private Object evalCatch(Object cdr, Env env) {
        if (Lists.length(cdr) < 1)
            throw new ProgramException
                ("args.length must be greater than 0: ~S.", Lists.list(cdr));
        Object tagspec
            = Values.singleValue(eval(Lists.car(cdr), env));
        Object body = Lists.cdr(cdr);
        try {
            return evalBody(body, env);
        }
        catch (NonLocalExit.THROW e) {
            return valueOf(e, tagspec, env);
        }
    }
    static private Object evalUnwindProtect(Object cdr, Env env) {
        if (Lists.length(cdr) < 1)
            throw new ProgramException
                ("args.length must be greater than 0: ~S.", Lists.list(cdr));
        Object form = Lists.car(cdr);
        Object cleanupforms = Lists.cdr(cdr);
        try {
            return eval(form, env);
        }
        finally {
            evalBody(cleanupforms, env);
        }
    }
    static private Object evalEvalWhen(Object cdr, Env env) {
        Object timeList = Lists.car(cdr);
        Object body = Lists.cdr(cdr);
        if (Lists.memq(Symbols.EVAL, timeList) != Symbols.NIL)
            return evalBody(body, env);
        else
            return Symbols.NIL;
    }
    static private Object evalDeclare(Object cdr, Env env) {
        return Symbols.DECLARE;
    }
    static private Object evalMultipleValueBind(Object cdr, Env env) {
        if (Lists.length(cdr) < 2)
            throw new ProgramException
                ("args.length must be greater than 1: ~S.", Lists.list(cdr));
        Object vars = Lists.car(cdr);
        Values mv = Values.multipleValue(eval(Lists.cadr(cdr), env));
        Object body = Lists.cddr(cdr);
        {
            Env newenv = env.child();
            Symbol var = Symbols.NIL;
            Object val = Symbols.NIL;
            int i = 0;
            /*
             * Note that this iteration will not work
             * if the MV is used in the body of following method:
             * Lists#isEnd, Lists#car, Lists#cdr, Env#set, Data#symbol.
             */
            for (Object l = vars; !Lists.isEnd(l); l = Lists.cdr(l)) {
                var = Data.symbol(Lists.car(l));
                val = mv.nth(i++);
                newenv.bind(var, val);
            }
            return evalBody(body, newenv);
        }
    }
    static private Object evalMultipleValueList(Object cdr, Env env) {
        if (Lists.length(cdr) != 1)
            throw new ProgramException
                ("args.length must be 1: ~S.", Lists.list(cdr));
        Values mv = Values.multipleValue(eval(Lists.car(cdr), env));
        return mv.toList();
    }
    static private Object evalMultipleValueSetq(Object cdr, Env env) {
        if (Lists.length(cdr) != 2)
            throw new ProgramException
                ("args.length must be 2: ~S.", Lists.list(cdr));
        Object vars = Lists.car(cdr);
        Values mv = Values.multipleValue(eval(Lists.cadr(cdr), env));
        Symbol var = Symbols.NIL;
        Object val = Symbols.NIL;
        int i = 0;
        /*
         * Note that this iteration will not work
         * if the MV is used in the body of following method:
         * Lists#isEnd, Lists#car, Lists#cdr, Env#set, Data#symbol.
         */
        for (Object l = vars; !Lists.isEnd(l); l = Lists.cdr(l)) {
            var = Data.symbol(Lists.car(l));
            val = mv.nth(i++);
            env.set(var, val);
        }
        return val;
    }
    static private Object evalNthValue(Object cdr, Env env) {
        if (Lists.length(cdr) != 2)
            throw new ProgramException
                ("args.length must be 2: ~S.", Lists.list(cdr));
        Integer n = Data.fixnum(eval(Lists.car(cdr), env));
        Values mv = Values.multipleValue(eval(Lists.cadr(cdr), env));
        return mv.nth(n.intValue());
    }
    static private Object evalSubr(Subr subr, Object cdr, Env env)
        throws java.lang.Exception {
        int nargs = Lists.length(cdr);
        switch (nargs) {
        case 0:
            if (subr instanceof Callable0) {
                Callable0 c0 = (Callable0) subr;
                return c0.call0(env);
            }
            if (subr instanceof Callable0r) {
                Callable0r c0r = (Callable0r) subr;
                return c0r.call0(Symbols.NIL, env);
            }
            break;
        case 1:
            if (subr instanceof Callable1) {
                Callable1 c1 = (Callable1) subr;
                Object r0 = Values.singleValue(eval(Lists.car(cdr), env));
                return c1.call1(r0, env);
            }
            if (subr instanceof Callable0r) {
                Callable0r c0r = (Callable0r) subr;
                Object args = evalArgs(cdr, env);
                return c0r.call0(args, env);
            }
            if (subr instanceof Callable1r) {
                Callable1r c1r = (Callable1r) subr;
                Object r0 = Values.singleValue(eval(Lists.car(cdr), env));
                return c1r.call1(r0, Symbols.NIL, env);
            }
            break;
        case 2:
            if (subr instanceof Callable2) {
                Callable2 c2 = (Callable2) subr;
                Object r0 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r1 = Values.singleValue(eval(Lists.car(cdr), env));
                return c2.call2(r0, r1, env);
            }
            if (subr instanceof Callable0r) {
                Callable0r c0r = (Callable0r) subr;
                Object args = evalArgs(cdr, env);
                return c0r.call0(args, env);
            }
            if (subr instanceof Callable1r) {
                Callable1r c1r = (Callable1r) subr;
                Object r0 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object args = evalArgs(cdr, env);
                return c1r.call1(r0, args, env);
            }
            if (subr instanceof Callable2r) {
                Callable2r c2r = (Callable2r) subr;
                Object r0 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r1 = Values.singleValue(eval(Lists.car(cdr), env));
                return c2r.call2(r0, r1, Symbols.NIL, env);
            }
            break;
        case 3:
            if (subr instanceof Callable3) {
                Callable3 c3 = (Callable3) subr;
                Object r0 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r1 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r2 = Values.singleValue(eval(Lists.car(cdr), env));
                return c3.call3(r0, r1, r2, env);
            }
            if (subr instanceof Callable0r) {
                Callable0r c0r = (Callable0r) subr;
                Object args = evalArgs(cdr, env);
                return c0r.call0(args, env);
            }
            if (subr instanceof Callable1r) {
                Callable1r c1r = (Callable1r) subr;
                Object r0 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object args = evalArgs(cdr, env);
                return c1r.call1(r0, args, env);
            }
            if (subr instanceof Callable2r) {
                Callable2r c2r = (Callable2r) subr;
                Object r0 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r1 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object args = evalArgs(cdr, env);
                return c2r.call2(r0, r1, args, env);
            }
            if (subr instanceof Callable3r) {
                Callable3r c3r = (Callable3r) subr;
                Object r0 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r1 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r2 = Values.singleValue(eval(Lists.car(cdr), env));
                return c3r.call3(r0, r1, r2, Symbols.NIL, env);
            }
            break;
        case 4:
            if (subr instanceof Callable4) {
                Callable4 c4 = (Callable4) subr;
                Object r0 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r1 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r2 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r3 = Values.singleValue(eval(Lists.car(cdr), env));
                return c4.call4(r0, r1, r2, r3, env);
            }
            if (subr instanceof Callable0r) {
                Callable0r c0r = (Callable0r) subr;
                Object args = evalArgs(cdr, env);
                return c0r.call0(args, env);
            }
            if (subr instanceof Callable1r) {
                Callable1r c1r = (Callable1r) subr;
                Object r0 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object args = evalArgs(cdr, env);
                return c1r.call1(r0, args, env);
            }
            if (subr instanceof Callable2r) {
                Callable2r c2r = (Callable2r) subr;
                Object r0 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r1 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object args = evalArgs(cdr, env);
                return c2r.call2(r0, r1, args, env);
            }
            if (subr instanceof Callable3r) {
                Callable3r c3r = (Callable3r) subr;
                Object r0 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r1 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r2 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object args = evalArgs(cdr, env);
                return c3r.call3(r0, r1, r2, args, env);
            }
            if (subr instanceof Callable4r) {
                Callable4r c4r = (Callable4r) subr;
                Object r0 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r1 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r2 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r3 = Values.singleValue(eval(Lists.car(cdr), env));
                return c4r.call4(r0, r1, r2, r3, Symbols.NIL, env);
            }
            break;
        case 5:
            if (subr instanceof Callable5) {
                Callable5 c5 = (Callable5) subr;
                Object r0 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r1 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r2 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r3 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r4 = Values.singleValue(eval(Lists.car(cdr), env));
                return c5.call5(r0, r1, r2, r3, r4, env);
            }
            if (subr instanceof Callable0r) {
                Callable0r c0r = (Callable0r) subr;
                Object args = evalArgs(cdr, env);
                return c0r.call0(args, env);
            }
            if (subr instanceof Callable1r) {
                Callable1r c1r = (Callable1r) subr;
                Object r0 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object args = evalArgs(cdr, env);
                return c1r.call1(r0, args, env);
            }
            if (subr instanceof Callable2r) {
                Callable2r c2r = (Callable2r) subr;
                Object r0 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r1 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object args = evalArgs(cdr, env);
                return c2r.call2(r0, r1, args, env);
            }
            if (subr instanceof Callable3r) {
                Callable3r c3r = (Callable3r) subr;
                Object r0 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r1 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r2 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object args = evalArgs(cdr, env);
                return c3r.call3(r0, r1, r2, args, env);
            }
            if (subr instanceof Callable4r) {
                Callable4r c4r = (Callable4r) subr;
                Object r0 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r1 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r2 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r3 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object args = evalArgs(cdr, env);
                return c4r.call4(r0, r1, r2, r3, args, env);
            }
            if (subr instanceof Callable5r) {
                Callable5r c5r = (Callable5r) subr;
                Object r0 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r1 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r2 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r3 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r4 = Values.singleValue(eval(Lists.car(cdr), env));
                return c5r.call5(r0, r1, r2, r3, r4, Symbols.NIL, env);
            }
            break;
        default:
            if (subr instanceof Callable0r) {
                Callable0r c0r = (Callable0r) subr;
                Object args = evalArgs(cdr, env);
                return c0r.call0(args, env);
            }
            if (subr instanceof Callable1r) {
                Callable1r c1r = (Callable1r) subr;
                Object r0 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object args = evalArgs(cdr, env);
                return c1r.call1(r0, args, env);
            }
            if (subr instanceof Callable2r) {
                Callable2r c2r = (Callable2r) subr;
                Object r0 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r1 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object args = evalArgs(cdr, env);
                return c2r.call2(r0, r1, args, env);
            }
            if (subr instanceof Callable3r) {
                Callable3r c3r = (Callable3r) subr;
                Object r0 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r1 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r2 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object args = evalArgs(cdr, env);
                return c3r.call3(r0, r1, r2, args, env);
            }
            if (subr instanceof Callable4r) {
                Callable4r c4r = (Callable4r) subr;
                Object r0 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r1 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r2 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r3 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object args = evalArgs(cdr, env);
                return c4r.call4(r0, r1, r2, r3, args, env);
            }
            if (subr instanceof Callable5r) {
                Callable5r c5r = (Callable5r) subr;
                Object r0 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r1 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r2 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r3 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object r4 = Values.singleValue(eval(Lists.car(cdr), env));
                cdr = Lists.cdr(cdr);
                Object args = evalArgs(cdr, env);
                return c5r.call5(r0, r1, r2, r3, r4, args, env);
            }
            break;
        }

        if (subr instanceof Callable) {
            Callable c = (Callable) subr;
            Object args = evalArgs(cdr, env);
            return c.call(args, env);
        }
        else {
            throw new ProgramException
                 ("subr ~S cannot be applied to arguments: ~S.",
                  Lists.list(subr, cdr));
        }
    }
    static private Object evalExpr(Expr expr, Object cdr, Env env) {
        Object args = evalArgs(cdr, env);
        {
            Env newenv = env.child();
            bindArgs(expr.lambdaList(), args, newenv);
            return evalBody(expr.body(), newenv);
        }
    }
    static private Object evalFSubr(Subr subr, Object cdr, Env env)
        throws java.lang.Exception {
        if (subr instanceof Callable1) {
            Callable1 c1 = (Callable1) subr;
            return c1.call1(cdr, env);
        }
        else if (subr instanceof Callable) {
            // inefficient!
            Callable c = (Callable) subr;
            Object args = Lists.list(cdr);
            return c.call(args, env);
        }
        else {
            throw new ProgramException
                 ("subr ~S cannot be applied to arguments: ~S.",
                  Lists.list(subr, cdr));
        }
    }
    static private Object evalFexpr(Expr fexpr, Object cdr, Env env) {
        // inefficient!
        Object args = Lists.list(cdr);
        {
            Env newenv = env.child();
            bindArgs(fexpr.lambdaList(), args, newenv);
            return evalBody(fexpr.body(), newenv);
        }
    }
    static private Object evalLambda(Object car, Object cdr, Env env) {
        Object vars = Lists.cadr(car);
        Object body = Lists.cddr(car);
        Expr expr = new Expr("EXPR", vars, body, env);
        return evalExpr(expr, cdr, env);
    }

    /*
     * eval util methods
     */

    /**
     * Evaluates each form in the specified list
     * and returns a list of the results of each evaluation.
     * @param args List of forms
     * @param env
     * @return List of the results of each evaluation
     * @throws Evaluator.Exception
     */
    static public Object evalArgs(Object args, Env env) {
        ListBuilder lb = new ListBuilder();
        for (Object l = args; !Lists.isEnd(l); l = Lists.cdr(l)) {
            lb.append(Values.singleValue(eval(Lists.car(l), env)));
        }
        return lb.toList();
    }
    /**
     * Evaluates each form in the specified list
     * and returns the value of the last form.
     * @param body List of forms
     * @param env
     * @return Value of the last form
     * @throws Evaluator.Exception
     */
    static public Object evalBody(Object body, Env env) {
        Object ret = Symbols.NIL;
        for (Object l = body; !Lists.isEnd(l); l = Lists.cdr(l)) {
            ret = eval(Lists.car(l), env);
        }
        return ret;
    }
    /**
     * Creates a binding of the variables defined by the lambdaList
     * <code>ll</code> and the arguments specified by <code>args</code>.
     * @param ll LambdaList
     * @param args
     * @param env
     * @return Environment specified by <code>env</code>, in which
     * the bindings created by this method are stored
     */
    static public Env bindArgs(LambdaList ll, Object args, Env env) {
        //if (Logger.tracelevelp(env))
        //    Logger.trace("[bindArgs] ll=~S args=~S",
        //                 Lists.list(vars, args), env);
        Object var;
        Object initform;
        Symbol svar;
        Symbol key;
        Object arg;
        // bind rest vars
        if (ll.isWhole()) {
            var = ll.wholeVar();
            if (Data.isSymbol(var)) {
                env.bind(Data.symbol(var), args);
            }
            else {
                bindArgs(Data.lambdaList(var), args, env);
            }
        }
        // bind required vars
        for (int i = 0; i < ll.reqCount(); i++) {
            checkMinArgs(ll, args, env);
            var = ll.reqVar(i);
            arg = Lists.car(args);
            if (Data.isSymbol(var)) {
                env.bind(Data.symbol(var), arg);
            }
            else {
                bindArgs(Data.lambdaList(var), arg, env);
            }
            args = Lists.cdr(args);
        }
        // bind optional vars
        for (int i = 0; i < ll.optCount(); i++) {
            var = ll.optVar(i);
            svar = ll.optSvar(i);
            if (Lists.isEnd(args)) {
                initform = ll.optInitform(i);
                if (Data.isSymbol(var)) {
                    env.bind(Data.symbol(var), eval(initform, env));
                }
                else {
                    bindArgs(Data.lambdaList(var), eval(initform, env), env);
                }
                if (svar != LambdaList.NOT_SPECIFIED) {
                    env.bind(svar, Symbols.NIL);
                }
            } else {
                arg = Lists.car(args);
                if (Data.isSymbol(var)) {
                    env.bind(Data.symbol(var), arg);
                }
                else {
                    bindArgs(Data.lambdaList(var), arg, env);
                }
                if (svar != LambdaList.NOT_SPECIFIED) {
                    env.bind(svar, Symbols.T);
                }
            }
            args = Lists.cdr(args);
        }
        // bind rest vars
        if (ll.isRest()) {
            var = ll.restVar();
            if (Data.isSymbol(var)) {
                env.bind(Data.symbol(var), args);
            }
            else {
                bindArgs(Data.lambdaList(var), args, env);
            }
        }
        else {
            // no rest var exists
            if (ll.keyCount() <= 0) {
                // no keyword var exists
                checkMaxArgs(ll, args, env);
            }
        }
        // bind keyword vars
        if (ll.keyCount() > 0) {
            // check arguments for keyword vars
            checkKeyArgs(ll, args, env);
        }
        for (int i = 0; i < ll.keyCount(); i++) {
            var = ll.keyVar(i);
            key = ll.keyKey(i);
            svar = ll.keySvar(i);
            Object ret = Lists.memq(key, args);
            if (ret == Symbols.NIL) {
                initform = ll.keyInitform(i);
                if (Data.isSymbol(var)) {
                    env.bind(Data.symbol(var), eval(initform, env));
                }
                else {
                    bindArgs(Data.lambdaList(var), eval(initform, env), env);
                }
                if (svar != LambdaList.NOT_SPECIFIED) {
                    env.bind(svar, Symbols.NIL);
                }
            }
            else {
                arg = Lists.cadr(ret);
                if (Data.isSymbol(var)) {
                    env.bind(Data.symbol(var), arg);
                }
                else {
                    bindArgs(Data.lambdaList(var), arg, env);
                }
                if (svar != LambdaList.NOT_SPECIFIED) {
                    env.bind(svar, Symbols.T);
                }
            }
        }
        // bind aux vars
        for (int i = 0; i < ll.auxCount(); i++) {
            var = ll.auxVar(i);
            initform = ll.auxInitform(i);
            if (Data.isSymbol(var)) {
                env.bind(Data.symbol(var), eval(initform, env));
            }
            else {
                bindArgs(Data.lambdaList(var), eval(initform, env), env);
            }
        }
        return env;
    }
    /**
     * Apply the macro expansion function <code>macro</code>
     * to the specified form <code>exp</code>.
     * @param macro Macro expansion function
     * @param exp form to be expanded
     * @param env
     * @return Result of the expansion
     */
    static public Object evalMacro(Expr macro, Object exp, Env env) {
        Env newenv = env.child();
        bindArgs(macro.lambdaList(), exp, newenv);
        return evalBody(macro.body(), newenv);
    }
    static public Object checkMinArgs(LambdaList ll, Object args, Env env) {
        if (Lists.isEnd(args)) {
            // args end -> error!
            throw new ProgramException
                ("args.length is less than expected: ~S",
                 Lists.list(ll.params()));
        }
        return args;
    }
    static public Object checkMaxArgs(LambdaList ll, Object args, Env env) {
        if (!Lists.isEnd(args)) {
            // args does not end -> error!
            throw new ProgramException
                ("args.length is more than expected: ~S",
                 Lists.list(ll.params()));
        }
        return args;
    }
    static public Object checkKeyArgs(LambdaList ll, Object args, Env env) {
        boolean allow = false;
        for (Object l = args; !Lists.isEnd(l); l = Lists.cddr(l)) {
            if (!Data.isSymbol(Lists.car(l))) {
                throw new ProgramException
                    ("illegal keyword list specified. "+
                     "key must be symbol: ~S.",
                     Lists.list(args));
            }
            else if (Lists.isEnd(Lists.cdr(l))) {
                throw new ProgramException
                    ("illegal keyword list specified. "+
                     "value for key not exists: ~S.",
                     Lists.list(args));
            }
            if (Lists.car(l) == Symbols.KW_ALLOW_OTHER_KEYS) {
                if (Lists.cadr(l) != Symbols.NIL) {
                    // test may be omitted.
                    allow = true;
                }
                else {
                    // test is required.
                }
            }
        }
        if (!allow && !ll.allowOtherKeys()) {
            // test for other keys
            for (Object l = args; !Lists.isEnd(l); l = Lists.cddr(l)) {
                //System.out.println("l="+l);
                Symbol arg_key = Data.symbol(Lists.car(l));
                if (arg_key == Symbols.KW_ALLOW_OTHER_KEYS) {
                    continue;
                }
                boolean found = false;
                for (int i = 0; i < ll.keyCount(); i++) {
                    if (arg_key == ll.keyKey(i)) {
                        // arg_key was found in keywowd vars
                        found = true;
                        break;
                    }
                }
                if (!found)
                    throw new ProgramException
                        ("illegal keyword in argument list: ~S.",
                         Lists.list(l));
            }
        }
        return args;
    }
    /**
     * Returns the function object specified by <code>func</code>.
     * @param func Object to be interpreted as a function,
     *        which must be either a symbol or a lambda expression
     * @param env
     * @return Function object
     */
    static public Function function(Object func, Env env) {
        if (Data.isSymbol(func)) {
            Symbol sym = Data.symbol(func);
            Object plist = env.lisp().functions(sym);
            if (plist == Symbols.NIL) {
                throw new UndefinedFunctionException(func);
            }
            return Data.function(Lists.cadr(plist));
        }
        else if (Data.isPair(func) &&
                 Lists.car(func) == Symbols.LAMBDA) {
            Object vars = Lists.cadr(func);
            Object body = Lists.cddr(func);
            return new Expr("EXPR", vars, body, env);
        }
        else {
            throw new UndefinedFunctionException(func);
        }
    }
    static public Object exitWithThrow(Symbol tag, Object val, Env env) {
        throw new NonLocalExit.THROW(tag, val);
    }
    static public Object exitWithError(Object msg, Object datum,
                                       Object kwd, Env env) {
        String ctrlstr;
        Object args;
        if (datum == null) {
            ctrlstr = "~A";
            args = Lists.list(msg);
        }
        else {
            ctrlstr = "~A: ~S";
            args = Lists.list(msg, datum);
        }
        if (kwd == Symbols.UNDF_FNCTN ||
            kwd == Symbols.UNBND_VRBL) {
            throw new CellException(ctrlstr, args);
        }
        else if (kwd == Symbols.WRNG_TYPE_ARG) {
            throw new TypeException(ctrlstr, args);
        }
        else if (kwd == Symbols.WRNG_NO_ARGS) {
            throw new ProgramException(ctrlstr, args);
        }
        else if (kwd == Symbols.UNSEEN_GO_TAG) {
            throw new ControlException(ctrlstr, args);
        }
        else if (kwd == Symbols.IO_LOSSAGE) {
            throw new StreamException(ctrlstr, args);
        }
        else { /* kwd == Symbols.FAIL_ACT */
            throw new SimpleException(ctrlstr, args);
        }
    }
    static public Object valueOf
        (NonLocalExit.THROW t, Object tagspec, Env env) {
        //if (Logger.tracelevelp(env)) {
        //    Logger.trace("[throw] tag: ~S",
        //                 Lists.list(t.tag()), env);
        //    Logger.trace("[throw] tagspec: ~S",
        //                 Lists.list(tagspec), env);
        //}
        if (Data.isAtom(tagspec)) {
            if (tagspec == t.tag())
                return t.val();
            else
                throw t;
        }
        else {
            if (!Lists.isEnd(Lists.memq(t.tag(), tagspec)))
                return t.val();
            else
                throw t;
        }
    }
    /** Returns true when the specified object is evaluated
        as a constant. */
    static public boolean isConstant(Object exp, Env env) {
        // for symbols
        if (Data.isSymbol(exp)) {
            Symbol sym = Data.symbol(exp);
            return sym.isSelfeval()
                || env.lisp().isConstant(sym);
        }
        // for other atoms
        else if (Data.isAtom(exp)) {
            return true;
        }
        // for lists
        else if (Data.isList(exp)) {
            return Lists.car(exp) == Symbols.QUOTE;
        }
        // for others
        else {
            throw new ProgramException
                ("unknown exp: ~S.", Lists.list(exp));
        }
    }

    private Evaluator() {}
}
