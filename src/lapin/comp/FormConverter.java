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
import lapin.eval.Funcall;
import lapin.eval.Macro;
import lapin.function.Expr;
import lapin.function.Function;
import lapin.lang.Data;
import lapin.lang.Env;
import lapin.lang.Lists;
import lapin.lang.ProgramException;
import lapin.lang.Symbol;
import lapin.lang.Symbols;
import lapin.lang.SysSymbols;
import lapin.lang.Values;
import lapin.util.ListBuilder;
import lapin.util.Logger;
import lapin.util.TracableException;

final class FormConverter {
    // uninterned symbols: for private use only
    static private final Symbol MACRO_NOT_EXPANDED
        = Symbol.gensym("_MACRO-NOT-EXPANDED_");

    /*
     * expansion function for compiler macro
     */

    static private Object cmacroExpand(Object exp, Env env) {
        while (true) {
            if (!Data.isList(exp)) {
                return exp;
            }
            Object car = Lists.car(exp);
            if (!Data.isSymbol(car)) {
                return exp;
            }
            Symbol sym = Data.symbol(car);
            Object val = env.lisp().getProp(sym, Symbols.CMACRO);
            if (!Data.isExpr(val)) {
                return exp;
            }
            Expr macro = Data.expr(val);
            Object expanded = Evaluator.evalMacro(macro, exp, env);
            if (expanded == exp) {
                return exp;
            }
            exp = expanded;
        }
    }

    /*
     * form converter
     */

    static Object convertForm(Object exp, Env env) {
        //if (Logger.tracelevelp(env))
        //    Logger.trace("[convert] ~S -> ~S", Lists.list(exp, ret), env);
        try {
            return _convertForm(exp, env);
        } catch (Compiler.Exception e) {
            throw e.push(exp);
        } catch (TracableException e) {
            throw e;
        } catch (java.lang.Exception e) {
            throw new Compiler.Exception
                ("form convert error", e).push(exp);
        }
    }
    static private Object _convertForm(Object exp, Env env) {
        while (true) {
            exp = cmacroExpand(exp, env);
            // for symbols
            if (Data.isSymbol(exp)) {
                return convertSymbol(Data.symbol(exp), env);
            }
            // for other atoms
            else if (Data.isAtom(exp)) {
                return convertAtom(exp, env);
            }
            // for lists
            else if (Data.isList(exp)) {
                Object car = Lists.car(exp);
                Object cdr = Lists.cdr(exp);
                if (car == Symbols.IF) {
                    return convertIf(cdr, env);
                }
                else if (car == Symbols.AND) {
                    return convertAnd(cdr, env);
                }
                else if (car == Symbols.OR) {
                    return convertOr(cdr, env);
                }
                else if (car == Symbols.SETQ) {
                    return convertSetq(cdr, env);
                }
                else if (car == Symbols.PSETQ) {
                    return convertPsetq(cdr, env);
                }
                else if (car == Symbols.QUOTE) {
                    return convertQuote(cdr, env);
                }
                else if (car == Symbols.FUNCTION) {
                    return convertFunction(cdr, env);
                }
                else if (car == Symbols.PROGN) {
                    return convertProgn(cdr, env);
                }
                else if (car == Symbols.PROG) {
                    return convertProg(cdr, env);
                }
                else if (car == Symbols.RETURN) {
                    return convertReturn(cdr, env);
                }
                else if (car == Symbols.GO) {
                    return convertGo(cdr, env);
                }
                else if (car == Symbols.THROW) {
                    return convertThrow(cdr, env);
                }
                else if (car == Symbols.CATCH) {
                    return convertCatch(cdr, env);
                }
                else if (car == Symbols.UNWIND_PROTECT) {
                    return convertUnwindProtect(cdr, env);
                }
                else if (car == Symbols.EVAL_WHEN) {
                    return convertEvalWhen(cdr, env);
                }
                else if (car == Symbols.DECLARE) {
                    return convertDeclare(cdr, env);
                }
                else if (car == Symbols.MULTIPLE_VALUE_BIND) {
                    return convertMultipleValueBind(cdr, env);
                }
                else if (car == Symbols.MULTIPLE_VALUE_LIST) {
                    return convertMultipleValueList(cdr, env);
                }
                else if (car == Symbols.MULTIPLE_VALUE_SETQ) {
                    return convertMultipleValueSetq(cdr, env);
                }
                else if (car == Symbols.NTH_VALUE) {
                    return convertNthValue(cdr, env);
                }
                else if (Data.isSymbol(car)) {
                    Symbol sym = Data.symbol(car);
                    Symbol handlerType = SysSymbols.C_HANDLER_CONV_FORM;
                    if (env.lisp().getProp(sym, handlerType)
                        != Symbols.NIL) {
                        Function handler = Data.function
                            (env.lisp().getProp(sym, handlerType));
                        if (Logger.tracelevelp(env))
                            Logger.trace("[convert] type=~S handler=~S",
                                         Lists.list(handlerType, handler),
                                         env);
                        return Funcall.funcall1(handler, exp, env);
                    }
                    else {
                        Object plist = env.lisp().functions(sym);
                        Object type = Lists.car(plist);
                        Object func = Lists.cadr(plist);
                        if (type == Symbols.MACRO) {
                            exp = Evaluator.evalMacro
                                (Data.expr(func), exp, env);
                            continue;
                        }
                        else {
                            return convertCall(sym, cdr, env);
                        }
                    }
                }
                else if (Data.isList(car)) {
                    Object caar = Lists.car(car);
                    if (caar == Symbols.LAMBDA) {
                        return convertCall(convertLambda(car, env), cdr, env);
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
                throw new ProgramException
                    ("unknown exp: ~S.", Lists.list(exp));
            }
        }
    }
    static private Object convertSymbol(Symbol sym, Env env) {
        if (sym.isSelfeval())
            return Forms.quote(sym);
        else
            return sym;
    }
    static private Object convertAtom(Object atom, Env env) {
        return Forms.quote(atom);
    }
    static private Object convertIf(Object cdr, Env env) {
        if (Lists.length(cdr) <= 1)
            throw new ProgramException
                ("args.length must be greater than 1: ~S.",
                 Lists.list(cdr));
        Object test = Lists.car(cdr);
        Object  con = Lists.cadr(cdr);
        Object  alt = Lists.caddr(cdr);
        test = convertForm(test, env);
        con  = convertForm( con, env);
        alt  = convertForm( alt, env);
        if (Forms.isNil(test)) {
            // test is NIL
            return alt;
        }
        else if (Forms.isConstant(test)) {
            // test is an atom (not a variable)
            return con;
        }
        else {
            return Lists.list(Symbols.IF, test, con, alt);
        }
    }
    static private Object convertAnd(Object cdr, Env env) {
        switch (Lists.length(cdr)) {
        case 0:
            return Forms.T;
        case 1:
            return convertForm(Lists.car(cdr), env);
        default:
            Object l = cdr;
            Object form = convertForm(Lists.car(l), env);
            if (Forms.isNil(form)) {
                return Forms.NIL;
            }
            l = Lists.cdr(l);
            ListBuilder lb = new ListBuilder();
            lb.append(Symbols.AND).append(form);
            while (!Lists.isEnd(l)) {
                form = convertForm(Lists.car(l), env);
                lb.append(form);
                if (Forms.isNil(form)) {
                    break;
                }
                l = Lists.cdr(l);
            }
            return lb.toList();
        }
    }
    static private Object convertOr(Object cdr, Env env) {
        switch (Lists.length(cdr)) {
        case 0:
            return Forms.NIL;
        case 1:
            return convertForm(Lists.car(cdr), env);
        default:
            Object l = cdr;
            Object form = convertForm(Lists.car(l), env);
            if (Forms.isConstant(form) && !Forms.isNil(form)) {
                return form;
            }
            l = Lists.cdr(l);
            ListBuilder lb = new ListBuilder();
            lb.append(Symbols.OR).append(form);
            while (!Lists.isEnd(l)) {
                form = convertForm(Lists.car(l), env);
                lb.append(form);
                if (Forms.isConstant(form) && !Forms.isNil(form)) {
                    break;
                }
                l = Lists.cdr(l);
            }
            return lb.toList();
        }
    }
    static private Object convertSetq(Object cdr, Env env) {
        int len = Lists.length(cdr);
        if (len%2 == 1) {
            throw new ProgramException
                ("odd number of arguments: ~S.", Lists.list(cdr));
        }
        else if (len == 0) {
            return Forms.NIL;
        }
        ListBuilder lb = new ListBuilder();
        lb.append(Symbols.SETQ);
        for (Object l = cdr; !Lists.isEnd(l); l = Lists.cddr(l)) {
            Symbol var = Data.symbol(Lists.car(l));
            Object form = Lists.cadr(l);
            lb.append(var).append(convertForm(form, env));
        }
        return lb.toList();
    }
    static private Object convertPsetq(Object cdr, Env env) {
        int len = Lists.length(cdr);
        if (len%2 == 1) {
            throw new ProgramException
                ("odd number of arguments: ~S.", Lists.list(cdr));
        }
        else if (len == 0) {
            return Forms.NIL;
        }
        ListBuilder lb = new ListBuilder();
        lb.append(Symbols.PSETQ);
        for (Object l = cdr; !Lists.isEnd(l); l = Lists.cddr(l)) {
            Symbol var = Data.symbol(Lists.car(l));
            Object form = Lists.cadr(l);
            lb.append(var).append(convertForm(form, env));
        }
        return lb.toList();
    }
    static private Object convertQuote(Object cdr, Env env) {
        if (Lists.length(cdr) != 1)
            throw new ProgramException
                ("args.length must be 1: ~S.", Lists.list(cdr));
        return Lists.cons(Symbols.QUOTE, cdr);
    }
    static private Object convertFunction(Object cdr, Env env) {
        if (Lists.length(cdr) != 1)
            throw new ProgramException
                ("args.length must be 1: ~S.", Lists.list(cdr));
        Object func = Lists.car(cdr);
        if (Data.isSymbol(func))
            return Lists.list(Symbols.FUNCTION, func);
        else if (Data.isPair(func) &&
            Lists.car(func) == Symbols.LAMBDA)
            return Lists.list(Symbols.FUNCTION,
                              convertLambda(func, env));
        else
            throw new ProgramException
                ("unexpected argument type: ~S.", Lists.list(func));
    }
    static private Object convertProgn(Object cdr, Env env) {
        switch (Lists.length(cdr)) {
        case 0:
            return Forms.NIL;
        default:
            ListBuilder lb = new ListBuilder();
            lb.append(Symbols.PROGN);
            for (Object l = cdr; !Lists.isEnd(l); l = Lists.cdr(l)) {
                lb.append(convertForm(Lists.car(l), env));
            }
            return lb.toList();
        }
    }
    static private Object convertProg(Object cdr, Env env) {
        if (Lists.length(cdr) <= 0)
            throw new ProgramException
                ("args.length must be greater than 0: ~S.",
                 Lists.list(cdr));
        Object vars = Lists.car(cdr);
        Object body = Lists.cdr(cdr);
        if (!Data.isList(vars)) {
            throw new ProgramException
                ("PROG vars must be list: ~S.",
                 Lists.list(vars));
        }
        switch (Lists.length(cdr)) {
        case 1:
            return Forms.NIL;
        default:
            ListBuilder lb = new ListBuilder();
            lb.append(Symbols.PROG).append(vars);
            for (Object l = body; !Lists.isEnd(l); l = Lists.cdr(l)) {
                Object form = Lists.car(l);
                if (Data.isSymbol(form)) {
                    lb.append(form);
                }
                else {
                    lb.append(convertForm(form, env));
                }
            }
            return lb.toList();
        }
    }
    static private Object convertGo(Object cdr, Env env) {
        if (Lists.length(cdr) != 1)
            throw new ProgramException
                ("args.length must be 1: ~S.",
                 Lists.list(cdr));
        Object form = Lists.car(cdr);
        if (Data.isSymbol(form))
            return Lists.list(Symbols.GO, form);
        else
            return Lists.list(Symbols.GO, convertForm(form, env));
    }
    static private Object convertReturn(Object cdr, Env env) {
        if (Lists.length(cdr) != 1)
            throw new ProgramException
                ("args.length must be 1: ~S.", Lists.list(cdr));
        Object form = Lists.car(cdr);
        return Lists.list(Symbols.RETURN, convertForm(form, env));
    }
    static private Object convertThrow(Object cdr, Env env) {
        if (Lists.length(cdr) != 2)
            throw new ProgramException
                ("args.length must be 2: ~S.", Lists.list(cdr));
        Object tag = Lists.car(cdr);
        Object form = Lists.cadr(cdr);
        return Lists.list(Symbols.THROW,
                          convertForm(tag, env),
                          convertForm(form, env));
    }
    static private Object convertCatch(Object cdr, Env env) {
        if (Lists.length(cdr) <= 0)
            throw new ProgramException
                ("args.length must be greater than 0: ~S.",
                 Lists.list(cdr));
        Object tagspec = Lists.car(cdr);
        Object forms = Lists.cdr(cdr);
        ListBuilder lb = new ListBuilder();
        lb.append(Symbols.CATCH).append(convertForm(tagspec, env));
        for (Object l = forms; !Lists.isEnd(l); l = Lists.cdr(l)) {
            lb.append(convertForm(Lists.car(l), env));
        }
        return lb.toList();
    }
    static private Object convertUnwindProtect(Object cdr, Env env) {
        if (Lists.length(cdr) <= 0)
            throw new ProgramException
                ("args.length must be greater than 0: ~S.",
                 Lists.list(cdr));
        Object form = Lists.car(cdr);
        Object cleanupforms = Lists.cdr(cdr);
        ListBuilder lb = new ListBuilder();
        lb.append(Symbols.UNWIND_PROTECT).append(convertForm(form, env));
        for (Object l = cleanupforms; !Lists.isEnd(l); l = Lists.cdr(l)) {
            lb.append(convertForm(Lists.car(l), env));
        }
        return lb.toList();
    }
    static private Object convertEvalWhen(Object cdr, Env env) {
        if (Lists.length(cdr) <= 0)
            throw new ProgramException
                ("args.length must be greater than 0: ~S.",
                 Lists.list(cdr));
        Object timeList = Lists.car(cdr);
        Object body = Lists.cdr(cdr);
        if (Lists.memq(Symbols.EVAL, timeList) != Symbols.NIL) {
            ListBuilder lb = new ListBuilder();
            for (Object l = body; !Lists.isEnd(l); l = Lists.cdr(l)) {
                lb.append(convertForm(Lists.car(l), env));
            }
            return lb.toList();
        }
        else {
            return Forms.NIL;
        }
    }
    static private Object convertDeclare(Object cdr, Env env) {
        return Lists.cons(Symbols.DECLARE, cdr);
    }
    static private Object convertMultipleValueBind(Object cdr, Env env) {
        if (Lists.length(cdr) <= 1)
            throw new ProgramException
                ("args.length must be greater than 1: ~S.",
                 Lists.list(cdr));
        Object vars = Lists.car(cdr);
        Object mv = Lists.cadr(cdr);
        Object body = Lists.cddr(cdr);
        ListBuilder lb = new ListBuilder();
        lb.append(Symbols.MULTIPLE_VALUE_BIND)
            .append(vars).append(convertForm(mv, env));
        for (Object l = body; !Lists.isEnd(l); l = Lists.cdr(l)) {
            lb.append(convertForm(Lists.car(l), env));
        }
        return lb.toList();
    }
    static private Object convertMultipleValueList(Object cdr, Env env) {
        if (Lists.length(cdr) != 1)
            throw new ProgramException
                ("args.length must be 1: ~S.", Lists.list(cdr));
        Object mv = Lists.car(cdr);
        return Lists.list(Symbols.MULTIPLE_VALUE_LIST,
                          convertForm(mv, env));
    }
    static private Object convertMultipleValueSetq(Object cdr, Env env) {
        if (Lists.length(cdr) != 2)
            throw new ProgramException
                ("args.length must be 2: ~S.", Lists.list(cdr));
        Object vars = Lists.car(cdr);
        Object mv = Lists.cadr(cdr);
        return Lists.list(Symbols.MULTIPLE_VALUE_SETQ,
                          vars, convertForm(mv, env));
    }
    static private Object convertNthValue(Object cdr, Env env) {
        if (Lists.length(cdr) != 2)
            throw new ProgramException
                ("args.length must be 2: ~S.", Lists.list(cdr));
        Object n = Lists.car(cdr);
        Object mv = Lists.cadr(cdr);
        return Lists.list(Symbols.NTH_VALUE,
                          convertForm(n, env),
                          convertForm(mv, env));
    }
    static private Object convertCall(Object car, Object cdr, Env env) {
        ListBuilder lb = new ListBuilder();
        lb.append(car);
        for (Object l = cdr; !Lists.isEnd(l); l = Lists.cdr(l)) {
            lb.append(convertForm(Lists.car(l), env));
        }
        return lb.toList();
    }
    static private Object convertLambda(Object car, Env env) {
        Object vars = Lists.cadr(car);
        Object body = Lists.cddr(car);
        ListBuilder lb = new ListBuilder();
        lb.append(Symbols.LAMBDA).append(vars);
        for (Object l = body; !Lists.isEnd(l); l = Lists.cdr(l)) {
            lb.append(convertForm(Lists.car(l), env));
        }
        return lb.toList();
    }

    /*
     * macroexpander for compiler
     */

    static private Object _expandForm1(Object exp, Env env) {
        if (Logger.tracelevelp(env)) {
            Logger.trace("[expandForm1] ~S",
                         Lists.list(exp), env);
        }
        exp = cmacroExpand(exp, env);
        if (Data.isSymbol(exp)) {
            return MACRO_NOT_EXPANDED;
        }
        else if (Data.isAtom(exp)) {
            return MACRO_NOT_EXPANDED;
        }
        else if (Data.isList(exp)) {
            Object car = Lists.car(exp);
            if (car == Symbols.IF ||
                car == Symbols.AND ||
                car == Symbols.OR ||
                car == Symbols.SETQ ||
                car == Symbols.PSETQ ||
                car == Symbols.QUOTE ||
                car == Symbols.FUNCTION ||
                car == Symbols.PROGN ||
                car == Symbols.PROG ||
                car == Symbols.RETURN ||
                car == Symbols.GO ||
                car == Symbols.THROW ||
                car == Symbols.CATCH ||
                car == Symbols.UNWIND_PROTECT ||
                car == Symbols.EVAL_WHEN ||
                car == Symbols.DECLARE ||
                car == Symbols.MULTIPLE_VALUE_BIND ||
                car == Symbols.MULTIPLE_VALUE_LIST ||
                car == Symbols.MULTIPLE_VALUE_SETQ ||
                car == Symbols.NTH_VALUE) {
                return MACRO_NOT_EXPANDED;
            }
            else if (Data.isSymbol(car)) {
                Symbol sym = Data.symbol(car);
                if (env.lisp().getProp(sym, SysSymbols.C_HANDLER)
                    != Symbols.NIL) {
                    return MACRO_NOT_EXPANDED;
                }
                else {
                    Object plist = env.lisp().functions(sym);
                    Object type = Lists.car(plist);
                    Object func = Lists.cadr(plist);
                    if (type == Symbols.MACRO) {
                        return Evaluator.evalMacro
                            (Data.expr(func), exp, env);
                    }
                    else {
                        return MACRO_NOT_EXPANDED;
                    }
                }
            }
            else {
                return MACRO_NOT_EXPANDED;
            }
        }
        else {
            return MACRO_NOT_EXPANDED;
        }
    }
    static Values expandForm1(Object exp, Env env) {
        Object ret = _expandForm1(exp, env);
        if (ret == MACRO_NOT_EXPANDED) {
            Object v0 = exp;
            Object v1 = Symbols.NIL;
            return Values.currentValues().push(v0).push(v1);
        }
        else {
            Object v0 = ret;
            Object v1 = Symbols.T;
            return Values.currentValues().push(v0).push(v1);
        }
    }
    static Values expandForm(Object exp, Env env) {
        Symbol expanded = Symbols.NIL;
        while (true) {
            Object ret = _expandForm1(exp, env);
            if (ret == MACRO_NOT_EXPANDED) {
                Object v0 = exp;
                Object v1 = expanded;
                return Values.currentValues().push(v0).push(v1);
            }
            else {
                exp = ret;
                expanded = Symbols.T;
            }
        }
    }

    private FormConverter() {}
}
