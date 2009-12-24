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
//import lapin.eval.Macro;
import lapin.function.Function;
import lapin.lang.Data;
import lapin.lang.Env;
import lapin.lang.List;
import lapin.lang.Lists;
import lapin.lang.NotReachedException;
import lapin.lang.ProgramException;
import lapin.lang.Symbol;
import lapin.lang.Symbols;
import lapin.lang.Values;
import lapin.util.Logger;
import java.lang.reflect.Method;

final class TypeCalculator {
    // uninterned symbol: for private use only
    static private final Symbol DECL_INFO = Symbol.gensym("_DECL-INFO_");

    static private DeclInfo getDecl(Env env) {
        return (DeclInfo) env.get(DECL_INFO);
    }

    static Object typeOfArgs(Object args, Env env) {
        Object types = Symbols.NIL;
        for (Object l = args; !Lists.isEnd(l); l = Lists.cdr(l)) {
            types = Lists.cons(typeOf(Lists.car(l), env), types);
        }
        return Lists.nreverse(types);
    }
    static Class typeOf(Object exp, Env env) {
        /*
         * typeOf depends on the context
         * on which the exp is to be compiled
         */
        DeclInfo di = Context.get(env).getDecl();
        Class type;
        {
            Env newenv = env.child();
            newenv.bind(DECL_INFO, di);
            type = typeOfForm(exp, newenv);
        }
        if (Logger.debuglevelp(env))
            Logger.debug("[typeOf] ~S => ~S",
                         Lists.list(exp, type), env);
        return type;
    }
    static private Class typeOfForm(Object exp, Env env) {
        // for symbols
        if (Data.isSymbol(exp)) {
            Symbol sym = Data.symbol(exp);
            if (sym.isSelfeval())
                return typeOfAtom(sym, env);
            else
                return typeOfSymbol(sym, env);
        }
        // for other atoms
        else if (Data.isAtom(exp)) {
            return typeOfAtom(exp, env);
        }
        // for lists
        else if (Data.isList(exp)) {
            Object car = Lists.car(exp);
            Object cdr = Lists.cdr(exp);
            if (car == Symbols.IF) {
                return typeOfIf(cdr, env);
            }
            else if (car == Symbols.AND) {
                return typeOfAnd(cdr, env);
            }
            else if (car == Symbols.OR) {
                return typeOfOr(cdr, env);
            }
            else if (car == Symbols.SETQ) {
                return typeOfSetq(cdr, env);
            }
            else if (car == Symbols.PSETQ) {
                return typeOfPsetq(cdr, env);
            }
            else if (car == Symbols.QUOTE) {
                return typeOfQuote(cdr, env);
            }
            else if (car == Symbols.FUNCTION) {
                return typeOfFunction(cdr, env);
            }
            else if (car == Symbols.PROGN) {
                return typeOfProgn(cdr, env);
            }
            else if (car == Symbols.PROG) {
                return typeOfProg(cdr, env);
            }
            else if (car == Symbols.RETURN) {
                return typeOfReturn(cdr, env);
            }
            else if (car == Symbols.GO) {
                return typeOfGo(cdr, env);
            }
            else if (car == Symbols.THROW) {
                return typeOfThrow(cdr, env);
            }
            else if (car == Symbols.CATCH) {
                return typeOfCatch(cdr, env);
            }
            else if (car == Symbols.UNWIND_PROTECT) {
                return typeOfUnwindProtect(cdr, env);
            }
            else if (car == Symbols.EVAL_WHEN) {
                return typeOfEvalWhen(cdr, env);
            }
            else if (car == Symbols.DECLARE) {
                return typeOfDeclare(cdr, env);
            }
            else if (car == Symbols.MULTIPLE_VALUE_BIND) {
                return typeOfMultipleValueBind(cdr, env);
            }
            else if (car == Symbols.MULTIPLE_VALUE_LIST) {
                return typeOfMultipleValueList(cdr, env);
            }
            else if (car == Symbols.MULTIPLE_VALUE_SETQ) {
                return typeOfMultipleValueSetq(cdr, env);
            }
            else if (car == Symbols.NTH_VALUE) {
                return typeOfNthValue(cdr, env);
            }
            else if (Data.isSymbol(car)) {
                Symbol sym = Data.symbol(car);
                Object plist = env.lisp().functions(sym);
                Object type = Lists.car(plist);
                Object func = Lists.cadr(plist);
                if (type == Symbols.MACRO)
                    throw new NotReachedException
                        ("unprocessed macro: ~S exp: ~S",
                         Lists.list(sym, exp));
                else
                    return typeOfCall(sym, func, cdr, env);
            }
            else if (Data.isList(car)) {
                Object caar = Lists.car(car);
                if (caar == Symbols.LAMBDA) {
                    return typeOfLambda(car, cdr, env);
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
    static private Class typeOfSymbol(Symbol sym, Env env) {
        return getDecl(env).javaTypeOfVar(sym, true);
    }
    static private Class typeOfAtom(Object atom, Env env) {
        throw new NotReachedException
            ("atom must be quoted : ~S", Lists.list(atom));
    }
    static private Class typeOfIf(Object cdr, Env env) {
        if (Lists.length(cdr) <= 1)
            throw new ProgramException
                ("args.length must be greater than 1: ~S.",
                 Lists.list(cdr));
        Object test = Lists.car(cdr);
        Object  con = Lists.cadr(cdr);
        Object  alt = Lists.caddr(cdr);
        if (Forms.isNil(test)) {
            // test is NIL
            return typeOfForm(alt, env);
        }
        else if (Forms.isConstant(test)) {
            // test is an atom (not a variable)
            return typeOfForm(con, env);
        }
        else {
            return typeOfAllForms(Lists.cdr(cdr), env);
        }
    }
    static private Class typeOfAnd(Object cdr, Env env) {
        switch (Lists.length(cdr)) {
        case 0:
            return typeOfForm(Forms.T, env);
        case 1:
            return typeOfForm(Lists.car(cdr), env);
        default:
            return typeOfAllForms(cdr, env);
        }
    }
    static private Class typeOfOr(Object cdr, Env env) {
        switch (Lists.length(cdr)) {
        case 0:
            return typeOfForm(Forms.NIL, env);
        case 1:
            return typeOfForm(Lists.car(cdr), env);
        default:
            return typeOfAllForms(cdr, env);
        }
    }
    static private Class typeOfSetq(Object cdr, Env env) {
        return typeOfLastAssignment(cdr, env);
    }
    static private Class typeOfPsetq(Object cdr, Env env) {
        return typeOfLastAssignment(cdr, env);
    }
    static private Class typeOfQuote(Object cdr, Env env) {
        if (Lists.length(cdr) != 1)
            throw new ProgramException
                ("args.length must be 1: ~S.", Lists.list(cdr));
        Object atom = Lists.car(cdr);
        if (atom == null)
            return Object.class;
        else
            return atom.getClass();
    }
    static private Class typeOfFunction(Object cdr, Env env) {
        return Function.class;
    }
    static private Class typeOfProgn(Object cdr, Env env) {
        return typeOfLastForm(cdr, env);
    }
    static private Class typeOfProg(Object cdr, Env env) {
        // XXX
        return Object.class;
    }
    static private Class typeOfGo(Object cdr, Env env) {
        // XXX
        return Object.class;
    }
    static private Class typeOfReturn(Object cdr, Env env) {
        // XXX
        return Object.class;
    }
    static private Class typeOfThrow(Object cdr, Env env) {
        // XXX
        return Object.class;
    }
    static private Class typeOfCatch(Object cdr, Env env) {
        if (Lists.length(cdr) <= 0)
            throw new ProgramException
                ("args.length must be greater than 0: ~S.",
                 Lists.list(cdr));
        Object tagspec = Lists.car(cdr);
        Object forms = Lists.cdr(cdr);
        return typeOfLastForm(forms, env);
    }
    static private Class typeOfUnwindProtect(Object cdr, Env env) {
        if (Lists.length(cdr) < 1)
            throw new ProgramException
                ("args.length must be greater than 0: ~S.",
                 Lists.list(cdr));
        Object form = Lists.car(cdr);
        return typeOfForm(form, env);
    }
    static private Class typeOfEvalWhen(Object cdr, Env env) {
        if (Lists.length(cdr) <= 0)
            throw new ProgramException
                ("args.length must be greater than 0: ~S.",
                 Lists.list(cdr));
        Object body = Lists.cdr(cdr);
        return typeOfLastForm(body, env);
    }
    static private Class typeOfDeclare(Object cdr, Env env) {
        return Object.class;
    }
    //static private Class typeOfMultipleValueBind(Object cdr, Env env) {
    //    if (Lists.length(cdr) <= 1)
    //        throw new ProgramException
    //            ("args.length must be greater than 1: ~S.",
    //             Lists.list(cdr));
    //    Object body = Lists.cddr(cdr);
    //    return typeOfLastForm(body, env);
    //}
    static private Class typeOfMultipleValueBind(Object cdr, Env env) {
        if (Lists.length(cdr) <= 1)
            throw new ProgramException
                ("args.length must be greater than 1: ~S.",
                 Lists.list(cdr));
        Object declsAndBody = Lists.cddr(cdr);
        Object decls,body;
        {
            Values mv = DeclInfo.findDecls(declsAndBody);
            decls = mv.nth(0);
            body = mv.nth(1);
        }
        DeclInfo di = getDecl(env).child().pushDecls(decls);
        {
            Env newenv = env.child();
            newenv.bind(DECL_INFO, di);
            return typeOfLastForm(body, newenv);
        }
    }
    static private Class typeOfMultipleValueList(Object cdr, Env env) {
        return List.class;
    }
    static private Class typeOfMultipleValueSetq(Object cdr, Env env) {
        if (Lists.length(cdr) != 2)
            throw new ProgramException
                ("args.length must be 2: ~S.", Lists.list(cdr));
        Object vars = Lists.car(cdr);
        if (Lists.isEnd(vars))
            return Object.class;
        else
            return typeOfSymbol(Data.symbol(Lists.car(vars)), env);
    }
    static private Class typeOfNthValue(Object cdr, Env env) {
        return Object.class;
    }
    //static private Class typeOfCall(Symbol sym, Object func,
    //                                Object cdr, Env env) {
    //    int nargs = Lists.length(cdr);
    //    Method tm = Subrs.getTargetMethod(func, nargs);
    //    if (tm != null) {
    //        return tm.getReturnType();
    //    }
    //    else {
    //        return Object.class;
    //    }
    //}
    static private Class typeOfCall(Symbol sym, Object func,
                                    Object cdr, Env env) {
        int nargs = Lists.length(cdr);
        ContextGroup cg = ContextGroup.get(env);
        // analyzes the function object
        // and collects compilation informations.
        Values mv = Subrs.analyzeSubr(sym, func, nargs, cg, env);
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
            return Data.javaMethod(tm).getReturnType();
        }
        else if (howToCompile == Subrs.USE_SUBR_FIELD) {
            return Data.javaMethod(cm).getReturnType();
        }
        else if (howToCompile == Subrs.USE_CONTEXT) {
            Context ctx2 = (Context) ce;
            Context.MethodInfo mi = ctx2.findMethod(nargs, false, env);
            return mi.retType();
        }
        else {
            return Object.class;
        }
    }
    static private Class typeOfLambda(Object car, Object cdr, Env env) {
        Object declsAndBody = Lists.cddr(car);
        Object decls,body;
        {
            Values mv = DeclInfo.findDecls(declsAndBody);
            decls = mv.nth(0);
            body = mv.nth(1);
        }
        DeclInfo di = getDecl(env).child().pushDecls(decls);
        {
            Env newenv = env.child();
            newenv.bind(DECL_INFO, di);
            return typeOfLastForm(body, newenv);
        }
    }

    static private Class typeOfAllForms(Object cdr, Env env) {
        if (cdr == Symbols.NIL)
            return Object.class;
        Object l = cdr;
        Class type = typeOfForm(Lists.car(l), env);
        if (Data.isEqual(type, Object.class))
            return Object.class;
        while (true) {
            l = Lists.cdr(l);
            if (Lists.isEnd(l))
                return type;
            if (!Data.isEqual(type, typeOfForm(Lists.car(l), env)))
                return Object.class;
        }
    }
    static private Class typeOfLastForm(Object cdr, Env env) {
        Class type = Object.class;
        for (Object l = cdr; !Lists.isEnd(l); l = Lists.cdr(l)) {
            if (Lists.isEnd(Lists.cdr(l))) {
                type = typeOfForm(Lists.car(l), env);
            }
        }
        return type;
    }
    static private Class typeOfLastAssignment(Object cdr, Env env) {
        int len = Lists.length(cdr);
        if (len%2 == 1)
            throw new ProgramException
                ("odd number of arguments: ~S.", Lists.list(cdr));
        Class type = Object.class;
        for (Object l = cdr; !Lists.isEnd(l); l = Lists.cddr(l)) {
            if (Lists.isEnd(Lists.cddr(l))) {
                Symbol var = Data.symbol(Lists.car(l));
                type = typeOfSymbol(var, env);
            }
        }
        return type;
    }

    private TypeCalculator() {}
}
