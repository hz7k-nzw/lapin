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
import lapin.function.Expr;
import lapin.lang.Data;
import lapin.lang.Env;
import lapin.lang.Lists;
import lapin.lang.Symbol;
import lapin.lang.Symbols;
import lapin.lang.Values;
import lapin.util.Logger;

/**
 * Functions for macro expansion.
 */
public final class Macro {
    // uninterned symbols: for private use only
    static private final Symbol MACRO_NOT_EXPANDED
        = Symbol.gensym("_MACRO-NOT-EXPANDED_");

    static private Object _expand1(Object exp, Env env) {
        if (Logger.tracelevelp(env)) {
            Logger.trace("[expand1] ~S",
                         Lists.list(exp), env);
        }
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
                Object plist = env.lisp().functions(sym);
                Object type = Lists.car(plist);
                Object func = Lists.cadr(plist);
                if (type == Symbols.MACRO) {
                    return Evaluator.evalMacro(Data.expr(func), exp, env);
                }
                else {
                    return MACRO_NOT_EXPANDED;
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
    /**
     * Executes MACROEXPAND-1.
     * @param exp form to be expanded
     * @param env
     * @return 2 values; the result of the macro expansion and
     *         a flag which indicates whether the specified
     *         form is expanded or not
     */
    static public Values expand1(Object exp, Env env) {
        Object ret = _expand1(exp, env);
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
    /**
     * Executes MACROEXPAND.
     * @param exp form to be expanded
     * @param env
     * @return 2 values; the result of the macro expansion and
     *         a flag which indicates whether the specified
     *         form is expanded or not
     */
    static public Values expand(Object exp, Env env) {
        Symbol expanded = Symbols.NIL;
        while (true) {
            Object ret = _expand1(exp, env);
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
    private Macro() {}
}
