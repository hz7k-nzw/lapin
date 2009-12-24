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
import lapin.function.SystemSubr;
import lapin.lang.Data;
import lapin.lang.Env;
import lapin.lang.Lists;
import lapin.lang.NotReachedException;
import lapin.lang.Symbol;
import lapin.lang.Symbols;
import lapin.lang.SysSymbols;
import lapin.lang.Values;
import lapin.util.Logger;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

final class Subrs {
    static final Symbol USE_TARGET_METHOD
        = Symbol.gensym("USE-TARGET-METHOD");
    static final Symbol USE_SUBR_FIELD
        = Symbol.gensym("USE-SUBR-FIELD");
    static final Symbol USE_CONTEXT
        = Symbol.gensym("USE-CONTEXT");

    static Values analyzeSubr(Symbol name, Object func, int nargs,
                              ContextGroup cg, Env env) {
        Symbol howToCompile;
        Method tm;
        Field sf;
        Method cm;
        Object cpv;
        Context ce;
        // tm
        if (0 <= nargs) {
            tm = getTargetMethod(func, nargs);
        }
        else {
            tm = null;
        }
        // sf
        if (tm == null) {
            sf = getSubrField(func, env);
        }
        else {
            sf = null;
        }
        // cm
        if (sf != null && 0 <= nargs) {
            cm = getCallableMethod(func, nargs);
            if (cm == null) {
                throw new NotReachedException
                    ("Subr ~S (~S) must implement Callable interface.",
                     Lists.list(name, func));
            }
        }
        else {
            cm = null;
        }
        // cpv
        if (0 <= nargs) {
            cpv = canProduceValues(func, nargs);
        }
        else {
            cpv = null;
        }
        // ce
        if (tm == null && cm == null) {
            /*
             * this calculation uses the result of pass1.
             */
            ce = cg.findContext(name, env);
        }
        else {
            ce = null;
        }
        // howToCompile
        if (tm != null) {
            howToCompile = USE_TARGET_METHOD;
        }
        else if (sf != null) {
            howToCompile = USE_SUBR_FIELD;
        }
        else if (ce != null) {
            howToCompile = USE_CONTEXT;
        }
        else {
            howToCompile = Symbols.NIL;
        }
        if (Logger.tracelevelp(env)) {
            Logger.trace("[comp:subr] name =~S",
                         Lists.list(name), env);
            Logger.trace("[comp:subr] func =~S",
                         Lists.list(func), env);
            Logger.trace("[comp:subr] nargs=~S",
                         Lists.list(Data.toFixnum(nargs)), env);
            Logger.trace("[comp:subr] howToCompile=~S",
                         Lists.list(howToCompile), env);
            Logger.trace("[comp:subr] targetMethod=~S",
                         Lists.list(tm), env);
            Logger.trace("[comp:subr] subrField=~S",
                         Lists.list(sf), env);
            Logger.trace("[comp:subr] callableMethod=~S",
                         Lists.list(cm), env);
            Logger.trace("[comp:subr] canProduceValues=~S",
                         Lists.list(cpv), env);
            Logger.trace("[comp:subr] compiledExpr=~S",
                         Lists.list(ce), env);
        }
        return Values.currentValues()
            .push(howToCompile) // how-to-compile-this-function
            .push(tm)  // target-method
            .push(sf)  // subr-field
            .push(cm)  // callable-method
            .push(cpv) // can-produce-values-p
            .push(ce); // context-of-compiled-expr
    }
    static Method getTargetMethod(Object func, int nargs) {
        if (func instanceof SystemSubr) {
            SystemSubr subr = (SystemSubr) func;
            return subr.getTargetMethod(nargs);
        }
        else {
            return null;
        }
    }
    static Field getSubrField(Object func, Env env) {
        if (func instanceof SystemSubr) {
            SystemSubr subr = (SystemSubr) func;
            Object val = env.lisp().getProp(subr, SysSymbols.SUBR_FIELD);
            if (Data.isJavaField(val))
                return Data.javaField(val);
            else
                return null;
        }
        else {
            return null;
        }
    }
    static Method getCallableMethod(Object func, int nargs) {
        Class type;
        type = Callables.getType(nargs, false, false);
        if (type != null && type.isInstance(func)) {
            return Callables.getMethod(type);
        }
        for (int i = 0; i <= nargs; i++) {
            type = Callables.getType(i, true, false);
            if (type != null && type.isInstance(func)) {
                return Callables.getMethod(type);
            }
        }
        type = Callables.getType(-1, true, true);
        if (type.isInstance(func)) {
            return Callables.getMethod(type);
        }
        return null;
    }
    static Object canProduceValues(Object func, int nargs) {
        if (func instanceof SystemSubr) {
            SystemSubr subr = (SystemSubr) func;
            return Data.toPredicate(subr.canProduceValues(nargs));
        }
        else {
            return null;
        }
    }
    private Subrs() {}
}
