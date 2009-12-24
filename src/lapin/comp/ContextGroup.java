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
import lapin.lang.Env;
import lapin.lang.Lists;
import lapin.lang.ProgramException;
import lapin.lang.Symbol;
import lapin.lang.Symbols;
import lapin.util.Logger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

final class ContextGroup {
    // uninterned symbol: for private use only
    static private final Symbol ENV_KEY = Symbol.gensym("_CONTEXT-GROUP_");

    static ContextGroup get(Env env) {
        return (ContextGroup) env.get(ENV_KEY);
    }
    static void bind(ContextGroup cg, Env env) {
        env.bind(ENV_KEY, cg);
    }

    String javapkg;
    int funId;
    boolean dumpInsts;
    boolean genByteCode;
    boolean genClassFile;
    boolean registCompiledExpr;
    boolean optimize;

    final HashMap declMap = new HashMap();
    final ArrayList contexts = new ArrayList();

    ContextGroup() {
        declMap.put(Symbols.SPECIAL, Symbols.NIL);
        declMap.put(Symbols.FIXNUM, Symbols.NIL);
        declMap.put(Symbols.FLONUM, Symbols.NIL);
        declMap.put(Symbols.CHARACTER, Symbols.NIL);
    }
    ContextGroup pushVars(Symbol key, Object vars) {
        Object pushedVars = declMap.get(key);
        if (pushedVars == null)
            throw new ProgramException
                ("unsupported type: ~S", Lists.list(key));
        for (Object l = vars; !Lists.isEnd(l); l = Lists.cdr(l)) {
            Object var = Lists.car(l);
            if (Lists.memq(var, pushedVars) == Symbols.NIL)
                pushedVars = Lists.cons(var, pushedVars);
        }
        declMap.put(key, pushedVars);
        return this;
    }
    Context genContext(Symbol name, Expr expr, Env env) {
        // javapkg
        String javapkg = this.javapkg;
        // funId
        String funId = String.valueOf(this.funId++);
        // convert global decls to DeclInfo
        Object specialVars = declMap.get(Symbols.SPECIAL);
        Object fixnumVars = declMap.get(Symbols.FIXNUM);
        Object flonumVars = declMap.get(Symbols.FLONUM);
        Object characterVars = declMap.get(Symbols.CHARACTER);
        DeclInfo di = env.lisp().getDeclInfo()
            // create child DeclInfo for the expr
            .child(name, expr.lambdaList())
            // push default func type
            .pushFuncType(Lists.list(name), Symbols.T)
            .pushSpecialVars(specialVars)
            .pushVarTypes(fixnumVars, Symbols.FIXNUM)
            .pushVarTypes(flonumVars, Symbols.FLONUM)
            .pushVarTypes(characterVars, Symbols.CHARACTER);
        // make new context, in which intermediate code is
        // accumulated
        Context ctx = new Context(javapkg, funId, name, expr, di, this);
        this.contexts.add(ctx);
        return ctx;
    }
    Context findContext(Symbol sym, Env env) {
        Context ret = null;
        for (Iterator it = contexts.iterator(); it.hasNext();) {
            Context ctx = (Context) it.next();
            if (sym == ctx.name()) {
                ret = ctx;
                break;
            }
        }
        if (Logger.debuglevelp(env))
            Logger.debug("[findContext] ~S => ~S",
                         Lists.list(sym, ret), env);
        return ret;
    }
}
