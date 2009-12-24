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
package lapin.function;
import lapin.comp.DeclInfo;
import lapin.lang.Data;
import lapin.lang.Env;
import lapin.lang.Lists;
import lapin.lang.Symbols;
import lapin.lang.Values;

/**
 * Base class for the interpreted functions.
 */
public class Expr extends Function {
    /** LambdaList of the function. */
    private final LambdaList lambdaList;
    /** decls of the function */
    private final Object decls;
    /** body of the function */
    private final Object body;
    /**
     * Constructs a interpreted function.
     * @param name Name of this function
     * @param vars Parameter spec of this function
     * @param body Body of this function
     * @param env Environment used to create this function
     */
    public Expr(String name, Object vars, Object body, Env env) {
        super(name);
        if (vars == null)
            throw new NullPointerException("vars is null");
        if (body == null)
            throw new NullPointerException("body is null");
        if (env == null)
            throw new NullPointerException("env is null");
        // vars -> lambdaList
        this.lambdaList = new LambdaList(vars, env);
        // body -> decls and body
        {
            Values mv = DeclInfo.findDecls(body);
            this.decls = mv.nth(0);
            this.body = mv.nth(1);
        }
    }
    /** Returns the parameter spec used by this function. */
    public final LambdaList lambdaList() {
        return lambdaList;
    }
    /** Returns the declaration forms of this function. */
    public final Object decls() {
        return decls;
    }
    /** Returns the body of this function. */
    public final Object body() {
        return body;
    }
    //public final String toString() {
    //    Object vars = lambdaList().params();
    //    Object body = body();
    //    return name()+": "+Lists.list2(Symbols.LAMBDA, vars, body);
    //}
}
