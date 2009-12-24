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
import lapin.function.LambdaList;
import lapin.function.Subr;
import lapin.io.Reader;
import lapin.lang.Env;
import lapin.lang.Symbols;

/**
 * This class is a subclass of {@link Subr}
 * which is used when {@link Expr} is compiled.
 */
public abstract class CompiledExpr extends Subr {
    protected CompiledExpr(String name) {
        super(name);
    }
    /**
     * Read the specified string and returns a corresponding
     * lisp object.
     * @param s String which represents a lisp object
     * @return Result of read
     * @see Reader#readFromString
     */
    protected Object toSexp(String s, Env env) {
        return Reader.readFromString
            (s, Symbols.T, Symbols.NIL, Symbols.NIL, env);
    }
    /**
     * Read the specified string and returns a corresponding
     * {@link LambdaList}.
     * @param s String which represents a lisp object
     * @return Result of read
     */
    protected LambdaList toLambdaList(String s, Env env) {
        return new LambdaList(toSexp(s, env), env);
    }
    public String toString() {
        return "COMPILED-EXPR:"+super.name();
    }
}
