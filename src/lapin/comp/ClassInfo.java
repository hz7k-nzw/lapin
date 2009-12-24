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
import lapin.lang.Symbol;
import java.util.Iterator;

/**
 * This interface is used to access the intermediate code.
 * @see ByteCodeGenerator
 */
public interface ClassInfo {
    /** Returns the expr to be compiled. */
    Expr expr();
    /** Returns a name assigned to the expr. */
    Symbol name();
    /** Returns a class name assigned to
        a compilation result of this <code>ClassInfo</code>. */
    String classname();
    /** Returns a iterator of {@link MethodInfo}
        stored in this <code>ClassInfo</code>. */
    Iterator methodIterator();
}
