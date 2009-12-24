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

/**
 * This interface is used to access the intermediate code.
 * @see ByteCodeGenerator
 */
public interface MethodInfo {
    /** Return the owner of this <code>MethodInfo</code>. */
    ClassInfo classInfo();
    /** Returns the number of argument that the method
        represented by this <code>MethodInfo</code> accepts.
        <code>nargs</code> must satisfy the following inequality;
        <pre>
        reqCount &lt;= nargs &amp;&amp; nargs &lt;= reqCount + optCount
        </pre> */
    int nargs();
    /** Returns true
        if the method represented by this <code>MethodInfo</code>
        accepts rest or keyword parameters. */
    boolean rest();
    /** Returns true if the method represented by this
        <code>MethodInfo</code> is an implementation of
        a method declared in the <code>Callable</code>
        interface families. */
    boolean implCallable();
    /** Returns the name of this method.
        If this <code>MethodInfo</code> satisfies that
        {@link #nargs nargs} returns 1,
        {@link #rest rest} returns true and 
        {@link #implCallable implCallable} returns true,
        then this method returns "call1", which is a name of
        a method declared in {@link lapin.function.Callable1r}.
        If this <code>MethodInfo</code> satisfies that
        {@link #nargs nargs} returns 1,
        {@link #rest rest} returns true and
        {@link #implCallable implCallable} returns false,
        then this method returns "_call1". */
    String name();
    /** Returns the parameter types of the method
        represented by this <code>MethodInfo</code>. */
    Class[] paramTypes();
    /** Returns the return type of the method
        represented by this <code>MethodInfo</code>. */
    Class retType();

    /** Returns the count of the instructions. */
    int instLen();
    /** Returns the instruction at the specified index. */
    Object getInst(int i);
    /** Replaces the instruction at the specified index. */
    Object setInst(int i, Object inst);
    /** Appends the instruction to the end. */
    void addInst(Object inst);
    /** Inserts the instruction at the specified index. */
    void addInst(int i, Object inst);
    /** Removes the instruction at the specified index. */
    Object removeInst(int i);
}
