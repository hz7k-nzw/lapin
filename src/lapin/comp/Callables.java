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
//import lapin.function.Function;
import lapin.lang.Data;
import lapin.lang.Lists;
import lapin.lang.NotReachedException;
import lapin.lang.Symbol;
import lapin.lang.Symbols;
import lapin.lang.Values;
import java.lang.reflect.Method;
import java.util.Map;

/**
 * This class contains various methods for manipulating
 * <code>Callable</code> interfaces.
 * @see Callable
 * @see Callable0
 * @see Callable1
 * @see Callable2
 * @see Callable3
 * @see Callable4
 * @see Callable5
 * @see Callable0r
 * @see Callable1r
 * @see Callable2r
 * @see Callable3r
 * @see Callable4r
 * @see Callable5r
 */
public final class Callables {
    static private final Class CALLABLE_TYPE_DEFAULT = Callable.class;

    static private final Class[][] CALLABLE_TYPES = new Class[6][2];
    static {
        CALLABLE_TYPES[0][0] = Callable0.class;
        CALLABLE_TYPES[1][0] = Callable1.class;
        CALLABLE_TYPES[2][0] = Callable2.class;
        CALLABLE_TYPES[3][0] = Callable3.class;
        CALLABLE_TYPES[4][0] = Callable4.class;
        CALLABLE_TYPES[5][0] = Callable5.class;
        CALLABLE_TYPES[0][1] = Callable0r.class;
        CALLABLE_TYPES[1][1] = Callable1r.class;
        CALLABLE_TYPES[2][1] = Callable2r.class;
        CALLABLE_TYPES[3][1] = Callable3r.class;
        CALLABLE_TYPES[4][1] = Callable4r.class;
        CALLABLE_TYPES[5][1] = Callable5r.class;
    }

    /**
     * Returns the upperlimit of
     * <code>reqCount</code> + <code>optCount</code>.
     * When a expr has the LambdaList which satisfies the inequality;
     * {@link lapin.function.LambdaList#reqCount reqCount} +
     * {@link lapin.function.LambdaList#optCount optCount} &gt;
     * <code>upperlimit</code>,
     * the result of the compilation ({@link lapin.function.CompiledExpr})
     * must implement the default interface {@link Callable}.
     */
    static public int getMaxArgCount() {
        return CALLABLE_TYPES.length - 1;
    }

    /**
     * Returns a class object declared in <code>Callable</code>
     * interfaces that matches the specified spec.
     * @param nargs <code>reqCount</code> + <code>optCount</code>
     * @param rest if <code>true</code>, then the interface accepts
     *             rest or keyword parameter
     * @param allowDefault if <code>true</code>, then the default interface
     *        {@link Callable} can be returned
     * @return <code>null</code> or a class object.
     *         If the matched interface does not exist and
     *         <code>false</code> is specified for <code>allowDefault</code>,
     *         then <code>null</code> is returned.
     *         Else, a class object for either the matched interface or
     *         the default interface is returned.
     */
    static public Class getType(int nargs, boolean rest, boolean allowDefault) {
        if (0 <= nargs && nargs < CALLABLE_TYPES.length)
            return rest ? CALLABLE_TYPES[nargs][1] : CALLABLE_TYPES[nargs][0];
        else
            return allowDefault ? CALLABLE_TYPE_DEFAULT : null;
    }

    /**
     * Returns a method object declared in <code>Callable</code>
     * interfaces that matches the specified spec.
     * @param nargs <code>reqCount</code> + <code>optCount</code>
     * @param rest if <code>true</code>, then the interface accepts
     *             rest or keyword parameter
     * @return Method object declared in the matched interface
     */
    static public Method getMethod(int nargs, boolean rest) {
        return getMethod(getType(nargs, rest, true));
    }
    /**
     * Returns a method object declared in the specified
     * <code>Callable</code> interface <code>type</code>.
     * For example, if a class object for {@link Callable1 Callable1}
     * is specified as <code>type</code>, then a method object for 
     * {@link Callable1#call1 call1} is returned.
     * @param type Class object which represents a member of 
     *        the <code>Callable</code> interface families
     * @return Method object that is declared in <code>type</code>
     */
    static public Method getMethod(Class type) {
        if (type.equals(CALLABLE_TYPE_DEFAULT)) {
            return findMethod(type, "call");
        }
        for (int i = 0; i < CALLABLE_TYPES.length; i++) {
            for (int j = 0; j < 2; j++) {
                if (type.equals(CALLABLE_TYPES[i][j])) {
                    return findMethod(type, "call"+i);
                }
            }
        }
        throw new NotReachedException
            ("unknown type specified: ~S", Lists.list(type));
    }
    static private Method findMethod(Class type, String name) {
        Method[] methods = type.getMethods();
        for (int i = 0; i < methods.length; i++) {
            Method m = methods[i];
            if (m.getName().equals(name))
                return m;
        }
        throw new NotReachedException
            ("method ~S not found in ~S", Lists.list(name, type));
    }
    /**
     * Returns a information abount the specified interface.
     * @param type Class object which represents a member of 
     *        the <code>Callable</code> interface families
     * @return 2 values; fixnum and flag.
     *         If the default interface {@link Callable Callable} is specified,
     *         then this method returns <code>(-1 NIL)</code>.
     *         if other than the default interface is specified,
     *         then this method returns
     *         <code>(reqCount + optCount flag)</code>,
     *         where the value of flag is T if <code>type</code> accepts
     *         rest or keyword parameter and NIL otherwise.
     */
    static public Values getTypeInfo(Class type) {
        if (type.equals(CALLABLE_TYPE_DEFAULT)) {
            Object v0 = Data.toFixnum(-1);
            Object v1 = Symbols.NIL;
            return Values.currentValues().push(v0).push(v1);
        }
        for (int i = 0; i < CALLABLE_TYPES.length; i++) {
            for (int j = 0; j < 2; j++) {
                if (type.equals(CALLABLE_TYPES[i][j])) {
                    Object v0 = Data.toFixnum(i);
                    Object v1 = Data.toPredicate(j == 1);
                    return Values.currentValues().push(v0).push(v1);
                }
            }
        }
        throw new NotReachedException
            ("unknown type specified: ~S", Lists.list(type));
    }

    /** Uninterned symbol which represents the slot of the local
        variable "this". */
    static public final Symbol LOCAL_SLOT_THIS
        = Symbol.gensym("SLOT-THIS");
    /** Uninterned symbol which represents the slot of the local
        variable "args" (first parameter of {@link Callable#call call}). */
    static public final Symbol LOCAL_SLOT_ARGS
        = Symbol.gensym("SLOT-ARGS");
    /** Uninterned symbol which represents the slot of the local
        variable "env". */
    static public final Symbol LOCAL_SLOT_ENV
        = Symbol.gensym("SLOT-ENV");

    static private final Symbol[] LOCAL_SLOT_ARG_ARRAY
        = new Symbol[getMaxArgCount()];
    static {
        for (int i = 0; i< LOCAL_SLOT_ARG_ARRAY.length; i++)
            LOCAL_SLOT_ARG_ARRAY[i] = Symbol.gensym("SLOT-ARG-"+i);
    }

    /** Returns an uninterned symbol which represents
        the slot of the local variable "arg[i]"
        (ith parameter of the Callable[N]#call[N]). */
    static public Symbol getArgSlot(int i) {
        return LOCAL_SLOT_ARG_ARRAY[i];
    }

    private Callables() {}
}
