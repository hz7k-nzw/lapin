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
import lapin.function.LambdaList;
import lapin.function.SystemSubr;
import lapin.lang.Data;
import lapin.lang.Env;
import lapin.lang.List;
import lapin.lang.Lists;
import lapin.lang.Symbol;
import lapin.lang.Values;
//import lapin.load.Loader;
import lapin.eval.Evaluator;
import lapin.eval.NonLocalExit;
import java.lang.reflect.Field;
import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

/**
 * This class contains various methods for manipulating classes.
 */
public final class Classes {
    // method objects needed to generate byte code.

    /** {@link Method} object that reflects {@link Lists#cons cons}. */
    static public final Method CONS
        = getMethod(Lists.class, "cons",
                    new Class[] {Object.class,Object.class});
    /** {@link Method} object that reflects {@link Lists#car car}. */
    static public final Method CAR
        = getMethod(Lists.class, "car",
                    new Class[] {Object.class});
    /** {@link Method} object that reflects {@link Lists#cdr cdr}. */
    static public final Method CDR
        = getMethod(Lists.class, "cdr",
                    new Class[] {Object.class});
    /** {@link Method} object that reflects {@link Lists#memq memq}. */
    static public final Method MEMQ
        = getMethod(Lists.class, "memq",
                    new Class[] {Object.class,Object.class});
    ///** {@link Method} object that reflects
    //    {@link Evaluator#bindArgs bindArgs}. */
    //static public final Method BIND_ARGS
    //    = getMethod(Evaluator.class, "bindArgs",
    //                new Class[] {LambdaList.class,Object.class,Env.class});
    /** {@link Method} object that reflects
        {@link Evaluator#checkMinArgs checkMinArgs}. */
    static public final Method CHECK_MIN_ARGS
        = getMethod(Evaluator.class, "checkMinArgs",
                    new Class[] {LambdaList.class,Object.class,Env.class});
    /** {@link Method} object that reflects
        {@link Evaluator#checkMaxArgs checkMaxArgs}. */
    static public final Method CHECK_MAX_ARGS
        = getMethod(Evaluator.class, "checkMaxArgs",
                    new Class[] {LambdaList.class,Object.class,Env.class});
    /** {@link Method} object that reflects
        {@link Evaluator#checkKeyArgs checkKeyArgs}. */
    static public final Method CHECK_KEY_ARGS
        = getMethod(Evaluator.class, "checkKeyArgs",
                    new Class[] {LambdaList.class,Object.class,Env.class});
    /** {@link Method} object that reflects
        {@link Evaluator#function function}. */
    static public final Method FUNCTION
        = getMethod(Evaluator.class, "function",
                    new Class[] {Object.class,Env.class});
    /** {@link Method} object that reflects
        {@link Evaluator#exitWithThrow exitWithThrow}. */
    static public final Method EXIT_WITH_THROW
        = getMethod(Evaluator.class, "exitWithThrow",
                    new Class[] {Symbol.class,Object.class,Env.class});
    /** {@link Method} object that reflects
        {@link Evaluator#valueOf valueOf}. */
    static public final Method VAL_OF_THROW
        = getMethod(Evaluator.class, "valueOf",
                    new Class[] {NonLocalExit.THROW.class,
                                 Object.class,Env.class});
    /** {@link Method} object that reflects
        {@link Data#toBoolean toBoolean}. */
    static public final Method OBJ_TO_BOOL
        = getMethod(Data.class, "toBoolean",
                    new Class[] {Object.class});
    /** {@link Method} object that reflects
        {@link Data#toInt toInt}. */
    static public final Method OBJ_TO_INT
        = getMethod(Data.class, "toInt",
                    new Class[] {Object.class});
    /** {@link Method} object that reflects
        {@link Data#toDouble toDouble}. */
    static public final Method OBJ_TO_DOUBLE
        = getMethod(Data.class, "toDouble",
                    new Class[] {Object.class});
    /** {@link Method} object that reflects
        {@link Data#toChar toChar}. */
    static public final Method OBJ_TO_CHAR
        = getMethod(Data.class, "toChar",
                    new Class[] {Object.class});
    /** {@link Method} object that reflects
        {@link Data#toPredicate toPredicate}. */
    static public final Method BOOL_TO_OBJ
        = getMethod(Data.class, "toPredicate",
                    new Class[] {boolean.class});
    /** {@link Method} object that reflects
        {@link Data#toFixnum toFixnum}. */
    static public final Method INT_TO_OBJ
        = getMethod(Data.class, "toFixnum",
                    new Class[] {int.class});
    /** {@link Method} object that reflects
        {@link Data#toFlonum toFlonum}. */
    static public final Method DOUBLE_TO_OBJ
        = getMethod(Data.class, "toFlonum",
                    new Class[] {double.class});
    /** {@link Method} object that reflects
        {@link Data#toCharacter toCharacter}. */
    static public final Method CHAR_TO_OBJ
        = getMethod(Data.class, "toCharacter",
                    new Class[] {char.class});
    /** {@link Method} object that reflects
        {@link Values#singleValue singleValue}. */
    static public final Method SINGLE_VALUE
        = getMethod(Values.class, "singleValue",
                    new Class[] {Object.class});
    /** {@link Method} object that reflects
        {@link Values#multipleValue multipleValue}. */
    static public final Method MULTIPLE_VALUE
        = getMethod(Values.class, "multipleValue",
                    new Class[] {Object.class});
    /** {@link Method} object that reflects
        {@link Values#currentValues currentValues}. */
    static public final Method CURRENT_MV
        = getMethod(Values.class, "currentValues",
                    new Class[] {});
    /** {@link Method} object that reflects {@link Values#nth nth}. */
    static public final Method NTH_VALUE
        = getMethod(Values.class, "nth",
                    new Class[] {int.class});
    /** {@link Method} object that reflects {@link Values#push push}. */
    static public final Method PUSH_VALUE
        = getMethod(Values.class, "push",
                    new Class[] {Object.class});
    /** {@link Method} object that reflects {@link Values#toList toList}. */
    static public final Method MV_TO_LIST
        = getMethod(Values.class, "toList",
                    new Class[] {});

    ///**
    // * Returns a class object for the specified <code>name</code>.
    // * When a <code>name</code> for a primitive type is specified,
    // * corresponding class object is returned.
    // * @param name Name of class
    // * @param env
    // * @return Class object 
    // * @throws RuntimeException
    // */
    //static public Class getClass(String name, Env env) {
    //    try {
    //        if (name.equals("void"))
    //            return void.class;
    //        else if (name.equals("byte"))
    //            return byte.class;
    //        else if (name.equals("short"))
    //            return short.class;
    //        else if (name.equals("int"))
    //            return int.class;
    //        else if (name.equals("long"))
    //            return long.class;
    //        else if (name.equals("float"))
    //            return float.class;
    //        else if (name.equals("double"))
    //            return double.class;
    //        else if (name.equals("char"))
    //            return char.class;
    //        else if (name.equals("boolean"))
    //            return boolean.class;
    //        else {
    //            ClassLoader cl = Loader.getClassLoader(env);
    //            return Class.forName(name, true, cl);
    //        }
    //    } catch (java.lang.Exception e) {
    //        throw new RuntimeException
    //            ("failed to get class: "+name, e);
    //    }
    //}
    /**
     * Returns a field object for the specified class object and field name.
     * @param clazz Class object
     * @param name Field name of <code>clazz</code>
     * @return Field object
     * @throws RuntimeException
     */
    static public Field getField
        (Class clazz, String name) {
        if (clazz == null)
            throw new NullPointerException("class is null");
        try {
            return clazz.getField(name);
        } catch (java.lang.Exception e) {
            throw new RuntimeException
                ("failed to get field: "+clazz.getName()+"#"+name, e);
        }
    }
    /**
     * Returns a field object for the specified class object,
     * field name and parameter types.
     * @param clazz Class object
     * @param name Method name of <code>clazz</code>
     * @param paramTypes Array of parameter type of <code>clazz</code>
     * @return Method object
     * @throws RuntimeException
     */
    static public Method getMethod
        (Class clazz, String name, Class[] paramTypes) {
        if (clazz == null)
            throw new NullPointerException("class is null");
        try {
            return clazz.getMethod(name, paramTypes);
        } catch (java.lang.Exception e) {
            throw new RuntimeException
                ("failed to get method: "+clazz.getName()+"#"+name, e);
        }
    }
    /**
     * Returns true if the specified <code>type</code> is declared
     * as <code>static</code>.
     */
    static public boolean isStatic(Class type) {
        return Modifier.isStatic(type.getModifiers());
    }
    /**
     * Returns true if the specified <code>type</code> is declared
     * as <code>final</code>.
     */
    static public boolean isFinal(Class type) {
        return Modifier.isFinal(type.getModifiers());
    }
    /**
     * Returns true if the specified member <code>m</code> is declared
     * as <code>static</code>.
     */
    static public boolean isStatic(Member m) {
        return Modifier.isStatic(m.getModifiers());
    }
    /**
     * Returns true if the specified member <code>m</code> is declared
     * as <code>final</code>.
     */
    static public boolean isFinal(Member m) {
        return Modifier.isFinal(m.getModifiers());
    }
    /**
     * Returns the size of local variable for the specified <code>type</code>.
     * @param type Class object
     * @return The size of local variable.
     *         If <code>void</code> is specified, then returns 0.
     *         Else if either <code>long</code> or <code>double</code>
     *         is specified, then returns 2.
     *         Else, returns 1.
     */
    static public int sizeOf(Class type) {
        if (type.equals(void.class))
            return 0;
        else if (type.equals(double.class) ||
                 type.equals(long.class))
            return 2;
        else
            return 1;
    }
    /**
     * Returns true if a object whose type is represented by
     * <code>type</code> is always evaluated as <code>true</code>.
     * For example, a {@link Integer} object, which represents
     * <code>fixnum</code>, is always evaluated as <code>true</code>.
     * This method is used when a form is compiled as predicate and
     * the compiler needs to know if the predicate can be performed
     * at compile time.
     */
    static public boolean alwaysTrue(Class type) {
        if (type.equals(boolean.class))
            return false;
        else if (type.isAssignableFrom(Symbol.class) ||
                 type.isAssignableFrom(List.class) ||
                 type.isAssignableFrom(Values.class))
            return false;
        else
            return true;
    }
    /**
     * Returns true if the specified <code>type</code> is
     * either <code>int</code> or {@link Integer}.
     */
    static public boolean isFixnumType(Class type) {
        return type.equals(int.class)
            || type.equals(Integer.class);
    }
    /**
     * Returns true if the specified <code>type</code> is
     * either <code>double</code> or {@link Double}.
     */
    static public boolean isFlonumType(Class type) {
        return type.equals(double.class)
            || type.equals(Double.class);
    }
    /**
     * Returns true if the specified <code>type</code> is
     * either <code>char</code> or {@link Character}.
     */
    static public boolean isCharacterType(Class type) {
        return type.equals(char.class)
            || type.equals(Character.class);
    }
    /**
     * Returns true if the specified <code>type</code> is
     * <code>boolean</code>.
     * Note that forms that return <code>boolean</code> (NOT including
     * {@link Boolean}) are considered as "predicate form" by the compiler.
     * If the predicate form is compiled at the test clause of the IF form,
     * then the return value of the predicate form (boolean) is directly
     * pushed onto the operand stack of the JVM rather than converted to
     * NIL or T.
     */
    static public boolean isPredicateType(Class type) {
        return type.equals(boolean.class);
    }
    private Classes() {}
}
