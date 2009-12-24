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
package lapin.lang;
import lapin.function.Function;
import lapin.function.Expr;
import lapin.function.LambdaList;
import lapin.function.Subr;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.math.BigInteger;

/**
 * This class contains various methods for manipulating data types.
 */
public final class Data {
    /*
     * char cache
     */

    static private final int chars_limit = 128;
    static private final Character[] chars = new Character[chars_limit];
    static {
        for (int i = 0; i < chars_limit; i++) {
            chars[i] = new Character((char) i);
        }
    }

    /*
     * fixnum cache
     */

    static private final int fixnums_limit
        = 1 << Constants.FIXNUM_CACHE_POW.intValue();
    static private final Integer[] fixnums = new Integer[fixnums_limit << 1];
    static {
        int min,max;
        // positive region: [0, 2^N - 1]
        min = 0; max = fixnums_limit;
        for (int i = min; i < max; i++) {
            //System.out.println("cache:positive: "+i);
            fixnums[i] = new Integer(i);
        }
        // negative region: [-2^N, -1]
        min = fixnums_limit; max = fixnums.length;
        for (int i = min; i < max; i++) {
            //System.out.println("cache:negative: "+(i - max));
            fixnums[i] = new Integer(i - max);
        }
    }

    static private final Integer fixnum_zero = toFixnum(0);
    static private final Integer fixnum_one = toFixnum(1);

    /*
     * flonum cache
     */

    static private final Double flonum_zero = new Double(0.0);
    static private final Double flonum_one = new Double(1.0);

    /*
     * predicates for data types
     */

    ///**
    // * Returns the symbol which specifies the type of o. 
    // */
    //static public Symbol typep(Object o) {
    //    if (isSymbol(o)) {
    //        return Symbols.SYMBOL;
    //    }
    //    else if (isPair(o)) {
    //        return Symbols.LIST;
    //    }
    //    else if (isFixnum(o)) {
    //        return Symbols.FIXNUM;
    //    }
    //    else if (isFlonum(o)) {
    //        return Symbols.FLONUM;
    //    }
    //    else if (isBignum(o)) {
    //        return Symbols.BIGNUM;
    //    }
    //    else if (isCharacter(o)) {
    //        return Symbols.CHARACTER;
    //    }
    //    else if (isString(o)) {
    //        return Symbols.STRING;
    //    }
    //    else if (isArray(o)) {
    //        return Symbols.ARRAY;
    //    }
    //    else if (isHunk(o)) {
    //        return Symbols.HUNK;
    //    }
    //    else {
    //        return Symbols.RANDOM;
    //    }
    //}
    /**
     * Returns true if its argument is NIL (the empty list);
     * otherwise returns false.
     */
    static public boolean isNot(Object o) {
        return o == Symbols.NIL;
    }
    /**
     * Returns true if <code>o</code> is an atom, and false otherwise. 
     */
    static public boolean isAtom(Object o) {
        if (isPair(o)) {
            return false;
        }
        else {
            return true;
        }
    }
    /**
     * Returns true if <code>o</code> is a {@link List list},
     * and false otherwise. 
     */
    static public boolean isList(Object o) {
        return o instanceof List;
    }
    /**
     * Returns true if <code>o</code> is a {@link Symbol symbol},
     * and false otherwise. 
     */
    static public boolean isSymbol(Object o) {
        return o instanceof Symbol;
    }
    /**
     * Returns true if <code>o</code> is a {@link Pair pair},
     * and false otherwise. 
     */
    static public boolean isPair(Object o) {
        return o instanceof Pair;
    }
    /**
     * Returns true if <code>o</code> is a {@link Integer fixnum},
     * and false otherwise. 
     */
    static public boolean isFixnum(Object o) {
        return o instanceof Integer;
    }
    /**
     * Returns true if <code>o</code> is a {@link Double flonum},
     * and false otherwise. 
     */
    static public boolean isFlonum(Object o) {
        return o instanceof Double;
    }
    /**
     * Returns true if <code>o</code> is a {@link BigInteger bignum},
     * and false otherwise. 
     */
    static public boolean isBignum(Object o) {
        return o instanceof BigInteger;
    }
    /**
     * Returns true if <code>o</code> is an instance of {@link Boolean}, 
     * and false otherwise. 
     */
    static public boolean isJavaBoolean(Object o) {
        return o instanceof Boolean;
    }
    /**
     * Returns true if <code>o</code> is an instance of {@link Number}, 
     * and false otherwise. 
     */
    static public boolean isJavaNumber(Object o) {
        return o instanceof Number;
    }
    /**
     * Returns true if <code>o</code> is a {@link Character character},
     * and false otherwise. 
     */
    static public boolean isCharacter(Object o) {
        return o instanceof Character;
    }
    /**
     * Returns true if <code>o</code> is a {@link String string},
     * and false otherwise. 
     */
    static public boolean isString(Object o) {
        return o instanceof String;
    }
    /**
     * Returns true if <code>o</code> is an instance of {@link Function}, 
     * and false otherwise. 
     */
    static public boolean isFunction(Object o) {
        return o instanceof Function;
    }
    /**
     * Returns true if <code>o</code> is an instance of {@link Subr}, 
     * and false otherwise. 
     */
    static public boolean isSubr(Object o) {
        return o instanceof Subr;
    }
    /**
     * Returns true if <code>o</code> is an instance of {@link Expr}, 
     * and false otherwise. 
     */
    static public boolean isExpr(Object o) {
        return o instanceof Expr;
    }
    /**
     * Returns true if <code>o</code> is an instance of {@link Array}, 
     * and false otherwise. 
     */
    static public boolean isArray(Object o) {
        return o instanceof Array;
    }
    /**
     * Returns true if <code>o</code> is an instance of {@link Hunk}, 
     * and false otherwise. 
     */
    static public boolean isHunk(Object o) {
        return o instanceof Hunk;
    }
    /**
     * Returns true if <code>o</code> is an instance of {@link LambdaList}, 
     * and false otherwise. 
     */
    static public boolean isLambdaList(Object o) {
        return o instanceof LambdaList;
    }
    ///**
    // * Returns true if <code>o</code> is an instance of {@link Env}, 
    // * and false otherwise.
    // */
    //static public boolean isEnv(Object o) {
    //    return o instanceof Env;
    //}
    ///**
    // * Returns true if <code>o</code> is an instance of {@link Values}, 
    // * and false otherwise.
    // */
    //static public boolean isValues(Object o) {
    //    return o instanceof Values;
    //}
    /**
     * Returns true if <code>o</code> is a {@link Package package},
     * and false otherwise. 
     */
    static public boolean isPkg(Object o) {
        return o instanceof Package;
    }
    /**
     * Returns true if <code>o</code> is an instance of {@link Prop},
     * and false otherwise. 
     */
    static public boolean isProp(Object o) {
        return o instanceof Prop;
    }
    /**
     * Returns true if <code>o</code> is an instance of {@link InputStream}, 
     * and false otherwise. 
     */
    static public boolean isInputStream(Object o) {
        return o instanceof InputStream;
    }
    /**
     * Returns true if <code>o</code> is an instance of {@link OutputStream}, 
     * and false otherwise. 
     */
    static public boolean isOutputStream(Object o) {
        return o instanceof OutputStream;
    }
    /**
     * Returns true if <code>o</code> is an instance of {@link Reader}, 
     * and false otherwise. 
     */
    static public boolean isReader(Object o) {
        return o instanceof Reader;
    }
    /**
     * Returns true if <code>o</code> is an instance of {@link Writer}, 
     * and false otherwise. 
     */
    static public boolean isWriter(Object o) {
        return o instanceof Writer;
    }
    /**
     * Returns true if <code>o</code> is an instance of {@link File}, 
     * and false otherwise. 
     */
    static public boolean isFile(Object o) {
        return o instanceof File;
    }
    /**
     * Returns true if <code>o</code> is an instance of {@link Class}, 
     * and false otherwise. 
     */
    static public boolean isJavaClass(Object o) {
        return o instanceof Class;
    }
    /**
     * Returns true if <code>o</code> is an instance of {@link Field}, 
     * and false otherwise. 
     */
    static public boolean isJavaField(Object o) {
        return o instanceof Field;
    }
    /**
     * Returns true if <code>o</code> is an instance of {@link Method}, 
     * and false otherwise. 
     */
    static public boolean isJavaMethod(Object o) {
        return o instanceof Method;
    }
    /**
     * Returns true if <code>o</code> is an instance of {@link ClassLoader}, 
     * and false otherwise. 
     */
    static public boolean isClassLoader(Object o) {
        return o instanceof ClassLoader;
    }

    /*
     * narrowing reference conversions
     */

    static public List list(Object o) {
        try {
            return (List) o;
        }
        catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, List.class);
        }
    }
    static public Symbol symbol(Object o) {
        try {
            return (Symbol) o;
        }
        catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, Symbol.class);
        }
    }
    static public Pair pair(Object o) {
        try {
            return (Pair) o;
        }
        catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, Pair.class);
        }
    }
    static public Integer fixnum(Object o) {
        try {
            return (Integer) o;
        }
        catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, Integer.class);
        }
    }
    static public Double flonum(Object o) {
        try {
            return (Double) o;
        }
        catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, Double.class);
        }
    }
    static public BigInteger bignum(Object o) {
        try {
            return (BigInteger) o;
        }
        catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, BigInteger.class);
        }
    }
    static public Boolean javaBoolean(Object o) {
        try {
            return (Boolean) o;
        }
        catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, Boolean.class);
        }
    }
    static public Number javaNumber(Object o) {
        try {
            return (Number) o;
        }
        catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, Number.class);
        }
    }
    static public Character character(Object o) {
        try {
            return (Character) o;
        }
        catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, Character.class);
        }
    }
    static public String string(Object o) {
        try {
            return (String) o;
        }
        catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, String.class);
        }
    }
    static public Function function(Object o) {
        try {
            return (Function) o;
        }
        catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, Function.class);
        }
    }
    static public Subr subr(Object o) {
        try {
            return (Subr) o;
        }
        catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, Subr.class);
        }
    }
    static public Expr expr(Object o) {
        try {
            return (Expr) o;
        }
        catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, Expr.class);
        }
    }
    static public Array array(Object o) {
        try {
            return (Array) o;
        }
        catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, Array.class);
        }
    }
    static public Hunk hunk(Object o) {
        try {
            return (Hunk) o;
        }
        catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, Hunk.class);
        }
    }
    static public LambdaList lambdaList(Object o) {
        try {
            return (LambdaList) o;
        }
        catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, LambdaList.class);
        }
    }
    //static public Env env(Object o) {
    //    try {
    //        return (Env) o;
    //    }
    //    catch (ClassCastException e) {
    //        throw new IllegalDataTypeException(o, Env.class);
    //    }
    //}
    //static public Values values(Object o) {
    //    try {
    //        return (Values) o;
    //    }
    //    catch (ClassCastException e) {
    //        throw new IllegalDataTypeException(o, Values.class);
    //    }
    //}
    static public Package pkg(Object o) {
        try {
            return (Package) o;
        }
        catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, Package.class);
        }
    }
    static public Prop prop(Object o) {
        try {
            return (Prop) o;
        }
        catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, Prop.class);
        }
    }
    static public InputStream inputStream(Object o) {
        try {
            return (InputStream) o;
        }
        catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, InputStream.class);
        }
    }
    static public OutputStream outputStream(Object o) {
        try {
            return (OutputStream) o;
        }
        catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, OutputStream.class);
        }
    }
    static public Reader reader(Object o) {
        try {
            return (Reader) o;
        }
        catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, Reader.class);
        }
    }
    static public Writer writer(Object o) {
        try {
            return (Writer) o;
        }
        catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, Writer.class);
        }
    }
    static public File file(Object o) {
        try {
            return (File) o;
        }
        catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, File.class);
        }
    }
    static public Class javaClass(Object o) {
        try {
            return (Class) o;
        }
        catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, Class.class);
        }
    }
    static public Field javaField(Object o) {
        try {
            return (Field) o;
        }
        catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, Field.class);
        }
    }
    static public Method javaMethod(Object o) {
        try {
            return (Method) o;
        }
        catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, Method.class);
        }
    }
    static public ClassLoader classLoader(Object o) {
        try {
            return (ClassLoader) o;
        }
        catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, ClassLoader.class);
        }
    }

    /*
     * primitive type <=> reference type conversion
     */

    static public boolean toBoolean(Object o) {
        return o != Symbols.NIL;
    }
    static public int toInt(Object o) {
        return fixnum(o).intValue();
    }
    static public double toDouble(Object o) {
        return flonum(o).doubleValue();
    }
    static public char toChar(Object o) {
        return character(o).charValue();
    }

    static public Object toPredicate(boolean b) {
        return b ? Symbols.T : Symbols.NIL;
    }
    static public Character toCharacter(char c) {
        if (0 <= c && c < (char) chars_limit)
            return chars[(int) c];
        else
            return new Character(c);
    }
    static public Integer toFixnum(int i) {
        if (0 <= i && i < fixnums_limit) {
            //System.out.println(i+" -> "+fixnums[i]);
            return fixnums[i];
        }
        else if (-1 * fixnums_limit <= i && i < 0) {
            //System.out.println(i+" -> "+fixnums[fixnums.length + i]);
            return fixnums[fixnums.length + i];
        }
        else {
            return new Integer(i);
        }
    }
    static public Double toFlonum(double d) {
        return d == 0.0 ? flonum_zero
            :  d == 1.0 ? flonum_one
            :  new Double(d);
    }

    /*
     * generic equality and hash functions
     */

    static public boolean isEq(Object o1, Object o2) {
        return o1 == o2;
    }
    static public boolean isEqual(Object o1, Object o2) {
        return (o1 == null) ? o2 == null : o1.equals(o2);
    }
    static public int hashEq(Object o) {
        return o == null ? 0 : System.identityHashCode(o);
    }
    static public int hashEqual(Object o) {
        return o == null ? 0 : o.hashCode();
    }

    /*
     * compiler-happy functions
     */

    static public Object identity(Object o) {
        return o;
    }
    static public Symbol nil() {
        return Symbols.NIL;
    }
    static public Symbol t() {
        return Symbols.T;
    }
    static public boolean isTrue(boolean f) {
        return f;
    }
    static public boolean isFalse(boolean f) {
        return !f;
    }
    static public Integer zeroFixnum() {
        return fixnum_zero;
    }
    static public Double zeroFlonum() {
        return flonum_zero;
    }
    static public Integer oneFixnum() {
        return fixnum_one;
    }
    static public Double oneFlonum() {
        return flonum_one;
    }
    static public int zeroInt() {
        return 0;
    }
    static public double zeroDouble() {
        return 0.0;
    }
    static public int oneInt() {
        return 1;
    }
    static public double oneDouble() {
        return 1.0;
    }
    static public int identityInt(int n) {
        return n;
    }
    static public double identityDouble(double n) {
        return n;
    }
    static public char identityChar(char c) {
        return c;
    }
    private Data() {}
}
