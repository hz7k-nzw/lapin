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
import lapin.eval.Evaluator;
import lapin.lang.Data;
import lapin.lang.Env;
import lapin.lang.ProgramException;
import lapin.lang.Prop;
import lapin.lang.Lists;
import lapin.lang.Symbol;
import lapin.lang.Symbols;
import lapin.lang.TypeException;
import lapin.lang.Values;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

/**
 * Base class for the system functions.
 */
public abstract class SystemSubr extends Subr implements Prop {
    SystemSubr(String name) {
        super(name);
    }
    /**
     * Returns a method object which is invoked when this function is called.
     * When the return value of this method is not <code>null</code>,
     * it will be used by the compiler as a hint to emit more
     * efficient code.
     * @param nargs Number of arguments passed to this function
     * @return Method object used by this function,
     *         <code>null</code> is allowed
     */
    public Method getTargetMethod(int nargs) {
        return null;
    }
    /**
     * Returns true if it is possible for this function
     * to return the {@link Values multiple-value}.
     * The boolean returned by this method is used by the 
     * compiler as a hint to emit more efficient code.
     * @param nargs Number of arguments passed to this function
     * @return if true, then it is possible for this function
     *         to return the {@link Values multiple-value}
     */
    public boolean canProduceValues(int nargs) {
        return true;
    }

    static public SystemSubr toSubr0
        (String name, Class c, String m) {
        Method method = getMethod(c, m, 0);
        if (isStatic(method))
            return new SUBR0.MethodAdapter(name, method);
        else
            return new SUBR1.MethodAdapter(name, method);
    }
    static public SystemSubr toSubr1
        (String name, Class c, String m) {
        Method method = getMethod(c, m, 1);
        if (isStatic(method))
            return new SUBR1.MethodAdapter(name, method);
        else
            return new SUBR2.MethodAdapter(name, method);
    }
    static public SystemSubr toSubr2
        (String name, Class c, String m) {
        Method method = getMethod(c, m, 2);
        if (isStatic(method))
            return new SUBR2.MethodAdapter(name, method);
        else
            return new SUBR3.MethodAdapter(name, method);
    }
    static public SystemSubr toSubr3
        (String name, Class c, String m) {
        Method method = getMethod(c, m, 3);
        if (isStatic(method))
            return new SUBR3.MethodAdapter(name, method);
        else
            return new SUBR4.MethodAdapter(name, method);
    }
    static public SystemSubr toSubr4
        (String name, Class c, String m) {
        Method method = getMethod(c, m, 4);
        if (isStatic(method))
            return new SUBR4.MethodAdapter(name, method);
        else
            return new SUBR5.MethodAdapter(name, method);
    }
    static public SystemSubr toSubr5
        (String name, Class c, String m) {
        Method method = getMethod(c, m, 5);
        if (isStatic(method))
            return new SUBR5.MethodAdapter(name, method);
        else
            throw new RuntimeException
                ("nonstatic method with 5 args is not acceptable: "+method);
    }
    static public SystemSubr toSubr0
        (String name, Class c, String m,
         boolean canProduceValues) {
        Method method = getMethod(c, m, 0);
        if (isStatic(method))
            return new SUBR0.MethodAdapter(name, method, canProduceValues);
        else
            return new SUBR1.MethodAdapter(name, method, canProduceValues);
    }
    static public SystemSubr toSubr1
        (String name, Class c, String m,
         boolean canProduceValues) {
        Method method = getMethod(c, m, 1);
        if (isStatic(method))
            return new SUBR1.MethodAdapter(name, method, canProduceValues);
        else
            return new SUBR2.MethodAdapter(name, method, canProduceValues);
    }
    static public SystemSubr toSubr2
        (String name, Class c, String m,
         boolean canProduceValues) {
        Method method = getMethod(c, m, 2);
        if (isStatic(method))
            return new SUBR2.MethodAdapter(name, method, canProduceValues);
        else
            return new SUBR3.MethodAdapter(name, method, canProduceValues);
    }
    static public SystemSubr toSubr3
        (String name, Class c, String m,
         boolean canProduceValues) {
        Method method = getMethod(c, m, 3);
        if (isStatic(method))
            return new SUBR3.MethodAdapter(name, method, canProduceValues);
        else
            return new SUBR4.MethodAdapter(name, method, canProduceValues);
    }
    static public SystemSubr toSubr4
        (String name, Class c, String m,
         boolean canProduceValues) {
        Method method = getMethod(c, m, 4);
        if (isStatic(method))
            return new SUBR4.MethodAdapter(name, method, canProduceValues);
        else
            return new SUBR5.MethodAdapter(name, method, canProduceValues);
    }
    static public SystemSubr toSubr5
        (String name, Class c, String m,
         boolean canProduceValues) {
        Method method = getMethod(c, m, 5);
        if (isStatic(method))
            return new SUBR5.MethodAdapter(name, method, canProduceValues);
        else
            throw new RuntimeException
                ("nonstatic method with 5 args is not acceptable: "+method);
    }

    static public SystemSubr toSubr1
        (String name, Class c, String m,
         Class t0) {
        Method method = getMethod(c, m, new Class[]{t0});
        if (isStatic(method))
            return new SUBR1.MethodAdapter(name, method);
        else
            return new SUBR2.MethodAdapter(name, method);
    }
    static public SystemSubr toSubr2
        (String name, Class c, String m,
         Class t0, Class t1) {
        Method method = getMethod(c, m, new Class[]{t0,t1});
        if (isStatic(method))
            return new SUBR2.MethodAdapter(name, method);
        else
            return new SUBR3.MethodAdapter(name, method);
    }
    static public SystemSubr toSubr3
        (String name, Class c, String m,
         Class t0, Class t1, Class t2) {
        Method method = getMethod(c, m, new Class[]{t0,t1,t2});
        if (isStatic(method))
            return new SUBR3.MethodAdapter(name, method);
        else
            return new SUBR4.MethodAdapter(name, method);
    }
    static public SystemSubr toSubr4
        (String name, Class c, String m,
         Class t0, Class t1, Class t2, Class t3) {
        Method method = getMethod(c, m, new Class[]{t0,t1,t2,t3});
        if (isStatic(method))
            return new SUBR4.MethodAdapter(name, method);
        else
            return new SUBR5.MethodAdapter(name, method);
    }
    static public SystemSubr toSubr5
        (String name, Class c, String m,
         Class t0, Class t1, Class t2, Class t3, Class t4) {
        Method method = getMethod(c, m, new Class[]{t0,t1,t2,t3,t4});
        if (isStatic(method))
            return new SUBR5.MethodAdapter(name, method);
        else
            throw new RuntimeException
                ("nonstatic method with 5 args is not acceptable: "+method);
    }
    static public SystemSubr toSubr1
        (String name, Class c, String m,
         Class t0,
         boolean canProduceValues) {
        Method method = getMethod(c, m, new Class[]{t0});
        if (isStatic(method))
            return new SUBR1.MethodAdapter(name, method, canProduceValues);
        else
            return new SUBR2.MethodAdapter(name, method, canProduceValues);
    }
    static public SystemSubr toSubr2
        (String name, Class c, String m,
         Class t0, Class t1,
         boolean canProduceValues) {
        Method method = getMethod(c, m, new Class[]{t0,t1});
        if (isStatic(method))
            return new SUBR2.MethodAdapter(name, method, canProduceValues);
        else
            return new SUBR3.MethodAdapter(name, method, canProduceValues);
    }
    static public SystemSubr toSubr3
        (String name, Class c, String m,
         Class t0, Class t1, Class t2,
         boolean canProduceValues) {
        Method method = getMethod(c, m, new Class[]{t0,t1,t2});
        if (isStatic(method))
            return new SUBR3.MethodAdapter(name, method, canProduceValues);
        else
            return new SUBR4.MethodAdapter(name, method, canProduceValues);
    }
    static public SystemSubr toSubr4
        (String name, Class c, String m,
         Class t0, Class t1, Class t2, Class t3,
         boolean canProduceValues) {
        Method method = getMethod(c, m, new Class[]{t0,t1,t2,t3});
        if (isStatic(method))
            return new SUBR4.MethodAdapter(name, method, canProduceValues);
        else
            return new SUBR5.MethodAdapter(name, method, canProduceValues);
    }
    static public SystemSubr toSubr5
        (String name, Class c, String m,
         Class t0, Class t1, Class t2, Class t3, Class t4,
         boolean canProduceValues) {
        Method method = getMethod(c, m, new Class[]{t0,t1,t2,t3,t4});
        if (isStatic(method))
            return new SUBR5.MethodAdapter(name, method, canProduceValues);
        else
            throw new RuntimeException
                ("nonstatic method with 5 args is not acceptable: "+method);
    }
    static private Method getMethod
        (Class clazz, String name, int nargs) {
        Class[] argTypes = new Class[nargs];
        for (int i = 0; i < nargs; i++) {
            argTypes[i] = Object.class;
        }
        return getMethod(clazz, name, argTypes);
    }
    static private Method getMethod
        (Class clazz, String name, Class[] argTypes) {
        int nargs = argTypes.length;
        if (nargs < 0 || 5 < nargs) {
            throw new RuntimeException
                ("unsupported nargs: "+nargs);
        }
        try {
            return clazz.getMethod(name, argTypes);
        }
        catch (Exception e) {
            throw new RuntimeException
                ("failed to get method: "
                 +clazz.getName()+"#"+name+":("+nargs+" args)", e);
        }
    }
    static private void checkAcceptable(Method m) {
        Class rType = m.getReturnType();
        if (rType.equals(void.class) ||
            rType.equals(byte.class) ||
            rType.equals(short.class) ||
            rType.equals(long.class) ||
            rType.equals(float.class) ||
            rType.isArray()) {
            throw new RuntimeException
                ("return-type #<"+rType+"> is not supported: "+m);
        }
        Class[] pTypes = m.getParameterTypes();
        for (int i = 0; i < pTypes.length; i++) {
            Class pType = pTypes[i];
            if (pType.equals(byte.class) ||
                pType.equals(short.class) ||
                pType.equals(long.class) ||
                pType.equals(float.class) ||
                pType.isArray()) {
                throw new RuntimeException
                    ("param-type["+i+"] #<"+pType+"> is not supported: "+m);
            }
        }
    }
    static private boolean isStatic(Method m) {
        return Modifier.isStatic(m.getModifiers());
    }
    static private boolean isBoolean(Method m) {
        return m.getReturnType().equals(boolean.class);
    }
    static private Object toPredicate(Object b) {
        return Boolean.TRUE.equals(b) ? Symbols.T : Symbols.NIL;
    }
    static private boolean canProduceValues(Method m) {
        Class rType = m.getReturnType();
        return rType.isAssignableFrom(Values.class);
    }
    /*
     * Subtypes of system function
     */

    static private final Object[] EMPTY_ARGS = {};

    /** Base class for the generic SUBR. */
    static public abstract class SUBR
        extends SystemSubr implements Callable {
        /** lambda list of the function. */
        private final LambdaList lambdaList;
        /**
         * Constructs a subr with specified parameters.
         * @param name Name of this subr
         * @param reqCount Number of the required variables
         * @param optCount Number of the optional variables
         * @param rest if <code>true</code>, then rest variables is used
         * @param keywords List of symbols used in this subr
         * @param allowOtherKeys if true, then keyword not included in
         *        <code>keywords</code> can be passed as argument
         */
        protected SUBR(String name,
                       int reqCount, int optCount,boolean rest,
                       Object keywords, boolean allowOtherKeys) {
            super(name);
            this.lambdaList = new LambdaList
                (reqCount, optCount, rest, keywords, allowOtherKeys);
            //System.out.println(name+": "+lambdaList);
        }
        public final String toString() {
            return "SUBR:"+super.name();
        }
        /** Stores bindings in <code>env</code> and then invokes
            {@link #doCall doCall} method. */
        public final Object call(Object args, Env env) throws Exception {
            Env newenv = Evaluator.bindArgs
                (lambdaList, args, env.child());
            return doCall(newenv);
        }
        /**
         * Calls this function with the specified env.
         * @param env Environment in which the arguments applied
         *        to this function are stored.
         * @return Return value of the function
         * @throws java.lang.Exception Any exception thrown by this function
         */
        protected abstract Object doCall(Env env) throws Exception;

        /** Returns a {@link LambdaList} held by this subr. */
        protected final LambdaList lambdaList() {
            return lambdaList;
        }
        /**
         * Retrieves a value for the whole parameter
         * from <code>env</code>.
         * @param env
         * @return value for the parameter
         */
        protected final Object whole(Env env) {
            return env.get(Data.symbol(lambdaList.wholeVar()));
        }
        /**
         * Retrieves a value for the required parameter
         * at the specified index from <code>env</code>.
         * @param i index, which must satisfy
         *        <code>
         *        0 &lt;= i &amp;&amp;
         *        i &lt; {@link LambdaList#reqCount reqCount}
         *        </code>
         * @param env
         * @return value for the parameter
         */
        protected final Object required(int i, Env env) {
            return env.get(Data.symbol(lambdaList.reqVar(i)));
        }
        /**
         * Retrieves a value for the optional parameter
         * at the specified index from <code>env</code>.
         * @param i index, which must satisfy
         *        <code>
         *        0 &lt;= i &amp;&amp;
         *        i &lt; {@link LambdaList#optCount optCount}
         *        </code>
         * @param env
         * @return value for the parameter
         */
        protected final Object optional(int i, Env env) {
            return env.get(Data.symbol(lambdaList.optVar(i)));
        }
        /** Returns true if a value for the optional parameter
            at the specified index is supplied. */
        protected final boolean isOptional(int i, Env env) {
            return !Data.isNot(env.get(lambdaList.optSvar(i)));
        }
        /**
         * Retrieves a value for the rest parameter
         * from <code>env</code>.
         * @param env
         * @return value for the parameter
         */
        protected final Object rest(Env env) {
            return env.get(Data.symbol(lambdaList.restVar()));
        }
        /**
         * Retrieves a value for the keyword parameter
         * at the specified index from <code>env</code>.
         * @param i index, which must satisfy
         *        <code>
         *        0 &lt;= i &amp;&amp;
         *        i &lt; {@link LambdaList#keyCount keyCount}
         *        </code>
         * @param env
         * @return value for the parameter
         */
        protected final Object keyword(int i, Env env) {
            return env.get(Data.symbol(lambdaList.keyVar(i)));
        }
        /** Returns true if a value for the keyword parameter
            at the specified index is supplied. */
        protected final boolean isKeyword(int i, Env env) {
            return !Data.isNot(env.get(lambdaList.keySvar(i)));
        }
        /**
         * Retrieves a value for the keyword <code>key</code>
         * from <code>env</code>.
         * @param key keyword
         * @param env
         * @return value for the parameter
         */
        protected final Object keyword(Symbol key, Env env) {
            for (int i = 0; i < lambdaList.keyCount(); i++) {
                if (key == lambdaList.keyKey(i))
                    return keyword(i, env);
            }
            throw new ProgramException
                ("bind not found for keyword: ~S.", Lists.list(key));
        }
        /** Returns true if a value for the keyword
            <code>key</code> is supplied. */
        protected final boolean isKeyword(Symbol key, Env env) {
            for (int i = 0; i < lambdaList.keyCount(); i++) {
                if (key == lambdaList.keyKey(i))
                    return isKeyword(i, env);
            }
            throw new ProgramException
                ("bind not found for keyword: ~S.", Lists.list(key));
        }
        /**
         * Retrieves a value for the aux parameter
         * at the specified index from <code>env</code>.
         * @param i index, which must satisfy
         *        <code>
         *        0 &lt;= i &amp;&amp;
         *        i &lt; {@link LambdaList#auxCount auxCount}
         *        </code>
         * @param env
         * @return value for the parameter
         */
        protected final Object aux(int i, Env env) {
            return env.get(Data.symbol(lambdaList.auxVar(i)));
        }
    }/* end of SUBR */

    /** Base class for SUBR with no argument. */
    static public abstract class SUBR0
        extends SystemSubr implements Callable0 {
        protected SUBR0(String name) {
            super(name);
        }
        public final String toString() {
            return "SUBR0:"+super.name();
        }
        /** Method adapter */
        static final class MethodAdapter extends SUBR0 {
            /** target method */
            private final Method m;
            /** the target method returns boolean value ? */
            private final boolean b;
            /** flag which indicates this subr can return multiple values. */
            private final boolean cpv;
            MethodAdapter(String name, Method m) {
                this(name, m, SystemSubr.canProduceValues(m));
            }
            MethodAdapter(String name, Method m, boolean cpv) {
                super(name);
                SystemSubr.checkAcceptable(m);
                if (!SystemSubr.isStatic(m))
                    throw new RuntimeException
                        ("nonstatic method is not acceptable: "+m);
                this.m = m;
                this.b = SystemSubr.isBoolean(m);
                this.cpv = cpv;
            }
            public Object call0(Env env) throws Exception {
                try {
                    Object ret = m.invoke(null, EMPTY_ARGS);
                    return b ? SystemSubr.toPredicate(ret) : ret;
                } catch (IllegalArgumentException e) {
                    throw new TypeException
                        ("argument type mismatch: expected=~S.",
                         Lists.list(Lists.toList(m.getParameterTypes())));
                }
            }
            public Method getTargetMethod(int nargs) {
                if (nargs == 0)
                    return m;
                else
                    return super.getTargetMethod(nargs);
            }
            public boolean canProduceValues(int nargs) {
                if (nargs == 0)
                    return cpv;
                else
                    return super.canProduceValues(nargs);
            }
        }/* end of MethodAdapter */
    }/* end of SUBR0 */

    /** Base class for SUBR with 1 required argument. */
    static public abstract class SUBR1
        extends SystemSubr implements Callable1 {
        protected SUBR1(String name) {
            super(name);
        }
        public final String toString() {
            return "SUBR1:"+super.name();
        }
        /** Method adapter */
        static final class MethodAdapter extends SUBR1 {
            /** target method */
            private final Method m;
            /** the target method is static ? */
            private final boolean s;
            /** the target method returns boolean value ? */
            private final boolean b;
            /** flag which indicates this subr can return multiple values. */
            private final boolean cpv;
            MethodAdapter(String name, Method m) {
                this(name, m, SystemSubr.canProduceValues(m));
            }
            MethodAdapter(String name, Method m, boolean cpv) {
                super(name);
                SystemSubr.checkAcceptable(m);
                this.m = m;
                this.s = SystemSubr.isStatic(m);
                this.b = SystemSubr.isBoolean(m);
                this.cpv = cpv;
            }
            public Object call1(Object arg0, Env env) throws Exception {
                try {
                    Object ret = s
                        ? m.invoke(null, new Object[] {arg0})
                        : m.invoke(arg0, EMPTY_ARGS);
                    return b ? SystemSubr.toPredicate(ret) : ret;
                } catch (IllegalArgumentException e) {
                    throw new TypeException
                        ("argument type mismatch: expected=~S.",
                         Lists.list(Lists.toList(m.getParameterTypes())));
                }
            }
            public Method getTargetMethod(int nargs) {
                if (nargs == 1)
                    return m;
                else
                    return super.getTargetMethod(nargs);
            }
            public boolean canProduceValues(int nargs) {
                if (nargs == 1)
                    return cpv;
                else
                    return super.canProduceValues(nargs);
            }
        }/* end of MethodAdapter */
    }/* end of SUBR1 */

    /** Base class for SUBR with 2 required arguments. */
    static public abstract class SUBR2
        extends SystemSubr implements Callable2 {
        protected SUBR2(String name) {
            super(name);
        }
        public final String toString() {
            return "SUBR2:"+super.name();
        }
        /** Method adapter */
        static final class MethodAdapter extends SUBR2 {
            /** target method */
            private final Method m;
            /** the target method is static ? */
            private final boolean s;
            /** the target method returns boolean value ? */
            private final boolean b;
            /** flag which indicates this subr can return multiple values. */
            private final boolean cpv;
            MethodAdapter(String name, Method m) {
                this(name, m, SystemSubr.canProduceValues(m));
            }
            MethodAdapter(String name, Method m, boolean cpv) {
                super(name);
                SystemSubr.checkAcceptable(m);
                this.m = m;
                this.s = SystemSubr.isStatic(m);
                this.b = SystemSubr.isBoolean(m);
                this.cpv = cpv;
            }
            public Object call2
                (Object arg0, Object arg1, Env env) throws Exception {
                try {
                    Object ret = s
                        ? m.invoke(null, new Object[] {arg0,arg1})
                        : m.invoke(arg0, new Object[] {arg1});
                    return b ? SystemSubr.toPredicate(ret) : ret;
                } catch (IllegalArgumentException e) {
                    throw new TypeException
                        ("argument type mismatch: expected=~S.",
                         Lists.list(Lists.toList(m.getParameterTypes())));
                }
            }
            public Method getTargetMethod(int nargs) {
                if (nargs == 2)
                    return m;
                else
                    return super.getTargetMethod(nargs);
            }
            public boolean canProduceValues(int nargs) {
                if (nargs == 2)
                    return cpv;
                else
                    return super.canProduceValues(nargs);
            }
        }/* end of MethodAdapter */
    }/* end of SUBR2 */

    /** Base class for SUBR with 3 required arguments. */
    static public abstract class SUBR3
        extends SystemSubr implements Callable3 {
        protected SUBR3(String name) {
            super(name);
        }
        public final String toString() {
            return "SUBR3:"+super.name();
        }
        /** Method adapter */
        static final class MethodAdapter extends SUBR3 {
            /** target method */
            private final Method m;
            /** the target method is static ? */
            private final boolean s;
            /** the target method returns boolean value ? */
            private final boolean b;
            /** flag which indicates this subr can return multiple values. */
            private final boolean cpv;
            MethodAdapter(String name, Method m) {
                this(name, m, SystemSubr.canProduceValues(m));
            }
            MethodAdapter(String name, Method m, boolean cpv) {
                super(name);
                SystemSubr.checkAcceptable(m);
                this.m = m;
                this.s = SystemSubr.isStatic(m);
                this.b = SystemSubr.isBoolean(m);
                this.cpv = cpv;
            }
            public Object call3
                (Object arg0, Object arg1, Object arg2, Env env)
                throws Exception {
                try {
                    Object ret = s
                        ? m.invoke(null, new Object[] {arg0,arg1,arg2})
                        : m.invoke(arg0, new Object[] {arg1,arg2});
                    return b ? SystemSubr.toPredicate(ret) : ret;
                } catch (IllegalArgumentException e) {
                    throw new TypeException
                        ("argument type mismatch: expected=~S.",
                         Lists.list(Lists.toList(m.getParameterTypes())));
                }
            }
            public Method getTargetMethod(int nargs) {
                if (nargs == 3)
                    return m;
                else
                    return super.getTargetMethod(nargs);
            }
            public boolean canProduceValues(int nargs) {
                if (nargs == 3)
                    return cpv;
                else
                    return super.canProduceValues(nargs);
            }
        }/* end of MethodAdapter */
    }/* end of SUBR3 */

    /** Base class for SUBR with 4 required arguments. */
    static public abstract class SUBR4
        extends SystemSubr implements Callable4 {
        protected SUBR4(String name) {
            super(name);
        }
        public final String toString() {
            return "SUBR4:"+super.name();
        }
        /** Method adapter */
        static final class MethodAdapter extends SUBR4 {
            /** target method */
            private final Method m;
            /** the target method is static ? */
            private final boolean s;
            /** the target method returns boolean value ? */
            private final boolean b;
            /** flag which indicates this subr can return multiple values. */
            private final boolean cpv;
            MethodAdapter(String name, Method m) {
                this(name, m, SystemSubr.canProduceValues(m));
            }
            MethodAdapter(String name, Method m, boolean cpv) {
                super(name);
                SystemSubr.checkAcceptable(m);
                this.m = m;
                this.s = SystemSubr.isStatic(m);
                this.b = SystemSubr.isBoolean(m);
                this.cpv = cpv;
            }
            public Object call4
                (Object arg0, Object arg1, Object arg2, Object arg3,
                 Env env) throws Exception {
                try {
                    Object ret = s
                        ? m.invoke(null, new Object[] {arg0,arg1,arg2,arg3})
                        : m.invoke(arg0, new Object[] {arg1,arg2,arg3});
                    return b ? SystemSubr.toPredicate(ret) : ret;
                } catch (IllegalArgumentException e) {
                    throw new TypeException
                        ("argument type mismatch: expected=~S.",
                         Lists.list(Lists.toList(m.getParameterTypes())));
                }
            }
            public Method getTargetMethod(int nargs) {
                if (nargs == 4)
                    return m;
                else
                    return super.getTargetMethod(nargs);
            }
            public boolean canProduceValues(int nargs) {
                if (nargs == 4)
                    return cpv;
                else
                    return super.canProduceValues(nargs);
            }
        }/* end of MethodAdapter */
    }/* end of SUBR4 */

    /** Base class for SUBR with 5 required arguments. */
    static public abstract class SUBR5
        extends SystemSubr implements Callable5 {
        protected SUBR5(String name) {
            super(name);
        }
        public final String toString() {
            return "SUBR5:"+super.name();
        }
        /** Method adapter */
        static final class MethodAdapter extends SUBR5 {
            /** target method */
            private final Method m;
            /** the target method is static ? */
            private final boolean s;
            /** the target method returns boolean value ? */
            private final boolean b;
            /** flag which indicates this subr can return multiple values. */
            private final boolean cpv;
            MethodAdapter(String name, Method m) {
                this(name, m, SystemSubr.canProduceValues(m));
            }
            MethodAdapter(String name, Method m, boolean cpv) {
                super(name);
                SystemSubr.checkAcceptable(m);
                this.m = m;
                this.s = SystemSubr.isStatic(m);
                this.b = SystemSubr.isBoolean(m);
                this.cpv = cpv;
            }
            public Object call5
                (Object arg0, Object arg1, Object arg2, Object arg3,
                 Object arg4, Env env) throws Exception {
                try {
                    Object ret = s
                        ? m.invoke(null, new Object[] {arg0,arg1,arg2,arg3,arg4})
                        : m.invoke(arg0, new Object[] {arg1,arg2,arg3,arg4});
                    return b ? SystemSubr.toPredicate(ret) : ret;
                } catch (IllegalArgumentException e) {
                    throw new TypeException
                        ("argument type mismatch: expected=~S.",
                         Lists.list(Lists.toList(m.getParameterTypes())));
                }
            }
            public Method getTargetMethod(int nargs) {
                if (nargs == 5)
                    return m;
                else
                    return super.getTargetMethod(nargs);
            }
            public boolean canProduceValues(int nargs) {
                if (nargs == 5)
                    return cpv;
                else
                    return super.canProduceValues(nargs);
            }
        }/* end of MethodAdapter */
    }/* end of SUBR5 */

}/* end of SystemSubr */
