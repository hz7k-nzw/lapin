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
import lapin.io.IO;
import lapin.lang.Data;
import lapin.lang.Env;
import lapin.lang.Symbol;
import lapin.lang.SysSymbols;
import lapin.load.Loader;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Iterator;

/**
 * This class converts the intermediate code into the byte code.
 * Subclass of this class must provide the public constructor
 * with no argument and {@link #doGenerate doGenerate} method.
 * @see ClassInfo
 * @see MethodInfo
 */
public abstract class ByteCodeGenerator {
    /**
     * Creates a new instance of <code>ByteCodeGenerator</code>
     * with the specified <code>classInfo</code>.
     * Subclass of <code>ByteCodeGenerator</code> constructed by
     * this method is defined in the special variable
     * SYS::*BYTE-CODE-GENERATOR-CLASSNAME*.
     * @param classInfo {@link ClassInfo} object,
     *        which stores the intermediate code
     * @param env
     * @return a new instance of <code>ByteCodeGenerator</code>
     * @throws ByteCodeGenerator.Exception
     */
    static public ByteCodeGenerator getInstance(ClassInfo classInfo, Env env) {
        try {
            String className = Data.string
                (env.get(SysSymbols.BYTE_CODE_GENERATOR_CLASSNAME));
            ClassLoader cl = Loader.getClassLoader(env);
            Class clazz = Class.forName(className, true, cl);
            ByteCodeGenerator bcg = (ByteCodeGenerator) clazz.newInstance();
            bcg.init(classInfo, env);
            return bcg;
        }
        catch (java.lang.Exception e) {
            throw new ByteCodeGenerator.Exception
                ("failed to instantiate ByteCodeGenerator", e);
        }
    }

    /** context object */
    protected ClassInfo classInfo;

    protected ByteCodeGenerator() {
    }
    private void init(ClassInfo classInfo, Env env) {
        if (classInfo == null)
            throw new NullPointerException("classInfo is null");
        this.classInfo = classInfo;

        init(env);
    }
    /**
     * Initializes this object.
     * Subclass of <code>ByteCodeGenerator</code> is expected
     * (but not required) to overwrite this method.
     */
    protected void init(Env env) {
    }

    /**
     * Generates byte code.
     * Subclass of <code>ByteCodeGenerator</code> is required
     * to overwrite this method.
     * @param env
     * @return Byte array which represents the byte code
     * @throws java.lang.Exception Any exception occurred within this method
     */
    protected abstract byte[] doGenerate(Env env) throws java.lang.Exception;

    /**
     * Generates byte code.
     * {@link #doGenerate doGenerate} is invoked inside this method.
     * @param env
     * @return Array of bytes which represents the byte code
     * @throws ByteCodeGenerator.Exception
     */
    public final byte[] generate(Env env) {
        try {
            return doGenerate(env);
        }
        catch (java.lang.Exception e) {
            throw new ByteCodeGenerator.Exception
                ("failed to generate bytecode", e);
        }
    }
    /**
     * Generates byte code and write it to <code>out</code>.
     * {@link #doGenerate doGenerate} is invoked inside this method.
     * @param out Output stream to which the byte code is emitted
     * @param env
     * @throws IOException
     * @throws ByteCodeGenerator.Exception
     */
    public final void emit(OutputStream out, Env env) throws IOException {
        byte[] bytes = generate(env);
        out.write(bytes);
        out.flush();
    }

    /** Exception which is thrown by {@link ByteCodeGenerator}. */
    static public class Exception extends Compiler.Exception {
        Exception(String msg, Throwable cause) {
            super(msg, cause);
        }
    }
}

