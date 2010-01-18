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
package lapin.load;
import lapin.eval.Evaluator;
import lapin.function.Subr;
import lapin.io.FileException;
import lapin.io.IO;
import lapin.io.Reader;
import lapin.io.StreamException;
import lapin.lang.Data;
import lapin.lang.Env;
import lapin.lang.Lisp;
import lapin.lang.LispException;
import lapin.lang.Lists;
import lapin.lang.Package;
import lapin.lang.Prop;
import lapin.lang.Symbol;
import lapin.lang.Symbols;
import lapin.lang.SysSymbols;
import lapin.lang.TypeException;
import lapin.util.Logger;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;
import org.apache.commons.codec.binary.Base64;

/**
 * Loader functions.
 */
public final class Loader {
    /** param types array of the CompiledExpr constructor. */
    static private final Class[] CONSTRUCTOR_PARAM_TYPES
        = new Class[] { Env.class };

    // uninterned symbol: for private use only
    static private final Symbol MY_CLASS_LOADER
        = Symbol.gensym("_CLASS-LOADER_");

    /** classloader used internally to load subr classes */
    static private class MyClassLoader extends URLClassLoader {
        static private final URL[] emptyUrls = new URL[0];
        MyClassLoader(ClassLoader parent) {
            super(emptyUrls, parent);
        }
        Class defineClass(String name, byte[] bytes) {
            return super.defineClass(name, bytes, 0, bytes.length);
        }
    }

    /**
     * Loads a file denoted by <code>in</code> onto the lisp environment.
     * @param in Name of the input file.
     *        If a file extension is either ".lisp" or ".fasl", then this
     *        method loads a file specified by <code>in</code>.
     *        Else, the method creates two pathname
     *        <code>in + ".fasl"</code>, <code>in + ".lisp"</code>
     *        and for each pathname tests whether a file denoted by the
     *        pathname exists. If none of these files exist, then the method
     *        throws {@link lapin.io.FileException}. Else if either of these
     *        files exists (but not both), then the method loads the existing
     *        one. Else, by comparing a timestamp of these files, this method
     *        loads the newer one.
     * @param inFileEnc Character encoding for the input file.
     *        If NIL is specified, then the platform's default
     *        character encoding is used.
     * @param env
     * @return T
     * @throws lapin.io.FileException
     * @throws lapin.io.StreamException
     * @throws lapin.eval.Evaluator.Exception
     */
    static public Object loadFile(String in, Object inFileEnc, Env env) {
        if (Logger.debuglevelp(env)) {
            Logger.debug("[loadFile] in: ~S",
                         Lists.list(in), env);
            Logger.debug("[loadFile] inFileEnc: ~S",
                         Lists.list(inFileEnc), env);
        }

        String inLisp, inFasl;
        if (in.endsWith(".lisp")) {
            inLisp = in;
            inFasl = null;
        }
        else if (in.endsWith(".fasl")) {
            inLisp = null;
            inFasl = in;
        }
        else {
            inLisp = in+".lisp";
            inFasl = in+".fasl";
        }

        File dir = IO.dir(Symbols.LOAD_DIR, env);

        File inFile;
        if (inLisp != null && inFasl == null)
            inFile = new File(dir, inLisp);
        else if (inLisp == null && inFasl != null)
            inFile = new File(dir, inFasl);
        else {
            File inLispFile = new File(dir, inLisp);
            File inFaslFile = new File(dir, inFasl);
            boolean inLispFileExists = inLispFile.exists();
            boolean inFaslFileExists = inFaslFile.exists();
            long inLispLastMod = inLispFileExists
                ? inLispFile.lastModified() : Long.MIN_VALUE;
            long inFaslLastMod = inFaslFileExists
                ? inFaslFile.lastModified() : Long.MIN_VALUE;
            if (inLispLastMod < inFaslLastMod) {
                inFile = inFaslFile;
            }
            else {
                if (inLispFileExists && inFaslFileExists)
                    Logger.warn("[loadFile] ~S is older than ~S.~%"+
                                "Loading LISP file...~%",
                                Lists.list(inFasl, inLisp), env);
                inFile = inLispFile;
            }
        }
        if (Logger.debuglevelp(env))
            Logger.debug("[loadFile] inFile=~S",
                         Lists.list(inFile), env);

        Package currpkg = Package.get(env);
        {
            Env newenv = env.child();
            // bind Loader.MyClassLoader
            bindInternalClassLoader(newenv);
            // preserve current package
            newenv.bind(Symbols.PACKAGE, currpkg);

            InputStream inputStream = null;
            try {
                inputStream = IO.openInputStream(inFile);
                java.io.Reader r;
                r = IO.toReader(inputStream, inFileEnc);
                r = IO.wrapReader(r, Symbols.T);
                Object exp;
                while (true) {
                    exp = Reader.read(r, Symbols.NIL, IO.EOF,
                                      Symbols.NIL, newenv);
                    if (exp == IO.EOF) {
                        return Symbols.T;
                    }
                    Evaluator.eval(exp, newenv);
                }
            }
            finally {
                IO.close(inputStream);
            }
        }
    }
    /**
     * Loads a resource denoted by <code>in</code> onto the lisp environment.
     * Actual resource searching is delegated to
     * {@link Class#getResourceAsStream Class.getResourceAsStream}.
     * @param in Name of the resource.
     *        If a file extension is either ".lisp" or ".fasl", then this
     *        method loads a resource specified by <code>in</code>.
     *        Else, the method creates two pathname
     *        <code>in + ".fasl"</code>, <code>in + ".lisp"</code>
     *        and for each pathname tests whether a resource denoted by the
     *        pathname exists. If none of these resources exist, then the
     *        method throws {@link lapin.io.FileException}. Else if either of
     *        these resources exists (but not both), then the method loads
     *        the existing one. Else, this method loads the resource that
     *        points to FASL file.
     * @param inFileEnc Character encoding for the input resource.
     *        If NIL is specified, then the platform's default
     *        character encoding is used.
     * @param env
     * @return T
     * @throws lapin.io.FileException
     * @throws lapin.io.StreamException
     * @throws lapin.eval.Evaluator.Exception
     */
    static public Object loadResource(String in, Object inFileEnc, Env env) {
        if (Logger.debuglevelp(env)) {
            Logger.debug("[loadResource] in: ~S",
                         Lists.list(in), env);
            Logger.debug("[loadResource] inFileEnc: ~S",
                         Lists.list(inFileEnc), env);
        }

        String inLisp, inFasl;
        if (in.endsWith(".lisp")) {
            inLisp = in;
            inFasl = null;
        }
        else if (in.endsWith(".fasl")) {
            inLisp = null;
            inFasl = in;
        }
        else {
            inLisp = in+".lisp";
            inFasl = in+".fasl";
        }

        Package currpkg = Package.get(env);
        {
            Env newenv = env.child();
            // bind Loader.MyClassLoader
            bindInternalClassLoader(newenv);
            // preserve current package
            newenv.bind(Symbols.PACKAGE, currpkg);

            InputStream inputStream = null;
            try {
                if (inLisp != null && inFasl == null) {
                    inputStream = Loader.class.getResourceAsStream(inLisp);
                }
                else if (inLisp == null && inFasl != null) {
                    inputStream = Loader.class.getResourceAsStream(inFasl);
                }
                else {
                    inputStream = Loader.class.getResourceAsStream(inFasl);
                    if (inputStream == null) {
                        Logger.info("[loadResource] "+
                                    "FASL file ~S not found. "+
                                    "Loading LISP file ~S...~%",
                                    Lists.list(inFasl, inLisp), env);
                        inputStream = Loader.class.getResourceAsStream(inLisp);
                    }
                }
                if (inputStream == null)
                    throw new FileException
                        ("resource not found: ~S.", Lists.list(in));
                java.io.Reader r;
                r = IO.toReader(inputStream, inFileEnc);
                r = IO.wrapReader(r, Symbols.T);
                Object exp;
                while (true) {
                    exp = Reader.read(r, Symbols.NIL, IO.EOF, 
                                      Symbols.NIL, newenv);
                    if (exp == IO.EOF) {
                        return Symbols.T;
                    }
                    Evaluator.eval(exp, newenv);
                }
            }
            finally {
                IO.close(inputStream);
            }
        }
    }
    /**
     * Imports symbols declared in static fields of the specified class.
     * @param clazz Class object in which symbols are declared
     * @param export If any object other than NIL is specified,
     *        then exports the imported symbols.
     * @param env
     * @return T
     * @throws LoadException
     * @see Symbol
     * @see Package
     */
    static public Object impSymbols(Class clazz, Object export, Env env) {
        try {
            return _impSymbols(clazz, export, env);
        }
        catch (java.lang.Exception e) {
            throw new LoadException
                ("symbol import error: ~S ~S",
                 Lists.list(clazz, export), e);
        }
    }
    static private Object _impSymbols(Class clazz, Object export, Env env)
        throws IllegalAccessException {
        Lisp lisp = env.lisp();
        Field[] fields = clazz.getFields();
        Class symClass = Symbol.class;
        for (int i = 0; i < fields.length; i++) {
            Field f = fields[i];
            int m = f.getModifiers();
            if (!Modifier.isStatic(m))
                continue;
            if (!symClass.isAssignableFrom(f.getType()))
                continue;
            Symbol sym = Data.symbol(f.get(null));
            lisp.getObarray().imp(sym.pkg(), sym);
            if (!Data.isNot(export))
                lisp.getObarray().exp(sym.pkg(), sym);
        }
        return Symbols.T;
    }
    /**
     * Imports subrs defined in static fields of the specified class.
     * @param pkgname {@link lapin.lang.Package} in which name of imported
     *        subrs are interned
     * @param clazz Class object in which subrs are declared
     * @param indicator Property indicator used to indicate the subr,
     *        such as <code>SUBR</code>, <code>FSUBR</code>
     * @param export If any object other than NIL is specified,
     *        then exports the interned symbols.
     * @param env
     * @return T
     * @throws LoadException
     * @see Subr
     * @see Symbol
     * @see Package
     */
    static public Object impSubrs(Object pkgname, Class clazz,
                                  Symbol indicator, Object export,
                                  Env env) {
        try {
            return _impSubrs(pkgname, clazz, indicator, export, env);
        }
        catch (java.lang.Exception e) {
            throw new LoadException
                ("subr import error: ~S ~S ~S ~S",
                 Lists.list(pkgname, clazz, indicator, export), e);
        }
    }
    static private Object _impSubrs(Object pkgname, Class clazz,
                                    Symbol indicator, Object export,
                                    Env env)
        throws IllegalAccessException {
        Lisp lisp = env.lisp();
        Field[] fields = clazz.getFields();
        Class subrClass = Subr.class;
        for (int i = 0; i < fields.length; i++) {
            Field f = fields[i];
            int m = f.getModifiers();
            if (!Modifier.isStatic(m))
                continue;
            if (!subrClass.isAssignableFrom(f.getType()))
                continue;
            Subr subr = Data.subr(f.get(null));
            Symbol sym = Data.symbol
                (lisp.getObarray().intern(pkgname, subr.name()).nth(0));
            if (!Data.isNot(export))
                lisp.getObarray().exp(pkgname, sym);
            Object old = lisp.getProp(sym, indicator, Symbols.NIL);
            if (!Data.isNot(old))
                throw new LispException
                    ("conflict detected while importing subrs "+
                     "defined in ~S: name=~S indicator=~S",
                     Lists.list(clazz, sym, indicator));
            lisp.setProp(sym, indicator, subr);
            if (subr instanceof Prop)
                lisp.setProp((Prop) subr, SysSymbols.SUBR_FIELD, f);
        }
        return Symbols.T;
    }
    /**
     * Loads subrs specified by <code>args</code>.
     * @param args List of subr info.
     *        The subr info is a list that has three element:
     *        <code>(name classname classinfo)</code>,
     *        where <code>name</code> is a {@link Symbol}
     *        representing the function name,
     *        <code>classname</code> is a {@link String}
     *        representing the function class,
     *        and <code>classinfo</code> is either NIL or {@link String}
     *        representing a BASE64 enocded byte array.
     *        If <code>classinfo</code> is NIL, then the function
     *        class is loaded from <code>CLASSPATH</code>.
     *        Else, the function class is loaded from a byte code
     *        represented by <code>classinfo</code>.
     * @param env
     * @return T
     * @throws LoadException
     */
    static public Object loadSubrs(Object args, Env env) {
        Lisp lisp = env.lisp();
        for (Object l = args; !Lists.isEnd(l); l = Lists.cdr(l)) {
            Object arg = Lists.car(l);
            if (Lists.length(arg) != 3) {
                throw new TypeException
                    ("args must be list of (name classname classinfo): ~S.",
                     Lists.list(args));
            }
            Symbol name = Data.symbol(Lists.car(arg));
            String classname = Data.string(Lists.cadr(arg));
            Object classinfo = Lists.caddr(arg);
            Subr subr;
            if (classinfo == Symbols.NIL) {
                subr = toSubr(name, classname, env);
            }
            else {
                byte[] bytes = toBytes(Data.string(classinfo));
                subr = toSubr(name, classname, bytes, env);
            }
            lisp.defun(name, Symbols.SUBR, subr);
        }
        return Symbols.T;
    }
    /**
     * Loads a subr from <code>CLASSPATH</code>.
     * @param name Name of the function
     * @param classname Class name of the function
     * @param env 
     * @return Subr loaded by this method
     * @throws LoadException
     */
    static public Subr toSubr
        (Symbol name, String classname, Env env) {
        try {
            ClassLoader loader = getClassLoader(env);
            Class clazz = Class.forName(classname, true, loader);
            return _toSubr(name, clazz, env);
        }
        catch (java.lang.Exception e) {
            throw new LoadException
                ("subr load error: ~S ~S",
                 Lists.list(name, classname), e);
        }
    }
    /**
     * Loads a subr from a byte array.
     * @param name Name of the function
     * @param classname Class name of the function
     * @param bytes Byte array which represents the byte code of the function
     * @param env 
     * @return Subr loaded by this method
     * @throws LoadException
     */
    static public Subr toSubr
        (Symbol name, String classname, byte[] bytes, Env env) {
        try {
            MyClassLoader loader;
            if (env.isBound(Loader.MY_CLASS_LOADER))
                loader = (MyClassLoader) env.get(Loader.MY_CLASS_LOADER);
            else
                loader = new MyClassLoader(getClassLoader(env));
            Class clazz = loader.defineClass(classname, bytes);
            return _toSubr(name, clazz, env);
        }
        catch (java.lang.Exception e) {
            throw new LoadException
                ("subr load error: ~S ~S",
                 Lists.list(name, classname), e);
        }
    }
    static private Subr _toSubr(Symbol name, Class clazz, Env env)
        throws InstantiationException, NoSuchMethodException,
               IllegalAccessException, InvocationTargetException {
        if (Logger.debuglevelp(env)) {
            Logger.debug("[toSubr] name  : ~S",
                         Lists.list(name), env);
            Logger.debug("[toSubr] class : ~S",
                         Lists.list(clazz), env);
            Logger.debug("[toSubr] loader: ~S",
                         Lists.list(clazz.getClassLoader()), env);
        }
        Constructor c = clazz.getConstructor(CONSTRUCTOR_PARAM_TYPES);
        return Data.subr(c.newInstance(new Object[] { env }));
    }

    static public ClassLoader getClassLoader(Env env) {
        Object val = env.get(SysSymbols.CLASS_LOADER);
        if (val != Symbols.NIL)
            return Data.classLoader(val);
        else
            return Loader.class.getClassLoader();
    }
    static public Env bindInternalClassLoader(Env env) {
        ClassLoader parent = getClassLoader(env);
        MyClassLoader loader = new MyClassLoader(parent);
        env.bind(Loader.MY_CLASS_LOADER, loader);
        return env;
    }
    static public String toString(byte[] bytes) {
        try {
            return b2s(encode(compress(bytes)));
        } catch (IOException e) {
            throw new StreamException
                ("I/O error occurred while converting bytes to string",
                 Symbols.NIL, e);
        }
    }
    static public byte[] toBytes(String bytesAsString) {
        try {
            return decompress(decode(s2b(bytesAsString)));
        } catch (IOException e) {
            throw new StreamException
                ("I/O error occurred while converting string to bytes",
                 Symbols.NIL, e);
        }
    }
    static private byte[] compress(byte[] bytes) throws IOException {
        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        GZIPOutputStream gout = new GZIPOutputStream(bout);
        gout.write(bytes);
        gout.flush();
        IO.close(gout);
        IO.close(bout);
        return bout.toByteArray();
    }
    static private byte[] decompress(byte[] bytes) throws IOException {
        ByteArrayInputStream bin = new ByteArrayInputStream(bytes);
        GZIPInputStream gin = new GZIPInputStream(bin);
        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        int b;
        while ((b = gin.read()) != -1)
            bout.write(b);
        bout.flush();
        IO.close(gin);
        IO.close(bin);
        IO.close(bout);
        return bout.toByteArray();
    }
    static private byte[] encode(byte[] bytes) {
        return Base64.encodeBase64(bytes);
    }
    static private byte[] decode(byte[] bytes) {
        return Base64.decodeBase64(bytes);
    }
    static private String b2s(byte[] bytes)
        throws UnsupportedEncodingException {
        //try {
            return new String(bytes, "8859_1");
        //} catch (UnsupportedEncodingException e) {
        //    throw new RuntimeException("this should not happen", e);
        //}
    }
    static private byte[] s2b(String str)
        throws UnsupportedEncodingException {
        //try {
            return str.getBytes("8859_1");
        //}  catch (UnsupportedEncodingException e) {
        //    throw new RuntimeException("this should not happen", e);
        //}
    }
    private Loader() {}
}
