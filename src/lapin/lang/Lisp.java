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
import lapin.comp.DeclInfo;
import lapin.function.Function;
import lapin.function.subrs.LispFSubrs;
import lapin.function.subrs.LispSubrs;
import lapin.function.subrs.SysSubrs;
import lapin.io.IO;
import lapin.io.Readtable;
import lapin.load.Loader;
import lapin.util.FormattableException;
import lapin.util.Logger;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.util.HashMap;

/**
 * Lisp runtime.
 * Each instance of this class has its own runtime environment
 * (including package, plist of symbols) and can coexist with
 * other instances of this class in the same JVM.
 */
public final class Lisp {
    /*
     * Note that this implementation is not synchronized. If multiple
     * threads access an instance of this class concurrently, it must
     * be synchronized externally.
     */

    static private final InputStream SYS_BIN_IN = System.in;
    static private final OutputStream SYS_BIN_OUT = System.out;
    static private final OutputStream SYS_BIN_ERR = System.err;
    static private final Reader SYS_IN
        = IO.wrapReader(IO.toReader(SYS_BIN_IN, Symbols.NIL),
                        Lists.list(Symbols.KW_USE_UNREAD));
    static private final Writer SYS_OUT
        = IO.wrapWriter(IO.toWriter(SYS_BIN_OUT, Symbols.NIL),
                        Lists.list(Symbols.KW_USE_PRINT));
    static private final Writer SYS_ERR
        = IO.wrapWriter(IO.toWriter(SYS_BIN_ERR, Symbols.NIL),
                        Lists.list(Symbols.KW_USE_PRINT));

    static private final InputStream NULL_BIN_IN
        = new InputStream() { public int read() { return -1; } };
    static private final OutputStream NULL_BIN_OUT
        = new OutputStream() { public void write(int b) {} };
    static private final Reader NULL_IN
        = IO.wrapReader(IO.toReader(NULL_BIN_IN, Symbols.NIL),
                        Lists.list(Symbols.KW_USE_UNREAD));
    static private final Writer NULL_OUT
        = IO.wrapWriter(IO.toWriter(NULL_BIN_OUT, Symbols.NIL),
                        Lists.list(Symbols.KW_USE_PRINT));

    /** root (global) env */
    private final Env env;
    /** obarray */
    private final Obarray obarray;
    /** property table */
    private final Penv penv;
    /** readtable */
    private final Readtable readtbl;
    /** declInfo */
    private final DeclInfo di;
    /** alarmclock, in which java.util.Timer is used. */
    private final Alarmclock alarmclock;

    /** Creates a new lisp instance. */
    public Lisp() {
        long t1 = 0;
        long t2 = 0;
        if (Constants.DEBUG) {
            t1 = System.currentTimeMillis();
        }

        /*
         * step 0: create member objects
         */
        // root (global) env
        env = new Env(this);
        // obarray
        obarray = new Obarray();
        // property table
        penv = new Penv();
        // readtable
        readtbl = new Readtable();
        // declInfo
        di = new DeclInfo();
        // alarmclock
        alarmclock = new Alarmclock();

        if (Constants.DEBUG) {
            t2 = System.currentTimeMillis();
            System.err.println
                ("[init] step 0: elapsed time: "+(t2-t1)+"[msec]");
            t1 = t2;
        }

        /*
         * step 1: create packages
         */
        // keyword package
        obarray.mkPkg("KEYWORD", Symbols.NIL);
        // lisp package
        obarray.mkPkg("LISP", Symbols.NIL);
        // sys package (:use "LISP")
        obarray.mkPkg("SYS", Lists.list("LISP"));
        // user package (:use "LISP")
        obarray.mkPkg("USER", Lists.list("LISP"));

        if (Constants.DEBUG) {
            t2 = System.currentTimeMillis();
            System.err.println
                ("[init] step 1: elapsed time: "+(t2-t1)+"[msec]");
            t1 = t2;
        }

        /*
         * step 2: set global variables.
         */
        defvar(Symbols.OBARRAY, obarray);
        setProp(Symbols.OBARRAY, Symbols.ARRAY, obarray);
        defvar(Symbols.READTABLE, readtbl);
        setProp(Symbols.READTABLE, Symbols.ARRAY, readtbl);
        defconst(Symbols.PI, Numbers.PI);
        //defconst(Symbols.EXP, Numbers.E);
        defvar(Symbols.SYSTEM_IN, Lisp.SYS_IN);
        defvar(Symbols.SYSTEM_OUT, Lisp.SYS_OUT);
        defvar(Symbols.SYSTEM_ERR, Lisp.SYS_ERR);
        defvar(Symbols.SYSTEM_BIN_IN, Lisp.SYS_BIN_IN);
        defvar(Symbols.SYSTEM_BIN_OUT, Lisp.SYS_BIN_OUT);
        defvar(Symbols.SYSTEM_BIN_ERR, Lisp.SYS_BIN_ERR);
        defvar(Symbols.NULL_IN, Lisp.NULL_IN);
        defvar(Symbols.NULL_OUT, Lisp.NULL_OUT);
        defvar(Symbols.NULL_BIN_IN, Lisp.NULL_BIN_IN);
        defvar(Symbols.NULL_BIN_OUT, Lisp.NULL_BIN_OUT);
        defvar(Symbols.TERMINAL_IN, Lisp.SYS_IN);
        defvar(Symbols.TERMINAL_OUT, Lisp.SYS_OUT);
        defvar(Symbols.READ_EVAL, Symbols.T);
        defvar(Symbols.PRINT_CASE, Symbols.KW_UPCASE);
        defvar(Symbols.PRINT_CIRCLE, Symbols.T);
        defvar(Symbols.PRINT_ESCAPE, Symbols.T);
        defvar(Symbols.PRINT_EXCEPTION_VERBOSE, Symbols.NIL);
        defvar(Symbols.PRINT_BACKTRACE_VERBOSE, Symbols.NIL);
        defconst(Symbols.LAMBDA_LIST_KEYWORDS,
                 Lists.copyList(Symbols.LAMBDA_LIST_KEYWORD_LIST));
        defconst(Symbols.FUNCTION_INDICATORS,
                 Lists.copyList(Symbols.FUNCTION_INDICATOR_LIST));
        defvar(Symbols.ALARMCLOCK, Symbols.NIL);
        defvar(Symbols.COMPILE_INPUT_DIR);
        defvar(Symbols.COMPILE_OUTPUT_DIR);
        defvar(Symbols.COMPILE_CLASS_DIR);
        defvar(Symbols.LOAD_DIR);
        defvar(Symbols.LOG_LEVEL, Constants.LOG_LEVEL);
        defvar(Symbols.LOG_OUT, Lisp.SYS_ERR);
        defvar(SysSymbols.CLASS_LOADER);
        defvar(SysSymbols.BYTE_CODE_GENERATOR_CLASSNAME,
               Constants.BYTE_CODE_GENERATOR_CLASSNAME);
        defvar(SysSymbols.FUN_ID, Data.toFixnum(0));
        defvar(Symbols.PACKAGE, obarray.findPkg("USER"));

        if (Constants.DEBUG) {
            t2 = System.currentTimeMillis();
            System.err.println
                ("[init] step 2: elapsed time: "+(t2-t1)+"[msec]");
            t1 = t2;
        }

        // From now on Logger can be used.

        /*
         * step 3: include symbols.
         */
        try {
            Logger.debug("try to include symbols...", env);
            Loader.impSymbols(Symbols.class, Symbols.T, env);
            Loader.impSymbols(SysSymbols.class, Symbols.NIL, env);
            Logger.debug("OK!", env);
        }
        catch (java.lang.Exception e) {
            Logger.debug("NG!", env);
            FormattableException.fillInMessage(e, env);
            throw new RuntimeException
                ("failed to include system symbols", e);
        }

        if (Constants.DEBUG) {
            t2 = System.currentTimeMillis();
            System.err.println
                ("[init] step 3: elapsed time: "+(t2-t1)+"[msec]");
            t1 = t2;
        }

        /*
         * step 4: register subrs and fsubrs.
         */
        try {
            Logger.debug("try to include system functions...", env);
            Loader.impSubrs("LISP", LispSubrs.class,
                            Symbols.SUBR, Symbols.T, env);
            Loader.impSubrs("LISP", LispFSubrs.class,
                            Symbols.FSUBR, Symbols.T, env);
            Loader.impSubrs("SYS", SysSubrs.class,
                            Symbols.SUBR, Symbols.NIL, env);
            Logger.debug("OK!", env);
        }
        catch (java.lang.Exception e) {
            Logger.debug("NG!", env);
            FormattableException.fillInMessage(e, env);
            throw new RuntimeException
                ("failed to include system functions", e);
        }

        if (Constants.DEBUG) {
            t2 = System.currentTimeMillis();
            System.err.println
                ("[init] step 4: elapsed time: "+(t2-t1)+"[msec]");
            t1 = t2;
        }

        /*
         * step 5: load init script.
         */
        String resName = Constants.INIT_RESOURCE_NAME;
        try {
            Logger.debug("try to load "+resName+"...", env);
            Loader.loadResource(resName, Symbols.NIL, env);
            Logger.debug("OK!", env);
        }
        catch (java.lang.Exception e) {
            Logger.debug("NG!", env);
            FormattableException.fillInMessage(e, env);
            throw new RuntimeException
                ("failed to load resource: "+resName, e);
        }

        if (Constants.DEBUG) {
            t2 = System.currentTimeMillis();
            System.err.println
                ("[init] step 5: elapsed time: "+(t2-t1)+"[msec]");
        }
    }
    public Env getEnv() {
        return env;
    }
    public Obarray getObarray() {
        return obarray;
    }
    public Penv getPenv() {
        return penv;
    }
    public Readtable getReadtable() {
        return readtbl;
    }
    public DeclInfo getDeclInfo() {
        return di;
    }
    public Alarmclock getAlarmclock() {
        return alarmclock;
    }
    public Object get(Symbol sym) {
        return env.get(sym);
    }
    public void set(Symbol sym, Object val) {
        env.set(sym, val);
    }
    public Object getPlist(Prop prop) {
        return penv.getPlist(prop);
    }
    public void setPlist(Prop prop, Object plst) {
        penv.setPlist(prop, plst);
    }
    public Object getProp(Prop prop, Object indicator) {
        return getProp(prop, indicator, Symbols.NIL);
    }
    public Object getProp(Prop prop, Object indicator, Object defaultVal) {
        return penv.getProp(prop, indicator, defaultVal);
    }
    public Object setProp(Prop prop, Object indicator, Object val) {
        return penv.setProp(prop, indicator, val);
    }
    public Object remProp(Prop prop, Object indicator) {
        return penv.remProp(prop, indicator);
    }
    public void defun(Symbol sym, Symbol type, Function fun) {
        remProp(sym, type);
        setProp(sym, type, fun);
    }
    public void defvar(Symbol sym) {
        getDeclInfo().pushSpecialVar(sym);
    }
    public void defvar(Symbol sym, Object val) {
        set(sym, val);
        getDeclInfo().pushSpecialVar(sym);
    }
    public void defconst(Symbol sym, Object val) {
        set(sym, val);
        getDeclInfo().pushSpecialVar(sym);
        setProp(sym, Symbols.CONSTANT, Symbols.T);
    }
    public boolean isConstant(Symbol sym) {
        return getProp(sym, Symbols.CONSTANT) == Symbols.T;
    }
    public Object functions(Symbol sym) {
        return Plists.getl(getPlist(sym),
                           Symbols.FUNCTION_INDICATOR_LIST);
    }
}
