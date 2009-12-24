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
package lapin.function.subrs;
import lapin.comp.Classes;
import lapin.comp.Compiler;
import lapin.comp.DeclInfo;
import lapin.eval.Funcall;
import lapin.function.Function;
import lapin.function.Subr;
import lapin.function.SystemSubr;
import lapin.io.IO;
import lapin.io.Printer;
import lapin.lang.Conv;
import lapin.lang.Data;
import lapin.lang.Env;
import lapin.lang.Lists;
import lapin.lang.Numbers;
import lapin.lang.Package;
import lapin.lang.Symbols;
import lapin.load.Loader;
import lapin.util.form.Backquote;
import lapin.util.form.Do;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.math.BigInteger;
import java.util.Set;
import java.util.TreeSet;

public final class SysSubrs {
    static public final Subr _NIL
        = SystemSubr.toSubr0("NIL",Data.class,"nil");
    static public final Subr _T
        = SystemSubr.toSubr0("T",Data.class,"t");
    static public final Subr _TRUE
        = SystemSubr.toSubr1("TRUE",Data.class,
                             "isTrue",boolean.class);
    static public final Subr _FALSE
        = SystemSubr.toSubr1("FALSE",Data.class,
                             "isFalse",boolean.class);
    static public final Subr _IDENTITY
        = SystemSubr.toSubr1("IDENTITY",Data.class,"identity", false);
    static public final Subr _ZERO
        = SystemSubr.toSubr0("ZERO",Data.class,"zeroFixnum");
    static public final Subr _ZERO_FIXNUM
        = SystemSubr.toSubr0("ZERO:FIXNUM",Data.class,"zeroInt");
    static public final Subr _ZERO_FLONUM
        = SystemSubr.toSubr0("ZERO:FLONUM",Data.class,"zeroDouble");
    static public final Subr _ONE
        = SystemSubr.toSubr0("ONE",Data.class,"oneFixnum");
    static public final Subr _ONE_FIXNUM
        = SystemSubr.toSubr0("ONE:FIXNUM",Data.class,"oneInt");
    static public final Subr _ONE_FLONUM
        = SystemSubr.toSubr0("ONE:FLONUM",Data.class,"oneDouble");
    static public final Subr _IDENTITY_NUMBER
        = SystemSubr.toSubr1("IDENTITY:NUMBER",Data.class,"javaNumber");
    static public final Subr _IDENTITY_FIXNUM
        = SystemSubr.toSubr1("IDENTITY:FIXNUM",Data.class,
                             "identityInt",int.class);
    static public final Subr _IDENTITY_FLONUM
        = SystemSubr.toSubr1("IDENTITY:FLONUM",Data.class,
                             "identityDouble",double.class);
    static public final Subr _IDENTITY_BIGNUM
        = SystemSubr.toSubr1("IDENTITY_BIGNUM",Data.class,"bignum");
    static public final Subr _IDENTITY_CHARACTER
        = SystemSubr.toSubr1("IDENTITY:CHARACTER",Data.class,
                             "identityChar",char.class);
    static public final Subr FLONUM_TO_FIXNUM
        = SystemSubr.toSubr1("FLONUM-TO-FIXNUM",Conv.class,
                             "toInt",double.class);
    static public final Subr FIXNUM_TO_FLONUM
        = SystemSubr.toSubr1("FIXNUM-TO-FLONUM",Conv.class,
                             "toDouble",int.class);
    static public final Subr CHARACTER_TO_FIXNUM
        = SystemSubr.toSubr1("CHARACTER-TO-FIXNUM",Conv.class,
                             "toInt",char.class);
    static public final Subr FIXNUM_TO_CHARACTER
        = SystemSubr.toSubr1("FIXNUM-TO-CHARACTER",Conv.class,
                             "toChar",int.class);
    static public final Subr _EQ_FIXNUM
        = SystemSubr.toSubr2("EQ:FIXNUM",Numbers.class,
                             "isEqInt",int.class,int.class);
    static public final Subr _EQ_FLONUM
        = SystemSubr.toSubr2("EQ:FLONUM",Numbers.class,
                             "isEqDouble",double.class,double.class);
    static public final Subr _GT_FIXNUM
        = SystemSubr.toSubr2("GT:FIXNUM",Numbers.class,
                             "isGtInt",int.class,int.class);
    static public final Subr _GT_FLONUM
        = SystemSubr.toSubr2("GT:FLONUM",Numbers.class,
                             "isGtDouble",double.class,double.class);
    static public final Subr _LT_FIXNUM
        = SystemSubr.toSubr2("LT:FIXNUM",Numbers.class,
                             "isLtInt",int.class,int.class);
    static public final Subr _LT_FLONUM
        = SystemSubr.toSubr2("LT:FLONUM",Numbers.class,
                             "isLtDouble",double.class,double.class);
    static public final Subr _GE_FIXNUM
        = SystemSubr.toSubr2("GE:FIXNUM",Numbers.class,
                             "isGeInt",int.class,int.class);
    static public final Subr _GE_FLONUM
        = SystemSubr.toSubr2("GE:FLONUM",Numbers.class,
                             "isGeDouble",double.class,double.class);
    static public final Subr _LE_FIXNUM
        = SystemSubr.toSubr2("LE:FIXNUM",Numbers.class,
                             "isLeInt",int.class,int.class);
    static public final Subr _LE_FLONUM
        = SystemSubr.toSubr2("LE:FLONUM",Numbers.class,
                             "isLeDouble",double.class,double.class);
    static public final Subr _NEGATE_FIXNUM
        = SystemSubr.toSubr1("NEGATE:FIXNUM",Numbers.class,
                             "negateInt",int.class);
    static public final Subr _NEGATE_FLONUM
        = SystemSubr.toSubr1("NEGATE:FLONUM",Numbers.class,
                             "negateDouble",double.class);
    static public final Subr _INVERSE_FLONUM
        = SystemSubr.toSubr1("INVERSE:FLONUM",Numbers.class,
                             "inverseDouble",double.class);
    static public final Subr _APPEND_BIN
        = SystemSubr.toSubr2("APPEND:BIN",Lists.class,"append", false);
    static public final Subr _NCONC_BIN
        = SystemSubr.toSubr2("NCONC:BIN",Lists.class,"nconc", false);
    static public final Subr _PLUS_BIN
        = SystemSubr.toSubr2("PLUS:BIN",Numbers.class,"add");
    static public final Subr _ADD_FIXNUM_BIN
        = SystemSubr.toSubr2("+:BIN",Numbers.class,
                             "addInt",int.class,int.class);
    static public final Subr _ADD_FLONUM_BIN
        = SystemSubr.toSubr2("+$:BIN",Numbers.class,
                             "addDouble",double.class,double.class);
    static public final Subr _DIFFERENCE_BIN
        = SystemSubr.toSubr2("DIFFERENCE:BIN",Numbers.class,"sub");
    static public final Subr _SUB_FIXNUM_BIN
        = SystemSubr.toSubr2("-:BIN",Numbers.class,
                             "subInt",int.class,int.class);
    static public final Subr _SUB_FLONUM_BIN
        = SystemSubr.toSubr2("-$:BIN",Numbers.class,
                             "subDouble",double.class,double.class);
    static public final Subr _TIMES_BIN
        = SystemSubr.toSubr2("TIMES:BIN",Numbers.class,"mul");
    static public final Subr _MUL_FIXNUM_BIN
        = SystemSubr.toSubr2("*:BIN",Numbers.class,
                             "mulInt",int.class,int.class);
    static public final Subr _MUL_FLONUM_BIN
        = SystemSubr.toSubr2("*$:BIN",Numbers.class,
                             "mulDouble",double.class,double.class);
    static public final Subr _QUOTIENT_BIN
        = SystemSubr.toSubr2("QUOTIENT:BIN",Numbers.class,"div");
    static public final Subr _DIV_FIXNUM_BIN
        = SystemSubr.toSubr2("//:BIN",Numbers.class,
                             "divInt",int.class,int.class);
    static public final Subr _DIV_FLONUM_BIN
        = SystemSubr.toSubr2("//$:BIN",Numbers.class,
                             "divDouble",double.class,double.class);
    static public final Subr _EQUALP_BIN
        = SystemSubr.toSubr2("EQUALP:BIN",Numbers.class,"isEq");
    static public final Subr _GREATERP_BIN
        = SystemSubr.toSubr2("GREATERP:BIN",Numbers.class,"isGt");
    static public final Subr _LESSP_BIN
        = SystemSubr.toSubr2("LESSP:BIN",Numbers.class,"isLt");
    static public final Subr _GREATER_OR_EQUALP_BIN
        = SystemSubr.toSubr2("GREATER-OR-EQUALP:BIN",Numbers.class,"isGe");
    static public final Subr _LESS_OR_EQUALP_BIN
        = SystemSubr.toSubr2("LESS-OR-EQUALP:BIN",Numbers.class,"isLe");
    static public final Subr _MAX_BIN
        = SystemSubr.toSubr2("MAX:BIN",Numbers.class,"max");
    static public final Subr _MIN_BIN
        = SystemSubr.toSubr2("MIN:BIN",Numbers.class,"min");

    static public final Subr LOAD_SUBRS = new LOAD_SUBRS();
    static public final Subr LOAD_RESOURCE = new LOAD_RESOURCE();
    static public final Subr IMPORT_SYMBOLS = new IMPORT_SYMBOLS();
    static public final Subr IMPORT_SUBRS = new IMPORT_SUBRS();
    static public final Subr CONVERT_FORM = new CONVERT_FORM();
    static public final Subr MACROEXPAND_FORM = new MACROEXPAND_FORM();
    static public final Subr COMPILE_FORM = new COMPILE_FORM();
    static public final Subr COMPILE_CALL = new COMPILE_CALL();
    static public final Subr CALC_ARG_TYPES = new CALC_ARG_TYPES();
    static public final Subr CONSTANT_FORM_P = new CONSTANT_FORM_P();
    static public final Subr CONSTANT_VALUE = new CONSTANT_VALUE();
    static public final Subr FIXNUM_TYPE_P
        = SystemSubr.toSubr1("FIXNUM-TYPE-P",Classes.class,
                             "isFixnumType",Class.class);
    static public final Subr FLONUM_TYPE_P
        = SystemSubr.toSubr1("FLONUM-TYPE-P",Classes.class,
                             "isFlonumType",Class.class);
    static public final Subr CHARACTER_TYPE_P
        = SystemSubr.toSubr1("CHARACTER-TYPE-P",Classes.class,
                             "isCharacterType",Class.class);
    static public final Subr PREDICATE_TYPE_P
        = SystemSubr.toSubr1("PREDICATE-TYPE-P",Classes.class,
                             "isPredicateType",Class.class);
    static public final Subr BQ_EXPAND = new BQ_EXPAND();
    static public final Subr DO_EXPAND = new DO_EXPAND();
    static public final Subr FIND_DECLS = new FIND_DECLS();
    static public final Subr WRAP_INPUT_STREAM
        = SystemSubr.toSubr2("WRAP-INPUT-STREAM",
                             IO.class,"wrapInputStream",
                             InputStream.class,Object.class);
    static public final Subr WRAP_OUTPUT_STREAM
        = SystemSubr.toSubr2("WRAP-OUTPUT-STREAM",
                             IO.class,"wrapOutputStream",
                             OutputStream.class,Object.class);
    static public final Subr WRAP_READER
        = SystemSubr.toSubr2("WRAP-READER",
                             IO.class,"wrapReader",
                             Reader.class,Object.class);
    static public final Subr WRAP_WRITER
        = SystemSubr.toSubr2("WRAP-WRITER",
                             IO.class,"wrapWriter",
                             Writer.class,Object.class);

    static public final Subr GET_SYSTEM_PROPERTY
        = new GET_SYSTEM_PROPERTY();
    static public final Subr SYSTEM_PROPERTY_KEYS
        = new SYSTEM_PROPERTY_KEYS();
    static public final Subr CURRENT_TIME = new CURRENT_TIME();
    static public final Subr DATE = new DATE();
    static public final Subr TOTAL_MEMORY = new TOTAL_MEMORY();
    static public final Subr FREE_MEMORY = new FREE_MEMORY();
    static public final Subr EXEC = new EXEC();
    static public final Subr DUMP_PACKAGE = new DUMP_PACKAGE();
    static public final Subr DUMP_ENV = new DUMP_ENV();
    static public final Subr DUMP_PENV = new DUMP_PENV();

    /*
     * compiler & loader support functions
     */

    static class LOAD_RESOURCE extends SystemSubr.SUBR {
        static final Object keys
            = Lists.list(Symbols.KW_INPUT_FILE_ENCODING);
        LOAD_RESOURCE() { super("LOAD-RESOURCE",1,0,false,keys,false); }
        public Object doCall(Env env) {
            Object r0 = super.required(0, env);
            Object k0 = super.keyword(0, env);
            String in = Data.string(r0);
            return Loader.loadResource(in, k0, env);
        }
    }
    static class LOAD_SUBRS extends SystemSubr.SUBR1 {
        LOAD_SUBRS() { super("LOAD-SUBRS"); }
        public Object call1(Object r0, Env env) {
            return Loader.loadSubrs(r0, env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class IMPORT_SYMBOLS extends SystemSubr.SUBR {
        IMPORT_SYMBOLS() {
            super("IMPORT-SYMBOLS",1,1,false,Symbols.NIL,false); }
        protected Object doCall(Env env)
            throws IllegalAccessException, ClassNotFoundException {
            Object r0 = super.required(0, env);
            Object o0 = super.optional(0, env);
            ClassLoader cl = Loader.getClassLoader(env);
            Class c = Class.forName(Data.string(r0), true, cl);
            return Loader.impSymbols(c, o0, env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class IMPORT_SUBRS extends SystemSubr.SUBR {
        IMPORT_SUBRS() { super("IMPORT-SUBRS",2,2,false,Symbols.NIL,false); }
        protected Object doCall(Env env)
            throws IllegalAccessException, ClassNotFoundException {
            Object r0 = super.required(0, env);
            Object r1 = super.required(1, env);
            Object o0 = super.isOptional(0, env)
                ? super.optional(0, env)
                : Package.get(env);
            Object o1 = super.optional(1, env);
            ClassLoader cl = Loader.getClassLoader(env);
            Class c = Class.forName(Data.string(r0), true, cl);
            return Loader.impSubrs(o0, c, Data.symbol(r1), o1, env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class CONVERT_FORM extends SystemSubr.SUBR1 {
        CONVERT_FORM() { super("CONVERT-FORM"); }
        public Object call1(Object r0, Env env) {
            return Compiler.convertForm(r0, env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class MACROEXPAND_FORM extends SystemSubr.SUBR1 {
        MACROEXPAND_FORM() { super("MACROEXPAND-FORM"); }
        public Object call1(Object r0, Env env) {
            return Compiler.macroexpandForm(r0, env);
        }
    }
    static class COMPILE_FORM extends SystemSubr.SUBR1 {
        COMPILE_FORM() { super("COMPILE-FORM"); }
        public Object call1(Object r0, Env env) {
            return Compiler.compileForm(r0, env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class COMPILE_CALL extends SystemSubr.SUBR2 {
        COMPILE_CALL() { super("COMPILE-CALL"); }
        public Object call2(Object r0, Object r1, Env env) {
            return Compiler.compileCall(Data.symbol(r0), r1, env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class CALC_ARG_TYPES extends SystemSubr.SUBR1 {
        CALC_ARG_TYPES() { super("CALC-ARG-TYPES"); }
        public Object call1(Object r0, Env env) {
            return Compiler.calcArgTypes(r0, env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class CONSTANT_FORM_P extends SystemSubr.SUBR1 {
        CONSTANT_FORM_P() { super("CONSTANT-FORM-P"); }
        public Object call1(Object r0, Env env) {
            return Data.toPredicate(Compiler.isConstant(r0, env));
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class CONSTANT_VALUE extends SystemSubr.SUBR1 {
        CONSTANT_VALUE() { super("CONSTANT-VALUE"); }
        public Object call1(Object r0, Env env) {
            return Compiler.constantValue(r0, env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }

    /*
     * helper functions for macro expansion.
     */

    static class BQ_EXPAND extends SystemSubr.SUBR1 {
        BQ_EXPAND() { super("BQ-EXPAND"); }
        public Object call1(Object r0, Env env) {
            return Backquote.expand(r0);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class DO_EXPAND extends SystemSubr.SUBR5 {
        DO_EXPAND() { super("DO-EXPAND"); }
        public Object call5(Object r0, Object r1, Object r2,
                            Object r3, Object r4, Env env) {
            return Do.expand(r0, r1, r2, Data.symbol(r3),
                             Data.symbol(r4)/*, env*/);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class FIND_DECLS extends SystemSubr.SUBR1 {
        FIND_DECLS() { super("FIND-DECLS"); }
        public Object call1(Object r0, Env env) {
            return DeclInfo.findDecls(r0);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }

    /*
     * system functions
     */

    static class GET_SYSTEM_PROPERTY extends SystemSubr.SUBR1 {
        GET_SYSTEM_PROPERTY() { super("GET-SYSTEM-PROPERTY"); }
        public Object call1(Object r0, Env env) {
            return System.getProperty(Data.string(r0));
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class SYSTEM_PROPERTY_KEYS extends SystemSubr.SUBR0 {
        SYSTEM_PROPERTY_KEYS() { super("SYSTEM-PROPERTY-KEYS"); }
        public Object call0(Env env) {
            Set keys = System.getProperties().keySet();
            return Lists.toList(new TreeSet(keys).toArray());
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class CURRENT_TIME extends SystemSubr.SUBR0 {
        CURRENT_TIME() { super("CURRENT-TIME"); }
        public Object call0(Env env) {
            return BigInteger.valueOf(System.currentTimeMillis());
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class DATE extends SystemSubr.SUBR0 {
        DATE() { super("DATE"); }
        public Object call0(Env env) {
            return new java.util.Date();
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class TOTAL_MEMORY extends SystemSubr.SUBR0 {
        TOTAL_MEMORY() { super("TOTAL-MEMORY"); }
        public Object call0(Env env) {
            return BigInteger.valueOf(Runtime.getRuntime().totalMemory());
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class FREE_MEMORY extends SystemSubr.SUBR0 {
        FREE_MEMORY() { super("FREE-MEMORY"); }
        public Object call0(Env env) {
            return BigInteger.valueOf(Runtime.getRuntime().freeMemory());
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class EXEC extends SystemSubr.SUBR {
        static final Subr DEFAULT_INPUT_HANDLER
            = new SystemSubr.SUBR1("EXEC-DEFAULT-INPUT-HANDLER") {
                    public Object call1(Object r0, Env env)
                        throws IOException {
                        InputStream src = Data.inputStream
                            (env.get(Symbols.SYSTEM_BIN_IN));
                        OutputStream dst = Data.outputStream(r0);
                        int b;
                        while ((b = src.read()) != -1) {
                            dst.write(b); dst.flush();
                        }
                        dst.flush();
                        return Symbols.NIL;
                    }
                    public boolean canProduceValues(int nargs) {
                        return false;
                    }
                };
        static final Subr DEFAULT_OUTPUT_HANDLER
            = new SystemSubr.SUBR1("EXEC-DEFAULT-OUTPUT-HANDLER") {
                    public Object call1(Object r0, Env env)
                        throws IOException {
                        InputStream src = Data.inputStream(r0);
                        OutputStream dst = Data.outputStream
                            (env.get(Symbols.SYSTEM_BIN_OUT));
                        int b;
                        while ((b = src.read()) != -1) {
                            dst.write(b); dst.flush();
                        }
                        dst.flush();
                        return Symbols.NIL;
                    }
                    public boolean canProduceValues(int nargs) {
                        return false;
                    }
                };
        static final Subr DEFAULT_ERROR_HANDLER
            = new SystemSubr.SUBR1("EXEC-DEFAULT-ERROR-HANDLER") {
                    public Object call1(Object r0, Env env)
                        throws IOException {
                        InputStream src = Data.inputStream(r0);
                        OutputStream dst = Data.outputStream
                            (env.get(Symbols.SYSTEM_BIN_ERR));
                        int b;
                        while ((b = src.read()) != -1) {
                            dst.write(b); dst.flush();
                        }
                        dst.flush();
                        return Symbols.NIL;
                    }
                    public boolean canProduceValues(int nargs) {
                        return false;
                    }
                };
        static final Object keys
            = Lists.list(Symbols.KW_INPUT,
                         Symbols.KW_OUTPUT,
                         Symbols.KW_ERROR);
        EXEC() { super("EXEC",1,0,false,keys,false); }
        protected Object doCall(Env env)
            throws IOException, InterruptedException {
            Object r0 = super.required(0, env);
            Object k0 = super.isKeyword(0,env)
                ? super.keyword(0,env)
                : Symbols.NIL;
            Object k1 = super.isKeyword(1,env)
                ? super.keyword(1,env)
                : Symbols.T;
            Object k2 = super.isKeyword(2,env)
                ? super.keyword(2,env)
                : Symbols.T;
            String cmd = Data.string(r0);
            Function fIn;
            if (k0 == Symbols.NIL) {
                fIn = null;
            }
            else if (k0 == Symbols.T) {
                fIn = DEFAULT_INPUT_HANDLER;
            }
            else {
                fIn = Data.function(k0);
            }
            Function fOut;
            if (k1 == Symbols.NIL) {
                fOut = null;
            }
            else if (k1 == Symbols.T) {
                fOut = DEFAULT_OUTPUT_HANDLER;
            }
            else {
                fOut = Data.function(k1);
            }
            Function fErr;
            if (k2 == Symbols.NIL) {
                fErr = null;
            }
            else if (k2 == Symbols.T) {
                fErr = DEFAULT_ERROR_HANDLER;
            }
            else {
                fErr= Data.function(k2);
            }
            Runtime rt = Runtime.getRuntime();
            Process pr = rt.exec(cmd);
            Thread tIn = null;
            if (fIn != null) {
                OutputStream outToPrIn = IO.wrapOutputStream
                    (pr.getOutputStream(),
                     Lists.list(Symbols.KW_USE_BUFFER));
                tIn = makeThread(fIn, outToPrIn, env);
                tIn.start();
            }
            Thread tOut = null;
            if (fOut != null) {
                InputStream inFromPrOut = IO.wrapInputStream
                    (pr.getInputStream(),
                     Lists.list(Symbols.KW_USE_BUFFER));
                tOut = makeThread(fOut, inFromPrOut, env);
                tOut.start();
            }
            Thread tErr = null;
            if (fErr != null) {
                InputStream inFromPrErr = IO.wrapInputStream
                    (pr.getErrorStream(),
                     Lists.list(Symbols.KW_USE_BUFFER));
                tErr = makeThread(fErr, inFromPrErr, env);
                tErr.start();
            }
            pr.waitFor();
            if (tIn != null)
                tIn.join();
            if (tOut != null)
                tOut.join();
            if (tErr != null)
                tErr.join();
            return Data.toFixnum(pr.exitValue());
        }
        private Thread makeThread(final Function fun,
                                  final Object stream,
                                  final Env env) {
            return new Thread() {
                /*
                 * XXX:
                 * Sharing a single lisp instance with multiple threads
                 * is NOT safe!
                 */
                public void run() {
                    try {
                        Funcall.funcall1(fun, stream, env);
                    } catch (lapin.eval.NonLocalExit e) {
                        Printer.printException(e.exitFailed(),
                                               Symbols.NIL, env);
                    } catch (lapin.util.TracableException e) {
                        e.push(this);
                        Printer.printException(e, Symbols.NIL, env);
                        Printer.printBackTrace(e, Symbols.NIL, env);
                    } catch (java.lang.RuntimeException e) {
                        Printer.printException(e, Symbols.NIL, env);
                    } catch (java.lang.Error e) {
                        Printer.printException(e, Symbols.NIL, env);
                    } finally {
                        IO.close(stream);
                    }
                }};
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class DUMP_ENV extends SystemSubr.SUBR {
        DUMP_ENV() { super("DUMP-ENV",0,1,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object o0 = super.optional(0, env);
            env.dump(o0, env);
            return Symbols.T;
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class DUMP_PENV extends SystemSubr.SUBR {
        DUMP_PENV() { super("DUMP-PENV",0,1,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object o0 = super.optional(0, env);
            env.lisp().getPenv().dump(o0, env);
            return Symbols.T;
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class DUMP_PACKAGE extends SystemSubr.SUBR {
        DUMP_PACKAGE() { super("DUMP-PACKAGE",0,2,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object o0 = super.isOptional(0, env)
                ? super.optional(0, env)
                : Package.get(env);
            Object o1 = super.optional(1, env);
            env.lisp().getObarray().dumpPkg(o0, o1, env);
            return Symbols.T;
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    private SysSubrs() {}
}
