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
import lapin.comp.Compiler;
import lapin.eval.Evaluator;
import lapin.eval.Funcall;
import lapin.eval.Macro;
import lapin.eval.Map;
import lapin.eval.NonLocalExit;
import lapin.function.Arrays;
import lapin.function.Callable0;
import lapin.function.Callable1;
import lapin.function.Callable2;
import lapin.function.Callable0r;
import lapin.function.Callable1r;
import lapin.function.Callable2r;
import lapin.function.Expr;
import lapin.function.Function;
import lapin.function.Subr;
import lapin.function.SystemSubr;
import lapin.io.IO;
import lapin.io.Reader;
import lapin.io.Printer;
import lapin.lang.Conv;
import lapin.lang.Data;
import lapin.lang.Env;
import lapin.lang.Hunks;
import lapin.lang.ProgramException;
import lapin.lang.Lists;
import lapin.lang.Numbers;
import lapin.lang.Obarray;
import lapin.lang.Package;
import lapin.lang.Symbol;
import lapin.lang.Symbols;
import lapin.lang.Values;
import lapin.load.Loader;
import lapin.util.ListBuilder;
//import lapin.util.Logger;
//import java.io.StringReader;
import java.io.StringWriter;
import java.lang.reflect.Method;

public final class LispSubrs {
    static public final Subr READ = new READ();
    static public final Subr READ_PRESERVING_WHITESPACE
        = new READ_PRESERVING_WHITESPACE();
    static public final Subr READ_FROM_STRING
        = new READ_FROM_STRING();
    static public final Subr READ_CHAR = new READ_CHAR();
    static public final Subr UNREAD_CHAR = new UNREAD_CHAR();
    static public final Subr PEEK_CHAR = new PEEK_CHAR();
    static public final Subr LISTEN = new LISTEN();
    static public final Subr READ_LINE = new READ_LINE();
    static public final Subr READ_CHAR_NO_HANG
        = new READ_CHAR_NO_HANG();
    static public final Subr PEEK_CHAR_NO_HANG
        = new PEEK_CHAR_NO_HANG();
    static public final Subr READ_BYTE = new READ_BYTE();
    static public final Subr FORMAT = new FORMAT();
    static public final Subr PRIN1 = new PRIN1();
    static public final Subr PRINC = new PRINC();
    static public final Subr PRINT = new PRINT();
    static public final Subr WRITE_CHAR = new WRITE_CHAR();
    static public final Subr WRITE_STRING = new WRITE_STRING();
    static public final Subr WRITE_LINE = new WRITE_LINE();
    static public final Subr WRITE_BYTE = new WRITE_BYTE();
    static public final Subr TERPRI = new TERPRI();
    static public final Subr OPEN = new OPEN();
    static public final Subr CLOSE = new CLOSE();
    static public final Subr MAKE_STRING_INPUT_STREAM
        = new MAKE_STRING_INPUT_STREAM();
    static public final Subr MAKE_STRING_OUTPUT_STREAM
        = new MAKE_STRING_OUTPUT_STREAM();
    static public final Subr GET_OUTPUT_STREAM_STRING
        = new GET_OUTPUT_STREAM_STRING();
    static public final Subr EVAL = new EVAL();
    static public final Subr APPLY = new APPLY();
    static public final Subr FUNCALL = new FUNCALL();
    static public final Subr MACROEXPAND_1 = new MACROEXPAND_1();
    static public final Subr MACROEXPAND = new MACROEXPAND();
    static public final Subr CONSTANTP = new CONSTANTP();
    static public final Subr ERROR = new ERROR();
    static public final Subr MAPCAR = new MAPCAR();
    static public final Subr MAPLIST = new MAPLIST();
    static public final Subr MAPC = new MAPC();
    static public final Subr MAPL = new MAPL();
    static public final Subr TYPEP
        = SystemSubr.toSubr1("TYPEP",Data.class,"typep");
    static public final Subr NOT
        = SystemSubr.toSubr1("NOT",Data.class,"isNot");
    static public final Subr NULL
        = SystemSubr.toSubr1("NULL",Data.class,"isNot");
    static public final Subr ATOM
        = SystemSubr.toSubr1("ATOM",Data.class,"isAtom");
    static public final Subr LISTP
        = SystemSubr.toSubr1("LISTP",Data.class,"isList");
    static public final Subr SYMBOLP
        = SystemSubr.toSubr1("SYMBOLP",Data.class,"isSymbol");
    static public final Subr PAIRP
        = SystemSubr.toSubr1("PAIRP",Data.class,"isPair");
    static public final Subr FIXNUMP
        = SystemSubr.toSubr1("FIXNUMP",Data.class,"isFixnum");
    static public final Subr FLOATP
        = SystemSubr.toSubr1("FLOATP",Data.class,"isFlonum");
    static public final Subr BIGP
        = SystemSubr.toSubr1("BIGP",Data.class,"isBignum");
    static public final Subr JAVA_NUMBER_P
        = SystemSubr.toSubr1("JAVA-NUMBER-P",Data.class,"isJavaNumber");
    static public final Subr CHARACTERP
        = SystemSubr.toSubr1("CHARACTERP",Data.class,"isCharacter");
    static public final Subr STRINGP
        = SystemSubr.toSubr1("STRINGP",Data.class,"isString");
    static public final Subr FUNCTIONP
        = SystemSubr.toSubr1("FUNCTIONP",Data.class,"isFunction");
    static public final Subr SUBRP
        = SystemSubr.toSubr1("SUBRP",Data.class,"isSubr");
    static public final Subr EXPRP
        = SystemSubr.toSubr1("EXPRP",Data.class,"isExpr");
    static public final Subr ARRAYP
        = SystemSubr.toSubr1("ARRAYP",Data.class,"isArray");
    static public final Subr HUNKP
        = SystemSubr.toSubr1("HUNKP",Data.class,"isHunk");
    static public final Subr INPUT_STREAM_P
        = SystemSubr.toSubr1("INPUT-STREAM-P",Data.class,"isInputStream");
    static public final Subr OUTPUT_STREAM_P
        = SystemSubr.toSubr1("OUTPUT-STREAM-P",Data.class,"isOutputStream");
    static public final Subr READERP
        = SystemSubr.toSubr1("READERP",Data.class,"isReader");
    static public final Subr WRITERP
        = SystemSubr.toSubr1("WRITERP",Data.class,"isWriter");
    static public final Subr EQ
        = SystemSubr.toSubr2("EQ",Data.class,"isEq");
    static public final Subr EQUAL
        = SystemSubr.toSubr2("EQUAL",Data.class,"isEqual");
    static public final Subr ARRAYDIMS
        = SystemSubr.toSubr1("ARRAYDIMS",
                             lapin.lang.Arrays.class,"arraydims");
    static public final Subr ARRAY_TYPE
        = SystemSubr.toSubr1("ARRAY-TYPE",
                             lapin.lang.Arrays.class,"arrayType");
    static public final Subr MAKHUNK
         = SystemSubr.toSubr1("MAKHUNK",Hunks.class,
                              "make",int.class);
    static public final Subr HUNKSIZE
         = SystemSubr.toSubr1("HUNKSIZE",Hunks.class,"length");
    static public final Subr CXR
         = SystemSubr.toSubr2("CXR",Hunks.class,
                              "cxr",int.class,Object.class,
                              false);
    static public final Subr RPLACX
         = SystemSubr.toSubr3("RPLACX",Hunks.class,
                              "rplacx",int.class,Object.class,Object.class,
                              false);
    static public final Subr HUNK = new HUNK();
    static public final Subr CONS
        = SystemSubr.toSubr2("CONS",Lists.class,"cons", false);
    static public final Subr CAR
        = SystemSubr.toSubr1("CAR",Lists.class,"car", false);
    static public final Subr CDR
        = SystemSubr.toSubr1("CDR",Lists.class,"cdr", false);
    static public final Subr CAAR
        = SystemSubr.toSubr1("CAAR",Lists.class,"caar", false);
    static public final Subr CADR
        = SystemSubr.toSubr1("CADR",Lists.class,"cadr", false);
    static public final Subr CDAR
        = SystemSubr.toSubr1("CDAR",Lists.class,"cdar", false);
    static public final Subr CDDR
        = SystemSubr.toSubr1("CDDR",Lists.class,"cddr", false);
    static public final Subr CAAAR
        = SystemSubr.toSubr1("CAAAR",Lists.class,"caaar", false);
    static public final Subr CAADR
        = SystemSubr.toSubr1("CAADR",Lists.class,"caadr", false);
    static public final Subr CADAR
        = SystemSubr.toSubr1("CADAR",Lists.class,"cadar", false);
    static public final Subr CDAAR
        = SystemSubr.toSubr1("CDAAR",Lists.class,"cdaar", false);
    static public final Subr CADDR
        = SystemSubr.toSubr1("CADDR",Lists.class,"caddr", false);
    static public final Subr CDADR
        = SystemSubr.toSubr1("CDADR",Lists.class,"cdadr", false);
    static public final Subr CDDAR
        = SystemSubr.toSubr1("CDDAR",Lists.class,"cddar", false);
    static public final Subr CDDDR
        = SystemSubr.toSubr1("CDDDR",Lists.class,"cdddr", false);
    static public final Subr CAAAAR
        = SystemSubr.toSubr1("CAAAAR",Lists.class,"caaaar", false);
    static public final Subr CAAADR
        = SystemSubr.toSubr1("CAAADR",Lists.class,"caaadr", false);
    static public final Subr CAADAR
        = SystemSubr.toSubr1("CAADAR",Lists.class,"caadar", false);
    static public final Subr CADAAR
        = SystemSubr.toSubr1("CADAAR",Lists.class,"cadaar", false);
    static public final Subr CDAAAR
        = SystemSubr.toSubr1("CDAAAR",Lists.class,"cdaaar", false);
    static public final Subr CAADDR
        = SystemSubr.toSubr1("CAADDR",Lists.class,"caaddr", false);
    static public final Subr CADADR
        = SystemSubr.toSubr1("CADADR",Lists.class,"cadadr", false);
    static public final Subr CDAADR
        = SystemSubr.toSubr1("CDAADR",Lists.class,"cdaadr", false);
    static public final Subr CADDAR
        = SystemSubr.toSubr1("CADDAR",Lists.class,"caddar", false);
    static public final Subr CDADAR
        = SystemSubr.toSubr1("CDADAR",Lists.class,"cdadar", false);
    static public final Subr CDDAAR
        = SystemSubr.toSubr1("CDDAAR",Lists.class,"cddaar", false);
    static public final Subr CADDDR 
        = SystemSubr.toSubr1("CADDDR",Lists.class,"cadddr", false);
    static public final Subr CDADDR
        = SystemSubr.toSubr1("CDADDR",Lists.class,"cdaddr", false);
    static public final Subr CDDADR
        = SystemSubr.toSubr1("CDDADR",Lists.class,"cddadr", false);
    static public final Subr CDDDAR
        = SystemSubr.toSubr1("CDDDAR",Lists.class,"cdddar", false);
    static public final Subr CDDDDR
        = SystemSubr.toSubr1("CDDDDR",Lists.class,"cddddr", false);
    static public final Subr FIRST
        = SystemSubr.toSubr1("FIRST",Lists.class,"first", false);
    static public final Subr SECOND
        = SystemSubr.toSubr1("SECOND",Lists.class,"second", false);
    static public final Subr THIRD
        = SystemSubr.toSubr1("THIRD",Lists.class,"third", false);
    static public final Subr FOURTH
        = SystemSubr.toSubr1("FOURTH",Lists.class,"fourth", false);
    static public final Subr REST
        = SystemSubr.toSubr1("REST",Lists.class,"rest", false);
    static public final Subr LIST = new LIST();
    static public final Subr LIST2 = new LIST2();
    static public final Subr ENDP
        = SystemSubr.toSubr1("ENDP",Lists.class,"isEnd");
    static public final Subr LENGTH
        = SystemSubr.toSubr1("LENGTH",Lists.class,"length");
    static public final Subr LAST
        = SystemSubr.toSubr1("LAST",Lists.class,"last");
    static public final Subr BUTLAST
        = SystemSubr.toSubr1("BUTLAST",Lists.class,"butlast");
    static public final Subr NTH
        = SystemSubr.toSubr2("NTH",Lists.class,
                             "nth",int.class,Object.class, false);
    static public final Subr NTHCDR
        = SystemSubr.toSubr2("NTHCDR",Lists.class,
                             "nthcdr",int.class,Object.class, false);
    static public final Subr APPEND = new APPEND();
    static public final Subr REVERSE
        = SystemSubr.toSubr1("REVERSE",Lists.class,"reverse");
    static public final Subr COPY_LIST
        = SystemSubr.toSubr1("COPY-LIST",Lists.class,"copyList");
    static public final Subr RPLACA
        = SystemSubr.toSubr2("RPLACA",Lists.class,"rplaca");
    static public final Subr RPLACD
        = SystemSubr.toSubr2("RPLACD",Lists.class,"rplacd");
    static public final Subr NCONC = new NCONC();
    static public final Subr NREVERSE
        = SystemSubr.toSubr1("NREVERSE",Lists.class,"nreverse");
    static public final Subr DELETE = new DELETE();
    static public final Subr DELQ = new DELQ();
    static public final Subr MEMBER
        = SystemSubr.toSubr2("MEMBER",Lists.class,"member");
    static public final Subr MEMQ
        = SystemSubr.toSubr2("MEMQ",Lists.class,"memq");
    static public final Subr ASSOC
         = SystemSubr.toSubr2("ASSOC",Lists.class,"assoc", false);
    static public final Subr ASSQ
        = SystemSubr.toSubr2("ASSQ",Lists.class,"assq", false);
    //static public final Subr PAIRLIS
    //    = SystemSubr.toSubr3("PAIRLIS",Lists.class,"pairlis");
    static public final Subr SYMBOL_PACKAGE = new SYMBOL_PACKAGE();
    static public final Subr SYMBOL_NAME = new SYMBOL_NAME();
    static public final Subr MAKE_PACKAGE = new MAKE_PACKAGE();
    static public final Subr FIND_PACKAGE = new FIND_PACKAGE();
    static public final Subr USE_PACKAGE = new USE_PACKAGE();
    static public final Subr EXPORT = new EXPORT();
    static public final Subr IMPORT = new IMPORT();
    static public final Subr INTERN = new INTERN();
    static public final Subr FIND_SYMBOL = new FIND_SYMBOL();
    static public final Subr MAPATOMS = new MAPATOMS();
    static public final Subr GET = new GET();
    static public final Subr GETL = new GETL();
    static public final Subr PUTPROP = new PUTPROP();
    static public final Subr REMPROP = new REMPROP();
    static public final Subr PLIST = new PLIST();
    static public final Subr SETPLIST = new SETPLIST();
    static public final Subr GENSYM = new GENSYM();
    static public final Subr SET = new SET();
    static public final Subr BOUNDP = new BOUNDP();
    static public final Subr VALUES = new VALUES();
    static public final Subr FIXNUM
        = SystemSubr.toSubr1("FIXNUM",Conv.class,"toFixnum");
    static public final Subr BIGNUM
        = SystemSubr.toSubr1("BIGNUM",Conv.class,"toBignum");
    static public final Subr FLONUM
        = SystemSubr.toSubr1("FLONUM",Conv.class,"toFlonum");
    static public final Subr CHARACTER
        = SystemSubr.toSubr1("CHARACTER",Conv.class,"toCharacter");
    //static public final Subr FLOAT
    //    = SystemSubr.toSubr1("FLOAT",Conv.class,"flo");
    static public final Subr FIX
        = SystemSubr.toSubr1("FIX",Numbers.class,"fix");
    static public final Subr FIIX
        = SystemSubr.toSubr1("IFIX",Numbers.class,"ifix");
    static public final Subr NUMBERP
        = SystemSubr.toSubr1("NUMBERP",Numbers.class,"isNumber");
    static public final Subr FIXP
        = SystemSubr.toSubr1("FIXP",Numbers.class,"isFix");
    static public final Subr PLUS = new PLUS();
    static public final Subr ADD_FIXNUM = new ADD_FIXNUM();
    static public final Subr ADD_FLONUM = new ADD_FLONUM();
    static public final Subr ADD1
        = SystemSubr.toSubr1("ADD1",Numbers.class,"add1");
    static public final Subr ADD1_FIXNUM
        = SystemSubr.toSubr1("1+",Numbers.class,
                             "add1Int",int.class);
    static public final Subr ADD1_FLONUM
        = SystemSubr.toSubr1("1+$",Numbers.class,
                             "add1Double",double.class);
    static public final Subr DIFFERENCE = new DIFFERENCE();
    static public final Subr SUB_FIXNUM = new SUB_FIXNUM();
    static public final Subr SUB_FLONUM = new SUB_FLONUM();
    static public final Subr SUB1
        = SystemSubr.toSubr1("SUB1",Numbers.class,"sub1");
    static public final Subr SUB1_FIXNUM
        = SystemSubr.toSubr1("1-",Numbers.class,
                             "sub1Int",int.class);
    static public final Subr SUB1_FLONUM
        = SystemSubr.toSubr1("1-$",Numbers.class,
                             "sub1Double",double.class);
    static public final Subr TIMES = new TIMES();
    static public final Subr MUL_FIXNUM = new MUL_FIXNUM();
    static public final Subr MUL_FLONUM = new MUL_FLONUM();
    static public final Subr QUOTIENT = new QUOTIENT();
    static public final Subr DIV_FIXNUM = new DIV_FIXNUM();
    static public final Subr DIV_FLONUM = new DIV_FLONUM();
    static public final Subr REMAINDER
        = SystemSubr.toSubr2("REMAINDER",Numbers.class,"mod");
    static public final Subr MOD_FIXNUM
        = SystemSubr.toSubr2("%",Numbers.class,
                             "modInt",int.class,int.class);
    //static public final Subr MOD_FIXNUM
    //    = SystemSubr.toSubr2("\\",
    //              Numbers.class,"modInt",int.class,int.class);
    static public final Subr MINUS
        = SystemSubr.toSubr1("MINUS",Numbers.class,"negate");
    static public final Subr EQUALP = new EQUALP();
    static public final Subr EQ_FIX_OR_FLONUM
        = SystemSubr.toSubr2("=",Numbers.class,"isEqFixOrFlonum");
    static public final Subr GREATERP = new GREATERP();
    static public final Subr GT_FIX_OR_FLONUM
        = SystemSubr.toSubr2(">",Numbers.class,"isGtFixOrFlonum");
    static public final Subr LESSP = new LESSP();
    static public final Subr LT_FIX_OR_FLONUM
        = SystemSubr.toSubr2("<",Numbers.class,"isLtFixOrFlonum");
    static public final Subr GREATER_OR_EQUALP = new GREATER_OR_EQUALP();
    static public final Subr GE_FIX_OR_FLONUM
        = SystemSubr.toSubr2(">=",Numbers.class,"isGeFixOrFlonum");
    static public final Subr LESS_OR_EQUALP = new LESS_OR_EQUALP();
    static public final Subr LE_FIX_OR_FLONUM
        = SystemSubr.toSubr2("<=",Numbers.class,"isLeFixOrFlonum");
    static public final Subr PLUSP
        = SystemSubr.toSubr1("PLUSP",Numbers.class,"isPlus");
    static public final Subr ZEROP
        = SystemSubr.toSubr1("ZEROP",Numbers.class,"isZero");
    static public final Subr MINUSP
        = SystemSubr.toSubr1("MINUSP",Numbers.class,"isMinus");
    static public final Subr ABS
        = SystemSubr.toSubr1("ABS",Numbers.class,"abs");
    static public final Subr ODDP
        = SystemSubr.toSubr1("ODDP",Numbers.class,"isOdd");
    static public final Subr EVENP
        = SystemSubr.toSubr1("EVENP",Numbers.class,"isEven");
    static public final Subr MAX = new MAX();
    static public final Subr MIN = new MIN();
    static public final Subr EXPT
        = SystemSubr.toSubr2("EXPT",Numbers.class,"pow");
    static public final Subr EXP
        = SystemSubr.toSubr1("EXP",Numbers.class,"exp");
    static public final Subr LOG
        = SystemSubr.toSubr1("LOG",Numbers.class,"log");
    static public final Subr SQRT
        = SystemSubr.toSubr1("SQRT",Numbers.class,"sqrt");
    static public final Subr ACOS
        = SystemSubr.toSubr1("ACOS",Numbers.class,"acos");
    static public final Subr ASIN
        = SystemSubr.toSubr1("ASIN",Numbers.class,"asin");
    static public final Subr ATAN = new ATAN();
    static public final Subr COS
        = SystemSubr.toSubr1("COS",Numbers.class,"cos");
    static public final Subr SIN
        = SystemSubr.toSubr1("SIN",Numbers.class,"sin");
    static public final Subr TAN
        = SystemSubr.toSubr1("TAN",Numbers.class,"tan");
    static public final Subr RANDOM
        = SystemSubr.toSubr0("RANDOM",Math.class,"random");
    static public final Subr MAKE_ARRAY = new MAKE_ARRAY();
    static public final Subr ARRAY_GET = new ARRAY_GET();
    static public final Subr ARRAY_SET = new ARRAY_SET();
    static public final Subr COMPILE = new COMPILE();
    static public final Subr COMPILE_FILE = new COMPILE_FILE();
    static public final Subr LOAD = new LOAD();
    static public final Subr ALARMCLOCK = new ALARMCLOCK();
    static public final Subr SYMEVAL = new SYMEVAL();
    static public final Subr EXPLODEN = new EXPLODEN();
    static public final Subr IMPLODE = new IMPLODE();


    /*
     * function definitions
     */

    /*
     * I/O functions
     */

    static class READ extends SystemSubr.SUBR {
        READ() { super("READ",0,4,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object stream = super.optional(0, env);
            Object eof_error_p = super.isOptional(1, env)
                ? super.optional(1, env) : Symbols.T;
            Object eof_value = super.optional(2, env);
            Object recursive_p = super.optional(3, env);
            return Reader.read
                (stream, eof_error_p, eof_value, recursive_p, env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class READ_PRESERVING_WHITESPACE extends SystemSubr.SUBR {
        READ_PRESERVING_WHITESPACE() {
            super("READ-PRESERVING-WHITE-SPACE",0,4,false,Symbols.NIL,false);
        }
        protected Object doCall(Env env) {
            Object stream = super.optional(0, env);
            Object eof_error_p = super.isOptional(1, env)
                ? super.optional(1, env) : Symbols.T;
            Object eof_value = super.optional(2, env);
            Object recursive_p = super.optional(3, env);
            return Reader.readPreservingWhitespace
                (stream, eof_error_p, eof_value, recursive_p, env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class READ_FROM_STRING extends SystemSubr.SUBR {
        static final Object keys
            = Lists.list(Symbols.KW_START, Symbols.KW_END,
                         Symbols.KW_PRESERVE_WHITESPACE);
        READ_FROM_STRING() {
            super("READ-FROM-STRING",1,2,false,keys,false);
        }
        protected Object doCall(Env env) {
            Object obj = super.required(0, env);
            Object eof_error_p = super.isOptional(0, env)
                ? super.optional(0, env) : Symbols.T;
            Object eof_value = super.optional(1, env);
            Object start = super.keyword(0, env);
            Object end = super.keyword(1, env);
            Object whitespace_p = super.keyword(2, env);
            String string = IO.substring(Data.string(obj), start, end);
            return Reader.readFromString
                (string, eof_error_p, eof_value, whitespace_p, env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class READ_CHAR extends SystemSubr.SUBR {
        READ_CHAR() { super("READ-CHAR",0,4,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object stream = super.optional(0, env);
            Object eof_error_p = super.isOptional(1, env)
                ? super.optional(1, env) : Symbols.T;
            Object eof_value = super.optional(2, env);
            Object recursive_p = super.optional(3, env);
            return Reader.readChar
                (stream, eof_error_p, eof_value, recursive_p, env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class UNREAD_CHAR extends SystemSubr.SUBR {
        UNREAD_CHAR() { super("UNREAD-CHAR",1,1,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object ch = super.required(0, env);
            Object stream = super.optional(0, env);
            return Reader.unreadChar(Data.character(ch), stream, env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class PEEK_CHAR extends SystemSubr.SUBR {
        PEEK_CHAR() { super("PEEK-CHAR",1,4,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object peek_type = super.required(0, env);
            Object stream = super.optional(0, env);
            Object eof_error_p = super.isOptional(1, env)
                ? super.optional(1, env) : Symbols.T;
            Object eof_value = super.optional(2, env);
            Object recursive_p = super.optional(3, env);
            return Reader.peekChar
                (peek_type, stream, eof_error_p, eof_value, recursive_p, env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class LISTEN extends SystemSubr.SUBR {
        LISTEN() { super("LISTEN",0,1,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object stream = super.optional(0, env);
            return Reader.listen(stream, env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class READ_CHAR_NO_HANG extends SystemSubr.SUBR {
        READ_CHAR_NO_HANG() {
            super("READ-CHAR-NO-HANG",0,4,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object stream = super.optional(0, env);
            Object eof_error_p = super.isOptional(1, env)
                ? super.optional(1, env) : Symbols.T;
            Object eof_value = super.optional(2, env);
            Object recursive_p = super.optional(3, env);
            return Reader.readCharNoHang
                (stream, eof_error_p, eof_value, recursive_p, env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class PEEK_CHAR_NO_HANG extends SystemSubr.SUBR {
        PEEK_CHAR_NO_HANG() {
            super("PEEK-CHAR-NO-HANG",1,4,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object peek_type = super.required(0, env);
            Object stream = super.optional(0, env);
            Object eof_error_p = super.isOptional(1, env)
                ? super.optional(1, env) : Symbols.T;
            Object eof_value = super.optional(2, env);
            Object recursive_p = super.optional(3, env);
            return Reader.peekCharNoHang
                (peek_type, stream, eof_error_p, eof_value, recursive_p, env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class READ_LINE extends SystemSubr.SUBR {
        READ_LINE() { super("READ-LINE",0,4,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object stream = super.optional(0, env);
            Object eof_error_p = super.isOptional(1, env)
                ? super.optional(1, env) : Symbols.T;
            Object eof_value = super.optional(2, env);
            Object recursive_p = super.optional(3, env);
            return Reader.readLine
                (stream, eof_error_p, eof_value, recursive_p, env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class READ_BYTE extends SystemSubr.SUBR {
        READ_BYTE() { super("READ-BYTE",1,2,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object stream = super.required(0, env);
            Object eof_error_p = super.isOptional(0, env)
                ? super.optional(1, env) : Symbols.T;
            Object eof_value = super.optional(1, env);
            return Reader.readByte(stream, eof_error_p, eof_value, env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class FORMAT extends SystemSubr.SUBR
        implements Callable2r {
        FORMAT() { super("FORMAT",2,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object r0 = super.required(0, env);
            Object r1 = super.required(1, env);
            Object rest = super.rest(env);
            return call2(r0, r1, rest, env);
        }
        public Object call2(Object r0, Object r1, Object rest, Env env) {
            if (r0 == Symbols.NIL) {
                String ctrlstr = Data.string(r1);
                return Printer.formatToString(ctrlstr, rest, env);
            }
            else if (r0 == Symbols.T) {
                String ctrlstr = Data.string(r1);
                Object stream = Symbols.NIL;
                return Printer.format(ctrlstr, rest, stream, env);
            }
            else {
                String ctrlstr = Data.string(r1);
                Object stream = r0;
                return Printer.format(ctrlstr, rest, stream, env);
            }
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class PRINC extends SystemSubr.SUBR
        implements Callable1, Callable2 {
        PRINC() { super("PRINC",1,1,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object exp = super.required(0, env);
            Object stream = super.optional(0, env);
            return Printer.princ(exp, stream, env);
        }
        public Object call1(Object r0, Env env) {
            return Printer.princ(r0, Symbols.NIL, env);
        }
        public Object call2(Object r0, Object r1, Env env) {
            return Printer.princ(r0, r1, env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class PRIN1 extends SystemSubr.SUBR
        implements Callable1, Callable2 {
        PRIN1() { super("PRIN1",1,1,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object exp = super.required(0, env);
            Object stream = super.optional(0, env);
            return Printer.prin1(exp, stream, env);
        }
        public Object call1(Object r0, Env env) {
            return Printer.prin1(r0, Symbols.NIL, env);
        }
        public Object call2(Object r0, Object r1, Env env) {
            return Printer.prin1(r0, r1, env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class PRINT extends SystemSubr.SUBR
        implements Callable1, Callable2 {
        PRINT() { super("PRINT",1,1,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object exp = super.required(0, env);
            Object stream = super.optional(0, env);
            return Printer.print(exp, stream, env);
        }
        public Object call1(Object r0, Env env) {
            return Printer.print(r0, Symbols.NIL, env);
        }
        public Object call2(Object r0, Object r1, Env env) {
            return Printer.print(r0, r1, env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class WRITE_CHAR extends SystemSubr.SUBR
        implements Callable1, Callable2 {
        WRITE_CHAR() { super("WRITE-CHAR",1,1,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object obj = super.required(0, env);
            Object stream = super.optional(0, env);
            return Printer.writeChar(Data.character(obj), stream, env);
        }
        public Object call1(Object r0, Env env) {
            return Printer.writeChar(Data.character(r0), Symbols.NIL, env);
        }
        public Object call2(Object r0, Object r1, Env env) {
            return Printer.writeChar(Data.character(r0), r1, env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class WRITE_STRING extends SystemSubr.SUBR {
        static final Object keys
            = Lists.list(Symbols.KW_START, Symbols.KW_END);
        WRITE_STRING() { super("WRITE-STRING",1,1,false,keys,false); }
        protected Object doCall(Env env) {
            Object obj = super.required(0, env);
            Object stream = super.optional(0, env);
            Object start = super.keyword(0, env);
            Object end = super.keyword(1, env);
            String string = IO.substring(Data.string(obj), start, end);
            Printer.writeString(string, stream, env);
            return obj;
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class WRITE_LINE extends SystemSubr.SUBR {
        static final Object keys
            = Lists.list(Symbols.KW_START, Symbols.KW_END);
        WRITE_LINE() { super("WRITE-LINE",1,1,false,keys,false); }
        protected Object doCall(Env env) {
            Object obj = super.required(0, env);
            Object stream = super.optional(0, env);
            Object start = super.keyword(0, env);
            Object end = super.keyword(1, env);
            String string = IO.substring(Data.string(obj), start, end);
            Printer.writeLine(string, stream, env);
            return obj;
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class WRITE_BYTE extends SystemSubr.SUBR2 {
        WRITE_BYTE() { super("WRITE-BYTE"); }
        public Object call2(Object r0, Object r1, Env env) {
            return Printer.writeByte(Data.fixnum(r0), r1, env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class TERPRI extends SystemSubr.SUBR
        implements Callable0, Callable1 {
        TERPRI() { super("TERPRI",0,1,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object stream = super.optional(0, env);
            return Printer.terpri(stream, env);
        }
        public Object call0(Env env) {
            return Printer.terpri(Symbols.NIL, env);
        }
        public Object call1(Object r0, Env env) {
            return Printer.terpri(r0, env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class OPEN extends SystemSubr.SUBR {
        static final Object keys
            = Lists.toList(new Object[]{
                    Symbols.KW_DIRECTION,
                    Symbols.KW_ELEMENT_TYPE,
                    Symbols.KW_IF_EXISTS,
                    Symbols.KW_IF_DOES_NOT_EXIST,
                    Symbols.KW_EXTERNAL_FORMAT,
                    Symbols.KW_WRAP_OPTIONS,
                });
        OPEN() { super("OPEN",1,0,false,keys,false); }
        protected Object doCall(Env env) {
            Object stream = super.required(0, env);
            Object direction = super.isKeyword(0, env)
                ? super.keyword(0, env)
                : Symbols.KW_INPUT;
            Object elemType = super.isKeyword(1, env)
                ? super.keyword(1, env)
                : Symbols.CHARACTER;
            Object ifExists = super.isKeyword(2, env)
                ? super.keyword(2, env)
                : Symbols.KW_OVERWRITE;
            Object ifDoesNotExists = super.isKeyword(3, env)
                ? super.keyword(3, env)
                : Symbols.KW_ERROR;
            Object externalFormat = super.isKeyword(4, env)
                ? super.keyword(4, env)
                : Symbols.NIL;
            Object wrapOptions = super.isKeyword(5, env)
                ? super.keyword(5, env)
                : Symbols.T;
            return IO.open(stream, direction, elemType,
                           ifExists, ifDoesNotExists,
                           externalFormat, wrapOptions);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class CLOSE extends SystemSubr.SUBR {
        static final Object keys
            = Lists.list(Symbols.KW_ABORT);
        CLOSE() { super("CLOSE",1,0,false,keys,false); }
        protected Object doCall(Env env) {
            Object stream = super.required(0, env);
            Object abort = super.keyword(0, env);
            IO.close(stream, Data.toBoolean(abort));
            return Symbols.T;
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class MAKE_STRING_INPUT_STREAM extends SystemSubr.SUBR {
        MAKE_STRING_INPUT_STREAM() {
            super("MAKE-STRING-INPUT-STREAM",1,2,false,Symbols.NIL,false);
        }
        protected Object doCall(Env env) {
            Object obj = super.required(0, env);
            Object start = super.optional(0, env);
            Object end = super.optional(1, env);
            String string = IO.substring(Data.string(obj), start, end);
            return IO.makeStringReader(string);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class MAKE_STRING_OUTPUT_STREAM extends SystemSubr.SUBR0 {
        MAKE_STRING_OUTPUT_STREAM() {
            super("MAKE-STRING-OUTPUT-STREAM"); }
        public Object call0(Env env) {
            return IO.makeStringWriter();
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class GET_OUTPUT_STREAM_STRING extends SystemSubr.SUBR1 {
        GET_OUTPUT_STREAM_STRING() {
            super("GET-OUTPUT-STREAM-STRING"); }
        public Object call1(Object r0, Env env) {
            StringWriter sw = IO.getStringWriter(r0);
            String ret = sw.toString();
            StringBuffer buf = sw.getBuffer();
            buf.delete(0, buf.length());
            return ret;
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }

    /*
     * eval/apply/funcall functions
     */

    static class EVAL extends SystemSubr.SUBR1 {
        EVAL() { super("EVAL"); }
        public Object call1(Object r0, Env env) {
            return Evaluator.eval(r0, env);
        }
    }
    static class FUNCALL extends SystemSubr.SUBR
        implements Callable1r {
        FUNCALL() { super("FUNCALL",1,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object r0 = super.required(0, env);
            Object rest = super.rest(env);
            return call1(r0, rest, env);
        }
        public Object call1(Object r0, Object rest, Env env) {
            Function fun = Data.isFunction(r0)
                ? Data.function(r0) : Evaluator.function(r0, env);
            return Funcall.funcall(fun, rest, env);
        }
    }
    static class APPLY extends SystemSubr.SUBR
        implements Callable2r {
        APPLY() { super("APPLY",2,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object r0 = super.required(0, env);
            Object r1 = super.required(1, env);
            Object rest = super.rest(env);
            return call2(r0, r1, rest, env);
        }
        public Object call2(Object r0, Object r1, Object rest, Env env) {
            Function fun = Data.isFunction(r0)
                ? Data.function(r0) : Evaluator.function(r0, env);
            /*
             * Common Lisp the Language, 2nd Edition
             * 7. Control Structure / Function Invocation
             *
             * The arguments for the function consist of the last argument
             * to apply appended to the end of a list of all the other
             * arguments to apply but the function itself; it is as if
             * all the arguments to apply except the function were given
             * to list* to create the argument list.
             *
             */
            Object args;
            if (rest == Symbols.NIL) {
                if (r1 == Symbols.NIL)
                    args = Symbols.NIL;
                else
                    args = Data.pair(r1);
            }
            else {
                args = Lists.cons(r1, null);
                Object tail = args;
                Object l = rest;
                while (true) {
                    if (Lists.isEnd(Lists.cdr(l))) {
                        Object last = Lists.car(l);
                        if (last == Symbols.NIL)
                            Lists.rplacd(tail, last);
                        else
                            Lists.rplacd(tail, Data.pair(last));
                        break;
                    }
                    else {
                        Lists.rplacd(tail, Lists.cons(Lists.car(l), null));
                        tail = Lists.cdr(tail);
                        l = Lists.cdr(l);
                    }
                }

            }
            //if (Logger.tracelevelp(env)) {
            //    Logger.trace("[apply] r0:~S", Lists.list(r0), env);
            //    Logger.trace("[apply] r1:~S", Lists.list(r1), env);
            //    Logger.trace("[apply] rest:~S", Lists.list(rest), env);
            //    Logger.trace("[apply] args:~S", Lists.list(args), env);
            //}
            return Funcall.funcall(fun, args, env);
        }
    }
    static class MACROEXPAND_1 extends SystemSubr.SUBR1 {
        MACROEXPAND_1() { super("MACROEXPAND-1"); }
        public Object call1(Object r0, Env env) {
            return Macro.expand1(r0, env);
        }
    }
    static class MACROEXPAND extends SystemSubr.SUBR1 {
        MACROEXPAND() { super("MACROEXPAND"); }
        public Object call1(Object r0, Env env) {
            return Macro.expand(r0, env);
        }
    }
    static class CONSTANTP extends SystemSubr.SUBR1 {
        CONSTANTP() { super("CONSTANTP"); }
        public Object call1(Object r0, Env env) {
            return Data.toPredicate(Evaluator.isConstant(r0, env));
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class ERROR extends SystemSubr.SUBR {
        ERROR() { super("ERROR",0,3,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object o0,o1,o2;
            if (super.isOptional(0, env))
                o0 = super.optional(0, env);
            else
                o0 = "error!";
            if (super.isOptional(1, env))
                o1 = super.optional(1, env);
            else
                o1 = null;
            if (super.isOptional(2, env))
                o2 = super.optional(2, env);
            else
                o2 = Symbols.FAIL_ACT;
            return Evaluator.exitWithError(o0, o1, o2, env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }

    /*
     * map functions
     */

    static class MAPCAR extends SystemSubr.SUBR
        implements Callable2r {
        MAPCAR() { super("MAPCAR",2,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object r0 = super.required(0, env);
            Object r1 = super.required(1, env);
            Object rest = super.rest(env);
            return call2(r0, r1, rest, env);
        }
        public Object call2(Object r0, Object r1, Object rest, Env env) {
            Function fun = Data.isFunction(r0)
                ? Data.function(r0) : Evaluator.function(r0, env);
            return Map.mapcar(fun, Lists.cons(r1, rest), env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class MAPLIST extends SystemSubr.SUBR
        implements Callable2r {
        MAPLIST() { super("MAPLIST",2,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object r0 = super.required(0, env);
            Object r1 = super.required(1, env);
            Object rest = super.rest(env);
            return call2(r0, r1, rest, env);
        }
        public Object call2(Object r0, Object r1, Object rest, Env env) {
            Function fun = Data.isFunction(r0)
                ? Data.function(r0) : Evaluator.function(r0, env);
            return Map.maplist(fun, Lists.cons(r1, rest), env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class MAPC extends SystemSubr.SUBR
        implements Callable2r {
        MAPC() { super("MAPC",2,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object r0 = super.required(0, env);
            Object r1 = super.required(1, env);
            Object rest = super.rest(env);
            return call2(r0, r1, rest, env);
        }
        public Object call2(Object r0, Object r1, Object rest, Env env) {
            Function fun = Data.isFunction(r0)
                ? Data.function(r0) : Evaluator.function(r0, env);
            return Map.mapc(fun, Lists.cons(r1, rest), env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class MAPL extends SystemSubr.SUBR
        implements Callable2r {
        MAPL() { super("MAPL",2,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object r0 = super.required(0, env);
            Object r1 = super.required(1, env);
            Object rest = super.rest(env);
            return call2(r0, r1, rest, env);
        }
        public Object call2(Object r0, Object r1, Object rest, Env env) {
            Function fun = Data.isFunction(r0)
                ? Data.function(r0) : Evaluator.function(r0, env);
            return Map.mapl(fun, Lists.cons(r1, rest), env);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }

    /*
     * hunk functions
     */

    static class HUNK extends SystemSubr.SUBR
        implements Callable0r {
        HUNK() { super("HUNK",0,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object rest = super.rest(env);
            return call0(rest, env);
        }
        public Object call0(Object rest, Env env) {
            int len = Lists.length(rest);
            if (len <= 0)
                return Symbols.NIL;
            Object hnk = Hunks.make(len);
            int i = 1;
            for (Object l = rest; !Lists.isEnd(l); l = Lists.cdr(l)) {
                if (Lists.isEnd(Lists.cdr(l))) {
                    // the last element is stored in 0th position,
                    // which corresponds to the "CDR" of the hunk.
                    Hunks.rplacx(0, hnk, Lists.car(l));
                }
                else
                    Hunks.rplacx(i++, hnk, Lists.car(l));
            }
            return hnk;
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }

    /*
     * list functions
     */

    static class LIST extends SystemSubr.SUBR
        implements Callable0r {
        LIST() { super("LIST",0,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object rest = super.rest(env);
            return call0(rest, env);
        }
        public Object call0(Object rest, Env env) {
            return Lists.copyList(rest);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class LIST2 extends SystemSubr.SUBR
        implements Callable1r {
        LIST2() { super("LIST*",1,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object o = super.required(0, env);
            Object others = super.rest(env);
            return call1(o, others, env);
        }
        public Object call1(Object o, Object others, Env env) {
            Object ret;
            if (others == Symbols.NIL)
                ret = o;
            else {
                ret = Lists.cons(o, null);
                Object tail = ret;
                Object l = others;
                while (true) {
                    if (Lists.isEnd(Lists.cdr(l))) {
                        Lists.rplacd(tail, Lists.car(l));
                        break;
                    }
                    else {
                        Lists.rplacd(tail, Lists.cons(Lists.car(l), null));
                        tail = Lists.cdr(tail);
                        l = Lists.cdr(l);
                    }
                }
            }
            return ret;
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class APPEND extends SystemSubr.SUBR
        implements Callable0, Callable1, Callable2, Callable1r {
        static final Callable0 c0 = (Callable0) SysSubrs._NIL;
        static final Callable1 c1 = (Callable1) SysSubrs._IDENTITY;
        static final Callable2 c2 = (Callable2) SysSubrs._APPEND_BIN;
        APPEND() { super("APPEND",0,1,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            //System.out.println("append:doCall");
            Object o0 = super.optional(0, env);
            Object rest = super.rest(env);
            return call1(o0, rest, env);
        }
        public Object call1(Object o0, Object rest, Env env) {
            //System.out.println("append:call1r");
            return _append(o0, rest);
        }
        private Object _append(Object o, Object rest) {
            if (rest == Symbols.NIL)
                // (length rest) == 0
                return o;
            else if (Lists.cdr(rest) == Symbols.NIL)
                // (length rest) == 1
                return Lists.append(o, Lists.car(rest));
            else
                // (length rest) > 1
                return Lists.append
                    (o, _append(Lists.car(rest),
                                Lists.cdr(rest)));
        }
        public Object call0(Env env)
            throws java.lang.Exception {
            return c0.call0(env);
        }
        public Object call1(Object arg0, Env env)
            throws java.lang.Exception {
            return c1.call1(arg0, env);
        }
        public Object call2(Object arg0, Object arg1, Env env)
            throws java.lang.Exception {
            return c2.call2(arg0, arg1, env);
        }
        public Method getTargetMethod(int nargs) {
            switch (nargs) {
            case 0:
                return ((SystemSubr) c0).getTargetMethod(nargs);
            case 1:
                return ((SystemSubr) c1).getTargetMethod(nargs);
            case 2:
                return ((SystemSubr) c2).getTargetMethod(nargs);
            default:
                return super.getTargetMethod(nargs);
            }
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class NCONC extends SystemSubr.SUBR
        implements Callable0, Callable1, Callable2, Callable1r {
        static final Callable0 c0 = (Callable0) SysSubrs._NIL;
        static final Callable1 c1 = (Callable1) SysSubrs._IDENTITY;
        static final Callable2 c2 = (Callable2) SysSubrs._NCONC_BIN;
        NCONC() { super("NCONC",0,1,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object o0 = super.optional(0, env);
            Object rest = super.rest(env);
            return call1(o0, rest, env);
        }
        public Object call1(Object o0, Object rest, Env env) {
            return _nconc(o0, rest);
        }
        private Object _nconc(Object o, Object rest) {
            if (rest == Symbols.NIL)
                // (length rest) == 0
                return o;
            else if (Lists.cdr(rest) == Symbols.NIL)
                // (length rest) == 1
                return Lists.nconc(o, Lists.car(rest));
            else
                // (length rest) > 1
                return Lists.nconc
                    (o, _nconc(Lists.car(rest),
                               Lists.cdr(rest)));
        }
        public Object call0(Env env)
            throws java.lang.Exception {
            return c0.call0(env);
        }
        public Object call1(Object arg0, Env env)
            throws java.lang.Exception {
            return c1.call1(arg0, env);
        }
        public Object call2(Object arg0, Object arg1, Env env)
            throws java.lang.Exception {
            return c2.call2(arg0, arg1, env);
        }
        public Method getTargetMethod(int nargs) {
            switch (nargs) {
            case 0:
                return ((SystemSubr) c0).getTargetMethod(nargs);
            case 1:
                return ((SystemSubr) c1).getTargetMethod(nargs);
            case 2:
                return ((SystemSubr) c2).getTargetMethod(nargs);
            default:
                return super.getTargetMethod(nargs);
            }
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }

    static class DELETE extends SystemSubr.SUBR {
        DELETE() { super("DELETE",2,1,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object r0 = super.required(0, env);
            Object r1 = super.required(1, env);
            if (!super.isOptional(0, env)) {
                return Lists.delete(r0, r1);
            }
            else {
                Object o0 = super.optional(0, env);
                return Lists.delete(r0, r1, Data.fixnum(o0).intValue());
            }
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class DELQ extends SystemSubr.SUBR {
        DELQ() { super("DELQ",2,1,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object r0 = super.required(0, env);
            Object r1 = super.required(1, env);
            if (!super.isOptional(0, env)) {
                return Lists.delq(r0, r1);
            }
            else {
                Object o0 = super.optional(0, env);
                return Lists.delq(r0, r1, Data.fixnum(o0).intValue());
            }
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }

    /*
     * symbol/plist/env functions
     */

    static class SYMBOL_PACKAGE extends SystemSubr.SUBR1 {
        SYMBOL_PACKAGE() { super("SYMBOL-PACKAGE"); }
        public Object call1(Object r0, Env env) {
            Object pkg = Data.symbol(r0).pkg();
            return pkg != null ? pkg : Symbols.NIL;
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class SYMBOL_NAME extends SystemSubr.SUBR1 {
        SYMBOL_NAME() { super("SYMBOL-NAME"); }
        public Object call1(Object r0, Env env) {
            return Data.symbol(r0).pname();
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class MAKE_PACKAGE extends SystemSubr.SUBR {
        static final Object keys
            = Lists.list(Symbols.KW_USE);
        MAKE_PACKAGE() { super("MAKE-PACKAGE",1,0,false,keys,false); }
        protected Object doCall(Env env) {
            Object r0 = super.required(0, env);
            Object k0 = super.keyword(0, env);
            return env.lisp().getObarray().mkPkg(r0, k0);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class FIND_PACKAGE extends SystemSubr.SUBR1 {
        FIND_PACKAGE() { super("FIND-PACKAGE"); }
        public Object call1(Object r0, Env env) {
            Object pkg = env.lisp().getObarray().findPkg(r0);
            return pkg != null ? pkg : Symbols.NIL;
        }
    }
    static class USE_PACKAGE extends SystemSubr.SUBR {
        USE_PACKAGE() { super("USE-PACKAGE",1,1,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object r0 = super.required(0, env);
            Object o0 = super.isOptional(0, env)
                ? super.optional(0, env)
                : Package.get(env);
            Obarray obarray = env.lisp().getObarray();
            if (Data.isList(r0)) {
                for (Object l = r0; !Lists.isEnd(l); l = Lists.cdr(l))
                    obarray.use(o0, Lists.car(l));
            }
            else {
                obarray.use(o0, r0);
            }
            return Symbols.T;
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class EXPORT extends SystemSubr.SUBR {
        EXPORT() { super("EXPORT",1,1,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object r0 = super.required(0, env);
            Object o0 = super.isOptional(0, env)
                ? super.optional(0, env)
                : Package.get(env);
            Obarray obarray = env.lisp().getObarray();
            if (Data.isList(r0)) {
                for (Object l = r0; !Lists.isEnd(l); l = Lists.cdr(l))
                    obarray.exp(o0, Data.symbol(Lists.car(l)));
            }
            else {
                obarray.exp(o0, Data.symbol(r0));
            }
            return Symbols.T;
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class IMPORT extends SystemSubr.SUBR {
        IMPORT() { super("IMPORT",1,1,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object r0 = super.required(0, env);
            Object o0 = super.isOptional(0, env)
                ? super.optional(0, env)
                : Package.get(env);
            Obarray obarray = env.lisp().getObarray();
            if (Data.isList(r0)) {
                for (Object l = r0; !Lists.isEnd(l); l = Lists.cdr(l))
                    obarray.imp(o0, Data.symbol(Lists.car(l)));
            }
            else {
                obarray.imp(o0, Data.symbol(r0));
            }
            return Symbols.T;
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class INTERN extends SystemSubr.SUBR {
        INTERN() { super("INTERN",1,1,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object r0 = super.required(0, env);
            Object o0 = super.isOptional(0, env)
                ? super.optional(0, env)
                : Package.get(env);
            return env.lisp().getObarray().intern(o0, Data.string(r0));
        }
    }
    static class FIND_SYMBOL extends SystemSubr.SUBR {
        FIND_SYMBOL() { super("FIND-SYMBOL",1,1,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object r0 = super.required(0, env);
            Object o0 = super.isOptional(0, env)
                ? super.optional(0, env)
                : Package.get(env);
            return env.lisp().getObarray()
                .find(o0, Data.string(r0), false);
        }
    }
    static class MAPATOMS extends SystemSubr.SUBR {
        MAPATOMS() { super("MAPATOMS",1,1,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object r0 = super.required(0, env);
            Object o0 = super.isOptional(0, env)
                ? super.optional(0, env)
                : Package.get(env);
            Function fun = Data.isFunction(r0)
                ? Data.function(r0) : Evaluator.function(r0, env);
            env.lisp().getObarray().mapc(o0, fun, env);
            return Symbols.NIL;
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class GET extends SystemSubr.SUBR2 {
        GET() { super("GET"); }
        public Object call2(Object r0, Object r1, Env env) {
            return env.lisp().getProp(r0, r1);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class GETL extends SystemSubr.SUBR2 {
        GETL() { super("GETL"); }
        public Object call2(Object r0, Object r1, Env env) {
            return env.lisp().getSubplist(r0, r1);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class PUTPROP extends SystemSubr.SUBR3 {
        PUTPROP() { super("PUTPROP"); }
        public Object call3(Object r0, Object r1, Object r2, Env env) {
            return env.lisp().setProp(r0, r2, r1);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class REMPROP extends SystemSubr.SUBR2 {
        REMPROP() { super("REMPROP"); }
        public Object call2(Object r0, Object r1, Env env) {
            return env.lisp().remProp(r0, r1);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class PLIST extends SystemSubr.SUBR1 {
        PLIST() { super("PLIST"); }
        public Object call1(Object r0, Env env) {
            return env.lisp().getPlist(r0);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class SETPLIST extends SystemSubr.SUBR2 {
        SETPLIST() { super("SETPLIST"); }
        public Object call2(Object r0, Object r1, Env env) {
            env.lisp().setPlist(r0, r1);
            return r1;
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    //static class GENSYM extends SystemSubr.SUBR0 {
    //    GENSYM() { super("GENSYM"); }
    //    public Object call0(Env env) {
    //        return env.lisp().gensym();
    //    }
    //}
    static class GENSYM extends SystemSubr.SUBR {
        GENSYM() { super("GENSYM",0,1,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object o1 = super.optional(0, env);
            if (o1 == Symbols.NIL)
                return env.lisp().getObarray().gensym();
            else
                return env.lisp().getObarray().gensym(Data.string(o1));
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class SET extends SystemSubr.SUBR2 {
        SET() { super("SET"); }
        public Object call2(Object r0, Object r1, Env env) {
            env.set(Data.symbol(r0), r1);
            return r1;
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class BOUNDP extends SystemSubr.SUBR1 {
        BOUNDP() { super("BOUNDP"); }
        public Object call1(Object r0, Env env) {
            return Data.toPredicate(env.isBound(Data.symbol(r0)));
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class VALUES extends SystemSubr.SUBR
        implements Callable0r {
        VALUES() { super("VALUES",0,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object rest = super.rest(env);
            return call0(rest, env);
        }
        public Object call0(Object rest, Env env) {
            /*
             * Note that this iteration will not work
             * if the MV is used in the body of following method:
             * Lists#isEnd, Lists#car, Lists#cdr.
             */
            Values mv = Values.currentValues();
            for (Object l = rest; !Lists.isEnd(l); l = Lists.cdr(l)) {
                mv.push(Lists.car(l));
            }
            return mv;
        }
    }

    /*
     * number functions
     */

    static class PLUS extends SystemSubr.SUBR
        implements Callable0, Callable1, Callable2, Callable0r {
        static final Callable0 c0 = (Callable0) SysSubrs._ZERO;
        static final Callable1 c1 = (Callable1) SysSubrs._IDENTITY_NUMBER;
        static final Callable2 c2 = (Callable2) SysSubrs._PLUS_BIN;
        PLUS() { super("PLUS",0,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object rest = super.rest(env);
            return call0(rest, env);
        }
        public Object call0(Object rest, Env env) {
            Object ret = Data.toFixnum(0);
            for (Object l = rest; !Lists.isEnd(l); l = Lists.cdr(l)) {
                ret = Numbers.add(ret, Lists.car(l));
            }
            return ret;
        }
        public Object call0(Env env)
            throws java.lang.Exception {
            return c0.call0(env);
        }
        public Object call1(Object arg0, Env env)
            throws java.lang.Exception {
            return c1.call1(arg0, env);
        }
        public Object call2(Object arg0, Object arg1, Env env)
            throws java.lang.Exception {
            return c2.call2(arg0, arg1, env);
        }
        public Method getTargetMethod(int nargs) {
            switch (nargs) {
            case 0:
                return ((SystemSubr) c0).getTargetMethod(nargs);
            case 1:
                return ((SystemSubr) c1).getTargetMethod(nargs);
            case 2:
                return ((SystemSubr) c2).getTargetMethod(nargs);
            default:
                return super.getTargetMethod(nargs);
            }
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class ADD_FIXNUM extends SystemSubr.SUBR
        implements Callable0, Callable1, Callable2, Callable0r {
        static final Callable0 c0 = (Callable0) SysSubrs._ZERO_FIXNUM;
        static final Callable1 c1 = (Callable1) SysSubrs._IDENTITY_FIXNUM;
        static final Callable2 c2 = (Callable2) SysSubrs._ADD_FIXNUM_BIN;
        ADD_FIXNUM() { super("+",0,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object rest = super.rest(env);
            return call0(rest, env);
        }
        public Object call0(Object rest, Env env) {
            int ret = 0;
            for (Object l = rest; !Lists.isEnd(l); l = Lists.cdr(l)) {
                ret = ret + Data.fixnum(Lists.car(l)).intValue();
            }
            return Data.toFixnum(ret);
        }
        public Object call0(Env env)
            throws java.lang.Exception {
            return c0.call0(env);
        }
        public Object call1(Object arg0, Env env)
            throws java.lang.Exception {
            return c1.call1(arg0, env);
        }
        public Object call2(Object arg0, Object arg1, Env env)
            throws java.lang.Exception {
            return c2.call2(arg0, arg1, env);
        }
        public Method getTargetMethod(int nargs) {
            switch (nargs) {
            case 0:
                return ((SystemSubr) c0).getTargetMethod(nargs);
            case 1:
                return ((SystemSubr) c1).getTargetMethod(nargs);
            case 2:
                return ((SystemSubr) c2).getTargetMethod(nargs);
            default:
                return super.getTargetMethod(nargs);
            }
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class ADD_FLONUM extends SystemSubr.SUBR
        implements Callable0, Callable1, Callable2, Callable0r {
        static final Callable0 c0 = (Callable0) SysSubrs._ZERO_FLONUM;
        static final Callable1 c1 = (Callable1) SysSubrs._IDENTITY_FLONUM;
        static final Callable2 c2 = (Callable2) SysSubrs._ADD_FLONUM_BIN;
        ADD_FLONUM() { super("+$",0,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object rest = super.rest(env);
            return call0(rest, env);
        }
        public Object call0(Object rest, Env env) {
            double ret = 0.0;
            for (Object l = rest; !Lists.isEnd(l); l = Lists.cdr(l)) {
                ret = ret + Data.flonum(Lists.car(l)).doubleValue();
            }
            return Data.toFlonum(ret);
        }
        public Object call0(Env env)
            throws java.lang.Exception {
            return c0.call0(env);
        }
        public Object call1(Object arg0, Env env)
            throws java.lang.Exception {
            return c1.call1(arg0, env);
        }
        public Object call2(Object arg0, Object arg1, Env env)
            throws java.lang.Exception {
            return c2.call2(arg0, arg1, env);
        }
        public Method getTargetMethod(int nargs) {
            switch (nargs) {
            case 0:
                return ((SystemSubr) c0).getTargetMethod(nargs);
            case 1:
                return ((SystemSubr) c1).getTargetMethod(nargs);
            case 2:
                return ((SystemSubr) c2).getTargetMethod(nargs);
            default:
                return super.getTargetMethod(nargs);
            }
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class DIFFERENCE extends SystemSubr.SUBR
        implements Callable0, Callable1, Callable2, Callable0r {
        static final Callable0 c0 = (Callable0) SysSubrs._ZERO;
        static final Callable1 c1 = (Callable1) SysSubrs._IDENTITY_NUMBER;
        static final Callable2 c2 = (Callable2) SysSubrs._DIFFERENCE_BIN;
        DIFFERENCE() { super("DIFFERENCE",0,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object rest = super.rest(env);
            return call0(rest, env);
        }
        public Object call0(Object rest, Env env) {
            if (Lists.isEnd(rest)) {
                return Data.toFixnum(0);
            }
            Object ret = Data.javaNumber(Lists.car(rest));
            rest = Lists.cdr(rest);
            for (; !Lists.isEnd(rest); rest = Lists.cdr(rest)) {
                ret = Numbers.sub(ret, Lists.car(rest));
            }
            return ret;
        }
        public Object call0(Env env)
            throws java.lang.Exception {
            return c0.call0(env);
        }
        public Object call1(Object arg0, Env env)
            throws java.lang.Exception {
            return c1.call1(arg0, env);
        }
        public Object call2(Object arg0, Object arg1, Env env)
            throws java.lang.Exception {
            return c2.call2(arg0, arg1, env);
        }
        public Method getTargetMethod(int nargs) {
            switch (nargs) {
            case 0:
                return ((SystemSubr) c0).getTargetMethod(nargs);
            case 1:
                return ((SystemSubr) c1).getTargetMethod(nargs);
            case 2:
                return ((SystemSubr) c2).getTargetMethod(nargs);
            default:
                return super.getTargetMethod(nargs);
            }
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class SUB_FIXNUM extends SystemSubr.SUBR
        implements Callable0, Callable1, Callable2, Callable0r {
        static final Callable0 c0 = (Callable0) SysSubrs._ZERO_FIXNUM;
        static final Callable1 c1 = (Callable1) SysSubrs._NEGATE_FIXNUM;
        static final Callable2 c2 = (Callable2) SysSubrs._SUB_FIXNUM_BIN;
        SUB_FIXNUM() { super("-",0,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object rest = super.rest(env);
            return call0(rest, env);
        }
        public Object call0(Object rest, Env env) {
            if (Lists.isEnd(rest)) {
                return Data.toFixnum(0);
            }
            int ret = Data.fixnum(Lists.car(rest)).intValue();
            rest = Lists.cdr(rest);
            if (Lists.isEnd(rest)) {
                return Data.toFixnum(-ret);
            }
            for (; !Lists.isEnd(rest); rest = Lists.cdr(rest)) {
                ret = ret - Data.fixnum(Lists.car(rest)).intValue();
            }
            return Data.toFixnum(ret);
        }
        public Object call0(Env env)
            throws java.lang.Exception {
            return c0.call0(env);
        }
        public Object call1(Object arg0, Env env)
            throws java.lang.Exception {
            return c1.call1(arg0, env);
        }
        public Object call2(Object arg0, Object arg1, Env env)
            throws java.lang.Exception {
            return c2.call2(arg0, arg1, env);
        }
        public Method getTargetMethod(int nargs) {
            switch (nargs) {
            case 0:
                return ((SystemSubr) c0).getTargetMethod(nargs);
            case 1:
                return ((SystemSubr) c1).getTargetMethod(nargs);
            case 2:
                return ((SystemSubr) c2).getTargetMethod(nargs);
            default:
                return super.getTargetMethod(nargs);
            }
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class SUB_FLONUM extends SystemSubr.SUBR
        implements Callable0, Callable1, Callable2, Callable0r {
        static final Callable0 c0 = (Callable0) SysSubrs._ZERO_FLONUM;
        static final Callable1 c1 = (Callable1) SysSubrs._NEGATE_FLONUM;
        static final Callable2 c2 = (Callable2) SysSubrs._SUB_FLONUM_BIN;
        SUB_FLONUM() { super("-$",0,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object rest = super.rest(env);
            return call0(rest, env);
        }
        public Object call0(Object rest, Env env) {
            if (Lists.isEnd(rest)) {
                return Data.toFlonum(0.0);
            }
            double ret = Data.flonum(Lists.car(rest)).doubleValue();
            rest = Lists.cdr(rest);
            if (Lists.isEnd(rest)) {
                return Data.toFlonum(-ret);
            }
            for (; !Lists.isEnd(rest); rest = Lists.cdr(rest)) {
                ret = ret - Data.flonum(Lists.car(rest)).doubleValue();
            }
            return Data.toFlonum(ret);
        }
        public Object call0(Env env)
            throws java.lang.Exception {
            return c0.call0(env);
        }
        public Object call1(Object arg0, Env env)
            throws java.lang.Exception {
            return c1.call1(arg0, env);
        }
        public Object call2(Object arg0, Object arg1, Env env)
            throws java.lang.Exception {
            return c2.call2(arg0, arg1, env);
        }
        public Method getTargetMethod(int nargs) {
            switch (nargs) {
            case 0:
                return ((SystemSubr) c0).getTargetMethod(nargs);
            case 1:
                return ((SystemSubr) c1).getTargetMethod(nargs);
            case 2:
                return ((SystemSubr) c2).getTargetMethod(nargs);
            default:
                return super.getTargetMethod(nargs);
            }
        }
        public boolean canProduceValues() {
            return false;
        }
    }
    static class TIMES extends SystemSubr.SUBR
        implements Callable0, Callable1, Callable2, Callable0r {
        static final Callable0 c0 = (Callable0) SysSubrs._ONE;
        static final Callable1 c1 = (Callable1) SysSubrs._IDENTITY_NUMBER;
        static final Callable2 c2 = (Callable2) SysSubrs._TIMES_BIN;
        TIMES() { super("TIMES",0,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object rest = super.rest(env);
            return call0(rest, env);
        }
        public Object call0(Object rest, Env env) {
            Object ret = Data.toFixnum(1);
            for (Object l = rest; !Lists.isEnd(l); l = Lists.cdr(l)) {
                ret = Numbers.mul(ret, Lists.car(l));
            }
            return ret;
        }
        public Object call0(Env env)
            throws java.lang.Exception {
            return c0.call0(env);
        }
        public Object call1(Object arg0, Env env)
            throws java.lang.Exception {
            return c1.call1(arg0, env);
        }
        public Object call2(Object arg0, Object arg1, Env env)
            throws java.lang.Exception {
            return c2.call2(arg0, arg1, env);
        }
        public Method getTargetMethod(int nargs) {
            switch (nargs) {
            case 0:
                return ((SystemSubr) c0).getTargetMethod(nargs);
            case 1:
                return ((SystemSubr) c1).getTargetMethod(nargs);
            case 2:
                return ((SystemSubr) c2).getTargetMethod(nargs);
            default:
                return super.getTargetMethod(nargs);
            }
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class MUL_FIXNUM extends SystemSubr.SUBR
        implements Callable0, Callable1, Callable2, Callable0r {
        static final Callable0 c0 = (Callable0) SysSubrs._ONE_FIXNUM;
        static final Callable1 c1 = (Callable1) SysSubrs._IDENTITY_FIXNUM;
        static final Callable2 c2 = (Callable2) SysSubrs._MUL_FIXNUM_BIN;
        MUL_FIXNUM() { super("*",0,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object rest = super.rest(env);
            return call0(rest, env);
        }
        public Object call0(Object rest, Env env) {
            int ret = 1;
            for (Object l = rest; !Lists.isEnd(l); l = Lists.cdr(l)) {
                ret = ret * Data.fixnum(Lists.car(l)).intValue();
            }
            return Data.toFixnum(ret);
        }
        public Object call0(Env env)
            throws java.lang.Exception {
            return c0.call0(env);
        }
        public Object call1(Object arg0, Env env)
            throws java.lang.Exception {
            return c1.call1(arg0, env);
        }
        public Object call2(Object arg0, Object arg1, Env env)
            throws java.lang.Exception {
            return c2.call2(arg0, arg1, env);
        }
        public Method getTargetMethod(int nargs) {
            switch (nargs) {
            case 0:
                return ((SystemSubr) c0).getTargetMethod(nargs);
            case 1:
                return ((SystemSubr) c1).getTargetMethod(nargs);
            case 2:
                return ((SystemSubr) c2).getTargetMethod(nargs);
            default:
                return super.getTargetMethod(nargs);
            }
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class MUL_FLONUM extends SystemSubr.SUBR
        implements Callable0, Callable1, Callable2, Callable0r {
        static final Callable0 c0 = (Callable0) SysSubrs._ONE_FLONUM;
        static final Callable1 c1 = (Callable1) SysSubrs._IDENTITY_FLONUM;
        static final Callable2 c2 = (Callable2) SysSubrs._MUL_FLONUM_BIN;
        MUL_FLONUM() { super("*$",0,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object rest = super.rest(env);
            return call0(rest, env);
        }
        public Object call0(Object rest, Env env) {
            double ret = 1.0;
            for (Object l = rest; !Lists.isEnd(l); l = Lists.cdr(l)) {
                ret = ret * Data.flonum(Lists.car(l)).doubleValue();
            }
            return Data.toFlonum(ret);
        }
        public Object call0(Env env)
            throws java.lang.Exception {
            return c0.call0(env);
        }
        public Object call1(Object arg0, Env env)
            throws java.lang.Exception {
            return c1.call1(arg0, env);
        }
        public Object call2(Object arg0, Object arg1, Env env)
            throws java.lang.Exception {
            return c2.call2(arg0, arg1, env);
        }
        public Method getTargetMethod(int nargs) {
            switch (nargs) {
            case 0:
                return ((SystemSubr) c0).getTargetMethod(nargs);
            case 1:
                return ((SystemSubr) c1).getTargetMethod(nargs);
            case 2:
                return ((SystemSubr) c2).getTargetMethod(nargs);
            default:
                return super.getTargetMethod(nargs);
            }
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class QUOTIENT extends SystemSubr.SUBR
        implements Callable0, Callable1, Callable2, Callable0r {
        static final Callable0 c0 = (Callable0) SysSubrs._ONE;
        static final Callable1 c1 = (Callable1) SysSubrs._IDENTITY_NUMBER;
        static final Callable2 c2 = (Callable2) SysSubrs._QUOTIENT_BIN;
        QUOTIENT() { super("QUOTIENT",0,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object rest = super.rest(env);
            return call0(rest, env);
        }
        public Object call0(Object rest, Env env) {
            if (Lists.isEnd(rest)) {
                return Data.toFixnum(1);
            }
            Object ret = Data.javaNumber(Lists.car(rest));
            rest = Lists.cdr(rest);
            for (; !Lists.isEnd(rest); rest = Lists.cdr(rest)) {
                ret = Numbers.div(ret, Lists.car(rest));
            }
            return ret;
        }
        public Object call0(Env env)
            throws java.lang.Exception {
            return c0.call0(env);
        }
        public Object call1(Object arg0, Env env)
            throws java.lang.Exception {
            return c1.call1(arg0, env);
        }
        public Object call2(Object arg0, Object arg1, Env env)
            throws java.lang.Exception {
            return c2.call2(arg0, arg1, env);
        }
        public Method getTargetMethod(int nargs) {
            switch (nargs) {
            case 0:
                return ((SystemSubr) c0).getTargetMethod(nargs);
            case 1:
                return ((SystemSubr) c1).getTargetMethod(nargs);
            case 2:
                return ((SystemSubr) c2).getTargetMethod(nargs);
            default:
                return super.getTargetMethod(nargs);
            }
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class DIV_FIXNUM extends SystemSubr.SUBR
        implements Callable0, Callable1, Callable2, Callable0r {
        static final Callable0 c0 = (Callable0) SysSubrs._ONE_FIXNUM;
        static final Callable1 c1 = (Callable1) SysSubrs._IDENTITY_FIXNUM;
        static final Callable2 c2 = (Callable2) SysSubrs._DIV_FIXNUM_BIN;
        DIV_FIXNUM() { super("//",0,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object rest = super.rest(env);
            return call0(rest, env);
        }
        public Object call0(Object rest, Env env) {
            if (Lists.isEnd(rest)) {
                return Data.toFixnum(1);
            }
            int ret = Data.fixnum(Lists.car(rest)).intValue();
            rest = Lists.cdr(rest);
            for (; !Lists.isEnd(rest); rest = Lists.cdr(rest)) {
                ret = ret / Data.fixnum(Lists.car(rest)).intValue();
            }
            return Data.toFixnum(ret);
        }
        public Object call0(Env env)
            throws java.lang.Exception {
            return c0.call0(env);
        }
        public Object call1(Object arg0, Env env)
            throws java.lang.Exception {
            return c1.call1(arg0, env);
        }
        public Object call2(Object arg0, Object arg1, Env env)
            throws java.lang.Exception {
            return c2.call2(arg0, arg1, env);
        }
        public Method getTargetMethod(int nargs) {
            switch (nargs) {
            case 0:
                return ((SystemSubr) c0).getTargetMethod(nargs);
            case 1:
                return ((SystemSubr) c1).getTargetMethod(nargs);
            case 2:
                return ((SystemSubr) c2).getTargetMethod(nargs);
            default:
                return super.getTargetMethod(nargs);
            }
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class DIV_FLONUM extends SystemSubr.SUBR
        implements Callable0, Callable1, Callable2, Callable0r {
        static final Callable0 c0 = (Callable0) SysSubrs._ONE_FLONUM;
        static final Callable1 c1 = (Callable1) SysSubrs._INVERSE_FLONUM;
        static final Callable2 c2 = (Callable2) SysSubrs._DIV_FLONUM_BIN;
        DIV_FLONUM() { super("//$",0,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object rest = super.rest(env);
            return call0(rest, env);
        }
        public Object call0(Object rest, Env env) {
            if (Lists.isEnd(rest)) {
                return Data.toFlonum(1);
            }
            double ret = Data.flonum(Lists.car(rest)).doubleValue();
            rest = Lists.cdr(rest);
            if (Lists.isEnd(rest)) {
                return Data.toFlonum(1 / ret);
            }
            for (; !Lists.isEnd(rest); rest = Lists.cdr(rest)) {
                ret = ret / Data.flonum(Lists.car(rest)).doubleValue();
            }
            return Data.toFlonum(ret);
        }
        public Object call0(Env env)
            throws java.lang.Exception {
            return c0.call0(env);
        }
        public Object call1(Object arg0, Env env)
            throws java.lang.Exception {
            return c1.call1(arg0, env);
        }
        public Object call2(Object arg0, Object arg1, Env env)
            throws java.lang.Exception {
            return c2.call2(arg0, arg1, env);
        }
        public Method getTargetMethod(int nargs) {
            switch (nargs) {
            case 0:
                return ((SystemSubr) c0).getTargetMethod(nargs);
            case 1:
                return ((SystemSubr) c1).getTargetMethod(nargs);
            case 2:
                return ((SystemSubr) c2).getTargetMethod(nargs);
            default:
                return super.getTargetMethod(nargs);
            }
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class EQUALP extends SystemSubr.SUBR
        implements Callable2, Callable2r {
        static final Callable2 c2 = (Callable2) SysSubrs._EQUALP_BIN;
        EQUALP() { super("EQUALP",2,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object val1 = super.required(0,env);
            Object val2 = super.required(1,env);
            Object rest = super.rest(env);
            return call2(val1, val2, rest, env);
        }
        public Object call2(Object val1, Object val2, Object rest, Env env) {
            if (!Numbers.isEq(val1, val2)) {
                return Symbols.NIL;
            }
            val1 = val2;
            for (Object l = rest; !Lists.isEnd(l); l = Lists.cdr(l)) {
                val2 = Lists.car(l);
                if (!Numbers.isEq(val1, val2)) {
                    return Symbols.NIL;
                }
                val1 = val2;
            }
            return Symbols.T;
        }
        public Object call2(Object arg0, Object arg1, Env env)
            throws java.lang.Exception {
            return c2.call2(arg0, arg1, env);
        }
        public Method getTargetMethod(int nargs) {
            switch (nargs) {
            case 2:
                return ((SystemSubr) c2).getTargetMethod(nargs);
            default:
                return super.getTargetMethod(nargs);
            }
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class GREATERP extends SystemSubr.SUBR
        implements Callable2, Callable2r {
        static final Callable2 c2 = (Callable2) SysSubrs._GREATERP_BIN;
        GREATERP() { super("GREATERP",2,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object val1 = super.required(0,env);
            Object val2 = super.required(1,env);
            Object rest = super.rest(env);
            return call2(val1, val2, rest, env);
        }
        public Object call2(Object val1, Object val2, Object rest, Env env) {
            if (!Numbers.isGt(val1, val2)) {
                return Symbols.NIL;
            }
            val1 = val2;
            for (Object l = rest; !Lists.isEnd(l); l = Lists.cdr(l)) {
                val2 = Lists.car(l);
                if (!Numbers.isGt(val1, val2)) {
                    return Symbols.NIL;
                }
                val1 = val2;
            }
            return Symbols.T;
        }
        public Object call2(Object arg0, Object arg1, Env env)
            throws java.lang.Exception {
            return c2.call2(arg0, arg1, env);
        }
        public Method getTargetMethod(int nargs) {
            switch (nargs) {
            case 2:
                return ((SystemSubr) c2).getTargetMethod(nargs);
            default:
                return super.getTargetMethod(nargs);
            }
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class LESSP extends SystemSubr.SUBR
        implements Callable2, Callable2r {
        static final Callable2 c2 = (Callable2) SysSubrs._LESSP_BIN;
        LESSP() { super("LESSP",2,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object val1 = super.required(0,env);
            Object val2 = super.required(1,env);
            Object rest = super.rest(env);
            return call2(val1, val2, rest, env);
        }
        public Object call2(Object val1, Object val2, Object rest, Env env) {
            if (!Numbers.isLt(val1, val2)) {
                return Symbols.NIL;
            }
            val1 = val2;
            for (Object l = rest; !Lists.isEnd(l); l = Lists.cdr(l)) {
                val2 = Lists.car(l);
                if (!Numbers.isLt(val1, val2)) {
                    return Symbols.NIL;
                }
                val1 = val2;
            }
            return Symbols.T;
        }
        public Object call2(Object arg0, Object arg1, Env env)
            throws java.lang.Exception {
            return c2.call2(arg0, arg1, env);
        }
        public Method getTargetMethod(int nargs) {
            switch (nargs) {
            case 2:
                return ((SystemSubr) c2).getTargetMethod(nargs);
            default:
                return super.getTargetMethod(nargs);
            }
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class GREATER_OR_EQUALP extends SystemSubr.SUBR
        implements Callable2, Callable2r {
        static final Callable2 c2 = (Callable2) SysSubrs._GREATER_OR_EQUALP_BIN;
        GREATER_OR_EQUALP() {
            super("GREATER-OR-EQUALP",2,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object val1 = super.required(0,env);
            Object val2 = super.required(1,env);
            Object rest = super.rest(env);
            return call2(val1, val2, rest, env);
        }
        public Object call2(Object val1, Object val2, Object rest, Env env) {
            if (!Numbers.isGe(val1, val2)) {
                return Symbols.NIL;
            }
            val1 = val2;
            for (Object l = rest; !Lists.isEnd(l); l = Lists.cdr(l)) {
                val2 = Lists.car(l);
                if (!Numbers.isGe(val1, val2)) {
                    return Symbols.NIL;
                }
                val1 = val2;
            }
            return Symbols.T;
        }
        public Object call2(Object arg0, Object arg1, Env env)
            throws java.lang.Exception {
            return c2.call2(arg0, arg1, env);
        }
        public Method getTargetMethod(int nargs) {
            switch (nargs) {
            case 2:
                return ((SystemSubr) c2).getTargetMethod(nargs);
            default:
                return super.getTargetMethod(nargs);
            }
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class LESS_OR_EQUALP extends SystemSubr.SUBR
        implements Callable2, Callable2r {
        static final Callable2 c2 = (Callable2) SysSubrs._LESS_OR_EQUALP_BIN;
        LESS_OR_EQUALP() {
            super("LESS-OR-EQUALP",2,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object val1 = super.required(0,env);
            Object val2 = super.required(1,env);
            Object rest = super.rest(env);
            return call2(val1, val2, rest, env);
        }
        public Object call2(Object val1, Object val2, Object rest, Env env) {
            if (!Numbers.isLe(val1, val2)) {
                return Symbols.NIL;
            }
            val1 = val2;
            for (Object l = rest; !Lists.isEnd(l); l = Lists.cdr(l)) {
                val2 = Lists.car(l);
                if (!Numbers.isLe(val1, val2)) {
                    return Symbols.NIL;
                }
                val1 = val2;
            }
            return Symbols.T;
        }
        public Object call2(Object arg0, Object arg1, Env env)
            throws java.lang.Exception {
            return c2.call2(arg0, arg1, env);
        }
        public Method getTargetMethod(int nargs) {
            switch (nargs) {
            case 2:
                return ((SystemSubr) c2).getTargetMethod(nargs);
            default:
                return super.getTargetMethod(nargs);
            }
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class MAX extends SystemSubr.SUBR
        implements Callable2, Callable2r {
        static final Callable2 c2 = (Callable2) SysSubrs._MAX_BIN;
        MAX() { super("MAX",2,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object r0 = super.required(0,env);
            Object r1 = super.required(1,env);
            Object rest = super.rest(env);
            return call2(r0, r1, rest, env);
        }
        public Object call2(Object r0, Object r1, Object rest, Env env) {
            Object ret = Numbers.max(r0, r1);
            for (Object l = rest; !Lists.isEnd(l); l = Lists.cdr(l)) {
                ret = Numbers.max(ret, Lists.car(l));
            }
            return ret;
        }
        public Object call2(Object arg0, Object arg1, Env env)
            throws java.lang.Exception {
            return c2.call2(arg0, arg1, env);
        }
        public Method getTargetMethod(int nargs) {
            switch (nargs) {
            case 2:
                return ((SystemSubr) c2).getTargetMethod(nargs);
            default:
                return super.getTargetMethod(nargs);
            }
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class MIN extends SystemSubr.SUBR
        implements Callable2, Callable2r {
        static final Callable2 c2 = (Callable2) SysSubrs._MIN_BIN;
        MIN() { super("MIN",2,0,true,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object r0 = super.required(0,env);
            Object r1 = super.required(1,env);
            Object rest = super.rest(env);
            return call2(r0, r1, rest, env);
        }
        public Object call2(Object r0, Object r1, Object rest, Env env) {
            Object ret = Numbers.min(r0, r1);
            for (Object l = rest; !Lists.isEnd(l); l = Lists.cdr(l)) {
                ret = Numbers.min(ret, Lists.car(l));
            }
            return ret;
        }
        public Object call2(Object arg0, Object arg1, Env env)
            throws java.lang.Exception {
            return c2.call2(arg0, arg1, env);
        }
        public Method getTargetMethod(int nargs) {
            switch (nargs) {
            case 2:
                return ((SystemSubr) c2).getTargetMethod(nargs);
            default:
                return super.getTargetMethod(nargs);
            }
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    //static class ATAN extends SystemSubr.SUBR1 {
    //    ATAN() { super("ATAN"); }
    //    public Object call1(Object r0, Env env) {
    //        return Numbers.atan(r0);
    //    }
    //}
    //static class ATAN2 extends SystemSubr.SUBR2 {
    //    ATAN2() { super("ATAN2"); }
    //    public Object call1(Object r0, Object r1, Env env) {
    //        return Numbers.atan2(r0, r1);
    //    }
    //}
    static class ATAN extends SystemSubr.SUBR {
        ATAN() { super("ATAN",1,1,false,Symbols.NIL,false); }
        protected Object doCall(Env env) {
            Object r0 = super.required(0,env);
            if (super.isOptional(0,env)) {
                //System.out.println("atan2 called");
                Object o0 = super.optional(0,env);
                return Data.toFlonum(Numbers.atan2(r0, o0));
            }
            else {
                //System.out.println("atan called");
                return Data.toFlonum(Numbers.atan(r0));
            }
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }

    /*
     * array functions
     */

    static class MAKE_ARRAY extends SystemSubr.SUBR
        implements Callable2r {
        MAKE_ARRAY() { super("*ARRAY",2,0,true,Symbols.NIL,false); }
        public Object doCall(Env env) {
            Object r0 = super.required(0,env);
            Object r1 = super.required(1,env);
            Object rest = super.rest(env);
            return call2(r0, r1, rest, env);
        }
        public Object call2(Object r0, Object r1, Object rest, Env env) {
            Symbol name = Data.symbol(r0);
            Symbol type = Data.symbol(r1);
            return _make(name, type, rest, env);
        }
        private Object _make
            (Symbol name, Symbol type, Object dims, Env env) {
            Object a;
            int d0,d1,d2,d3,d4;
            int len = Lists.length(dims);
            switch (len) {
            case 1:
                d0 = Data.fixnum(Lists.car(dims)).intValue();
                a = Arrays.make(name, type, d0);
                break;
            case 2:
                d0 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d1 = Data.fixnum(Lists.car(dims)).intValue();
                a = Arrays.make(name, type, d0, d1);
                break;
            case 3:
                d0 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d1 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d2 = Data.fixnum(Lists.car(dims)).intValue();
                a = Arrays.make(name, type, d0, d1, d2);
                break;
            case 4:
                d0 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d1 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d2 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d3 = Data.fixnum(Lists.car(dims)).intValue();
                a = Arrays.make(name, type, d0, d1, d2, d3);
                break;
            case 5:
                d0 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d1 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d2 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d3 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d4 = Data.fixnum(Lists.car(dims)).intValue();
                a = Arrays.make(name, type, d0, d1, d2, d3, d4);
                break;
            default:
                throw new ProgramException
                    ("illegal dims.length: "+len, Symbols.NIL);
            }
            if (name == Symbols.NIL)
                return a;
            else {
                env.lisp().setProp(name, Symbols.ARRAY, a);
                return name;
            }
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class ARRAY_GET extends SystemSubr.SUBR
        implements Callable1r {
        ARRAY_GET() { super("*ARRAY-GET",1,0,true,Symbols.NIL,false); }
        public Object doCall(Env env) {
            Object r0 = super.required(0,env);
            Object rest = super.rest(env);
            return call1(r0, rest, env);
        }
        public Object call1(Object r0, Object rest, Env env) {
            return _get(r0, rest, env);
        }
        private Object _get(Object a, Object dims, Env env) {
            Object ret;
            int d0,d1,d2,d3,d4;
            int len = Lists.length(dims);
            switch (len) {
            case 1:
                d0 = Data.fixnum(Lists.car(dims)).intValue();
                ret = Arrays.get(a, d0);
                break;
            case 2:
                d0 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d1 = Data.fixnum(Lists.car(dims)).intValue();
                ret = Arrays.get(a, d0, d1);
                break;
            case 3:
                d0 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d1 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d2 = Data.fixnum(Lists.car(dims)).intValue();
                ret = Arrays.get(a, d0, d1, d2);
                break;
            case 4:
                d0 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d1 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d2 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d3 = Data.fixnum(Lists.car(dims)).intValue();
                ret = Arrays.get(a, d0, d1, d2, d3);
                break;
            case 5:
                d0 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d1 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d2 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d3 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d4 = Data.fixnum(Lists.car(dims)).intValue();
                ret = Arrays.get(a, d0, d1, d2, d3, d4);
                break;
            default:
                throw new ProgramException
                    ("illegal dims.length: "+len, Symbols.NIL);
            }
            return ret;
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class ARRAY_SET extends SystemSubr.SUBR
        implements Callable2r {
        ARRAY_SET() { super("*ARRAY-SET",2,0,true,Symbols.NIL,false); }
        public Object doCall(Env env) {
            Object r0 = super.required(0,env);
            Object r1 = super.required(1,env);
            Object rest = super.rest(env);
            return call2(r0, r1, rest, env);
        }
        public Object call2(Object r0, Object r1, Object rest, Env env) {
            return _set(r0, r1, rest, env);
        }
        private Object _set(Object a, Object o, Object dims, Env env) {
            int d0,d1,d2,d3,d4;
            int len = Lists.length(dims);
            switch (len) {
            case 1:
                d0 = Data.fixnum(Lists.car(dims)).intValue();
                Arrays.set(a, o, d0);
                break;
            case 2:
                d0 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d1 = Data.fixnum(Lists.car(dims)).intValue();
                Arrays.set(a, o, d0, d1);
                break;
            case 3:
                d0 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d1 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d2 = Data.fixnum(Lists.car(dims)).intValue();
                Arrays.set(a, o, d0, d1, d2);
                break;
            case 4:
                d0 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d1 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d2 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d3 = Data.fixnum(Lists.car(dims)).intValue();
                Arrays.set(a, o, d0, d1, d2, d3);
                break;
            case 5:
                d0 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d1 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d2 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d3 = Data.fixnum(Lists.car(dims)).intValue();
                dims = Lists.cdr(dims);
                d4 = Data.fixnum(Lists.car(dims)).intValue();
                Arrays.set(a, o, d0, d1, d2, d3, d4);
                break;
            default:
                throw new ProgramException
                    ("illegal dims.length: "+len, Symbols.NIL);
            }
            return o;
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }

    /*
     * compile/load functions
     */

    static class COMPILE extends SystemSubr.SUBR {
        static final Object keys
            = Lists.list(Symbols.KW_DRYRUN,
                         Symbols.KW_PRINT,
                         Symbols.KW_OPTIMIZE);
        COMPILE() { super("COMPILE",1,1,false,keys,false); }
        public Object doCall(Env env) {
            Object r0 = super.required(0,env);
            Object o0 = super.optional(0,env);
            Object k0 = super.keyword(0,env);
            Object k1 = super.keyword(1,env);
            Object k2 = super.isKeyword(2,env)
                ? super.keyword(2, env)
                : Symbols.T;
            Symbol sym = Data.symbol(r0);
            Expr expr = (o0 == Symbols.NIL)
                ? Data.expr(env.lisp().getProp(sym, Symbols.EXPR))
                : Data.expr(o0);
            return Compiler.compile(sym, expr, k0, k1, k2, env);
        }
    }
    static class COMPILE_FILE extends SystemSubr.SUBR {
        static final Object keys
            = Lists.toList(new Object[] {
                    Symbols.KW_OUTPUT_FILE,
                    Symbols.KW_INPUT_FILE_ENCODING,
                    Symbols.KW_OUTPUT_FILE_ENCODING,
                    Symbols.KW_GENERATE_CLASS_FILE,
                    Symbols.KW_PRINT,
                    Symbols.KW_OPTIMIZE,
                });
        COMPILE_FILE() { super("COMPILE-FILE",1,0,false,keys,false); }
        public Object doCall(Env env) {
            Object r0 = super.required(0, env);
            Object k0 = super.keyword(0, env);
            Object k1 = super.keyword(1, env);
            Object k2 = super.keyword(2, env);
            Object k3 = super.keyword(3, env);
            Object k4 = super.keyword(4, env);
            Object k5 = super.isKeyword(5, env)
                ? super.keyword(5, env)
                : Symbols.T;
            String in = Data.string(r0);
            String out;
            if (k0 == Symbols.NIL)
                out = null;
            else
                out = Data.string(k0);
            return Compiler.compileFile
                (in, out, k1, k2, k3, k4, k5, env);
        }
    }
    static class LOAD extends SystemSubr.SUBR {
        static final Object keys
            = Lists.list(Symbols.KW_INPUT_FILE_ENCODING);
        LOAD() { super("LOAD",1,0,false,keys,false); }
        public Object doCall(Env env) {
            Object r0 = super.required(0, env);
            Object k0 = super.keyword(0, env);
            String in = Data.string(r0);
            return Loader.loadFile(in, k0, env);
        }
    }

    /*
     * functions appeared in archaic scheme / rabbit compiler.
     */

    static class ALARMCLOCK extends SystemSubr.SUBR2 {
        ALARMCLOCK() { super("ALARMCLOCK"); }
        public Object call2(Object r0, Object r1, Env env) {
            Symbol timername = Data.symbol(r0);
            Integer q = Data.fixnum(r1);
            env.lisp().getAlarmclock().schedule(timername, q, env);
            return Symbols.T;
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class SYMEVAL extends SystemSubr.SUBR1 {
        SYMEVAL() { super("SYMEVAL"); }
        public Object call1(Object r0, Env env) {
            return env.get(Data.symbol(r0));
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class EXPLODEN extends SystemSubr.SUBR1 {
        EXPLODEN() { super("EXPLODEN"); }
        public Object call1(Object r0, Env env) {
            String s = Printer.princToString(r0, env);
            ListBuilder lb = new ListBuilder();
            for (int i = 0; i < s.length(); i++)
                lb.append(Data.toFixnum((int) s.charAt(i)));
            return lb.toList();
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }
    static class IMPLODE extends SystemSubr.SUBR1 {
        IMPLODE() { super("IMPLODE"); }
        public Object call1(Object r0, Env env) {
            StringBuffer buf = new StringBuffer();
            for (Object l = r0; !Lists.isEnd(l); l = Lists.cdr(l)) {
                Object obj = Lists.car(l);
                if (Data.isSymbol(obj)) {
                    buf.append(Data.symbol(obj).pname());
                }
                else if (Data.isCharacter(obj)) {
                    buf.append(Data.character(obj).charValue());
                }
                else {
                    buf.append((char) Data.fixnum(obj).intValue());
                }
            }
            Object pkg = Package.get(env);
            return env.lisp().getObarray().intern(pkg, buf.toString()).nth(0);
        }
        public boolean canProduceValues(int nargs) {
            return false;
        }
    }

    private LispSubrs() {}
}
