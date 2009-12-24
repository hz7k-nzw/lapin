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

/** Predefined symbols and some operations on those symbols. */
public final class Symbols {
    /**
     * Selfevaluating and interned symbol
     * which represents the boolean value FALSE and the empty list.
     * @see Nil
     */
    static public final Symbol NIL
        = Package.LISP.intern("NIL");
    /**
     * Selfevaluating and interned symbol
     * which represents the boolean value TRUE.
     * @see T
     */
    static public final Symbol T
        = Package.LISP.intern("T");

    /** Interned symbol whose pname is "ALARMCLOCK". */
    static public final Symbol ALARMCLOCK
        = Package.LISP.intern("ALARMCLOCK");
    /** Interned symbol whose pname is "AND". */
    static public final Symbol AND
        = Package.LISP.intern("AND");
    /** Interned symbol whose pname is "APPEND". */
    static public final Symbol APPEND
        = Package.LISP.intern("APPEND");
    /** Interned symbol whose pname is "ARRAY". */
    static public final Symbol ARRAY
        = Package.LISP.intern("ARRAY");
    /** Interned symbol whose pname is "BACKQUOTE". */
    static public final Symbol BACKQUOTE
        = Package.LISP.intern("BACKQUOTE");
    /** Interned symbol whose pname is "BIGNUM". */
    static public final Symbol BIGNUM
        = Package.LISP.intern("BIGNUM");
    /** Interned symbol whose pname is "CAR". */
    static public final Symbol CAR
        = Package.LISP.intern("CAR");
    /** Interned symbol whose pname is "CDR". */
    static public final Symbol CDR
        = Package.LISP.intern("CDR");
    /** Interned symbol whose pname is "CHARACTER". */
    static public final Symbol CHARACTER
        = Package.LISP.intern("CHARACTER");
    /** Interned symbol whose pname is "CMACRO". */
    static public final Symbol CMACRO
        = Package.LISP.intern("CMACRO");
    /** Interned symbol whose pname is "COMMENT". */
    static public final Symbol COMMENT
        = Package.LISP.intern("COMMENT");
    /** Interned symbol whose pname is "COMPILE". */
    static public final Symbol COMPILE
        = Package.LISP.intern("COMPILE");
    /** Interned symbol whose pname is "CONS". */
    static public final Symbol CONS
        = Package.LISP.intern("CONS");
    /** Interned symbol whose pname is "CONSTANT". */
    static public final Symbol CONSTANT
        = Package.LISP.intern("CONSTANT");
//    /** Interned symbol whose pname is "CURRENT-EXCEPTION". */
//    static public final Symbol CURRENT_EXCEPTION
//        = Package.LISP.intern("CURRENT-EXCEPTION");
//    /** Interned symbol whose pname is "CURRENT-EVAL-FORM". */
//    static public final Symbol CURRENT_EVAL_FORM
//        = Package.LISP.intern("CURRENT-EVAL-FORM");
    /** Interned symbol whose pname is "DECLARE". */
    static public final Symbol DECLARE
        = Package.LISP.intern("DECLARE");
    /** Interned symbol whose pname is "DEFCONST". */
    static public final Symbol DEFCONST
        = Package.LISP.intern("DEFCONST");
    /** Interned symbol whose pname is "DEFMACRO". */
    static public final Symbol DEFMACRO
        = Package.LISP.intern("DEFMACRO");
    /** Interned symbol whose pname is "DEFUN". */
    static public final Symbol DEFUN
        = Package.LISP.intern("DEFUN");
    /** Interned symbol whose pname is "DEFVAR". */
    static public final Symbol DEFVAR
        = Package.LISP.intern("DEFVAR");
    /** Interned symbol whose pname is "EQ". */
    static public final Symbol EQ
        = Package.LISP.intern("EQ");
    /** Interned symbol whose pname is "EQUAL". */
    static public final Symbol EQUAL
        = Package.LISP.intern("EQUAL");
    /** Interned symbol whose pname is "EVAL". */
    static public final Symbol EVAL
        = Package.LISP.intern("EVAL");
    /** Interned symbol whose pname is "EVAL-WHEN". */
    static public final Symbol EVAL_WHEN
        = Package.LISP.intern("EVAL-WHEN");
    /** Interned symbol whose pname is "EXP". */
    static public final Symbol EXP
        = Package.LISP.intern("EXP");
    /** Interned symbol whose pname is "EXPR". */
    static public final Symbol EXPR
        = Package.LISP.intern("EXPR");
    /** Interned symbol whose pname is "FAIL-ACT". */
    static public final Symbol FAIL_ACT
        = Package.LISP.intern("FAIL-ACT");
    /** Interned symbol whose pname is "FEXPR". */
    static public final Symbol FEXPR
        = Package.LISP.intern("FEXPR");
    /** Interned symbol whose pname is "FIXNUM". */
    static public final Symbol FIXNUM
        = Package.LISP.intern("FIXNUM");
    /** Interned symbol whose pname is "FLONUM". */
    static public final Symbol FLONUM
        = Package.LISP.intern("FLONUM");
    /** Interned symbol whose pname is "FSUBR". */
    static public final Symbol FSUBR
        = Package.LISP.intern("FSUBR");
    /** Interned symbol whose pname is "FUNCTION". */
    static public final Symbol FUNCTION
        = Package.LISP.intern("FUNCTION");
    /** Interned symbol whose pname is "FUNCTION_INDICATORS". */
    static public final Symbol FUNCTION_INDICATORS
        = Package.LISP.intern("FUNCTION_INDICATORS");
    /** Interned symbol whose pname is "GO". */
    static public final Symbol GO
        = Package.LISP.intern("GO");
    /** Interned symbol whose pname is "HUNK". */
    static public final Symbol HUNK
        = Package.LISP.intern("HUNK");
    /** Interned symbol whose pname is "IF". */
    static public final Symbol IF
        = Package.LISP.intern("IF");
    /** Interned symbol whose pname is "IO-LOSSAGE". */
    static public final Symbol IO_LOSSAGE
        = Package.LISP.intern("IO-LOSSAGE");
    /** Interned symbol whose pname is "LAMBDA". */
    static public final Symbol LAMBDA
        = Package.LISP.intern("LAMBDA");
    /** Interned symbol whose pname is "LAMBDA-LIST-KEYWORDS". */
    static public final Symbol LAMBDA_LIST_KEYWORDS
        = Package.LISP.intern("LAMBDA-LIST-KEYWORDS");
    /** Interned symbol whose pname is "LET". */
    static public final Symbol LET
        = Package.LISP.intern("LET");
    /** Interned symbol whose pname is "LET*". */
    static public final Symbol LET2
        = Package.LISP.intern("LET*");
    /** Interned symbol whose pname is "LIST". */
    static public final Symbol LIST
        = Package.LISP.intern("LIST");
    /** Interned symbol whose pname is "LIST*". */
    static public final Symbol LIST2
        = Package.LISP.intern("LIST*");
    /** Interned symbol whose pname is "LOAD". */
    static public final Symbol LOAD
        = Package.LISP.intern("LOAD");
    /** Interned symbol whose pname is "LSUBR". */
    static public final Symbol LSUBR
        = Package.LISP.intern("LSUBR");
    /** Interned symbol whose pname is "MACRO". */
    static public final Symbol MACRO
        = Package.LISP.intern("MACRO");
    /** Interned symbol whose pname is "MATH-CONST". */
    static public final Symbol MATH_CONST
        = Package.LISP.intern("MATH-CONST");
    /** Interned symbol whose pname is "MULTIPLE-VALUE-BIND". */
    static public final Symbol MULTIPLE_VALUE_BIND
        = Package.LISP.intern("MULTIPLE-VALUE-BIND");
    /** Interned symbol whose pname is "MULTIPLE-VALUE-LIST". */
    static public final Symbol MULTIPLE_VALUE_LIST
        = Package.LISP.intern("MULTIPLE-VALUE-LIST");
    /** Interned symbol whose pname is "MULTIPLE-VALUE-SETQ". */
    static public final Symbol MULTIPLE_VALUE_SETQ
        = Package.LISP.intern("MULTIPLE-VALUE-SETQ");
    /** Interned symbol whose pname is "NCONC". */
    static public final Symbol NCONC
        = Package.LISP.intern("NCONC");
    /** Interned symbol whose pname is "NCONCABLE". */
    static public final Symbol NCONCABLE
        = Package.LISP.intern("NCONCABLE");
    /** Interned symbol whose pname is "NSPLICE". */
    static public final Symbol NSPLICE
        = Package.LISP.intern("NSPLICE");
    /** Interned symbol whose pname is "NTH-VALUE". */
    static public final Symbol NTH_VALUE
        = Package.LISP.intern("NTH-VALUE");
    /** Interned symbol whose pname is "OBARRAY". */
    static public final Symbol OBARRAY
        = Package.LISP.intern("OBARRAY");
    /** Interned symbol whose pname is "OR". */
    static public final Symbol OR
        = Package.LISP.intern("OR");
    /** Interned symbol whose pname is "RANDOM". */
    static public final Symbol RANDOM
        = Package.LISP.intern("RANDOM");
    /** Interned symbol whose pname is "PI". */
    static public final Symbol PI
        = Package.LISP.intern("PI");
    /** Interned symbol whose pname is "PROG". */
    static public final Symbol PROG
        = Package.LISP.intern("PROG");
    /** Interned symbol whose pname is "PROGN". */
    static public final Symbol PROGN
        = Package.LISP.intern("PROGN");
    /** Interned symbol whose pname is "PSETQ". */
    static public final Symbol PSETQ
        = Package.LISP.intern("PSETQ");
    /** Interned symbol whose pname is "QUOTE". */
    static public final Symbol QUOTE
        = Package.LISP.intern("QUOTE");
    /** Interned symbol whose pname is "READTABLE". */
    static public final Symbol READTABLE
        = Package.LISP.intern("READTABLE");
    /** Interned symbol whose pname is "RETURN". */
    static public final Symbol RETURN
        = Package.LISP.intern("RETURN");
    /** Interned symbol whose pname is "RUNTIME". */
    static public final Symbol RUNTIME
        = Package.LISP.intern("RUNTIME");
    /** Interned symbol whose pname is "SETQ". */
    static public final Symbol SETQ
        = Package.LISP.intern("SETQ");
    /** Interned symbol whose pname is "SIGNED-BYTE". */
    static public final Symbol SIGNED_BYTE
        = Package.LISP.intern("SIGNED-BYTE");
    /** Interned symbol whose pname is "SPECIAL". */
    static public final Symbol SPECIAL
        = Package.LISP.intern("SPECIAL");
    /** Interned symbol whose pname is "SPLICE". */
    static public final Symbol SPLICE
        = Package.LISP.intern("SPLICE");
    /** Interned symbol whose pname is "STRING". */
    static public final Symbol STRING
        = Package.LISP.intern("STRING");
    /** Interned symbol whose pname is "SUBR". */
    static public final Symbol SUBR
        = Package.LISP.intern("SUBR");
    /** Interned symbol whose pname is "SYMBOL". */
    static public final Symbol SYMBOL
        = Package.LISP.intern("SYMBOL");
    /** Interned symbol whose pname is "TIME". */
    static public final Symbol TIME
        = Package.LISP.intern("TIME");
    /** Interned symbol whose pname is "UNBND-VRBL". */
    static public final Symbol UNBND_VRBL
        = Package.LISP.intern("UNBND-VRBL");
    /** Interned symbol whose pname is "UNDF-FNCTN". */
    static public final Symbol UNDF_FNCTN
        = Package.LISP.intern("UNDF-FNCTN");
    /** Interned symbol whose pname is "UNQUOTE". */
    static public final Symbol UNQUOTE
        = Package.LISP.intern("UNQUOTE");
    /** Interned symbol whose pname is "UNSEEN-GO-TAG". */
    static public final Symbol UNSEEN_GO_TAG
        = Package.LISP.intern("UNSEEN-GO-TAG");
    /** Interned symbol whose pname is "UNWIND-PROTECT". */
    static public final Symbol UNWIND_PROTECT
        = Package.LISP.intern("UNWIND-PROTECT");
    /** Interned symbol whose pname is "VALUES". */
    static public final Symbol VALUES
        = Package.LISP.intern("VALUES");
    /** Interned symbol whose pname is "VOID". */
    static public final Symbol VOID
        = Package.LISP.intern("VOID");
    /** Interned symbol whose pname is "WRNG-NO-ARGS". */
    static public final Symbol WRNG_NO_ARGS
        = Package.LISP.intern("WRNG-NO-ARGS");
    /** Interned symbol whose pname is "WRNG-TYPE-ARG". */
    static public final Symbol WRNG_TYPE_ARG
        = Package.LISP.intern("WRNG-TYPE-ARG");

    /** Interned symbol whose pname is "*". */
    static public final Symbol ASTERISK
        = Package.LISP.intern("*");
    /** Interned symbol whose pname is "+". */
    static public final Symbol PLUS
        = Package.LISP.intern("+");

    /** Interned symbol whose pname is "*CATCH". */
    static public final Symbol CATCH
        = Package.LISP.intern("*CATCH");
    /** Interned symbol whose pname is "*THROW". */
    static public final Symbol THROW
        = Package.LISP.intern("*THROW");

    /** Interned symbol whose pname is "*COMPILE-CLASS-DIR*". */
    static public final Symbol COMPILE_CLASS_DIR
        = Package.LISP.intern("*COMPILE-CLASS-DIR*");
    /** Interned symbol whose pname is "*COMPILE-INPUT-DIR*". */
    static public final Symbol COMPILE_INPUT_DIR
        = Package.LISP.intern("*COMPILE-INPUT-DIR*");
    /** Interned symbol whose pname is "*COMPILE-OUTPUT-DIR*". */
    static public final Symbol COMPILE_OUTPUT_DIR
        = Package.LISP.intern("*COMPILE-OUTPUT-DIR*");
    /** Interned symbol whose pname is "*LOAD-DIR*". */
    static public final Symbol LOAD_DIR
        = Package.LISP.intern("*LOAD-DIR*");
    /** Interned symbol whose pname is "*LOG-LEVEL*". */
    static public final Symbol LOG_LEVEL
        = Package.LISP.intern("*LOG-LEVEL*");
    /** Interned symbol whose pname is "*LOG-OUT*". */
    static public final Symbol LOG_OUT
        = Package.LISP.intern("*LOG-OUT*");
    /** Interned symbol whose pname is "*NULL-BIN-IN*". */
    static public final Symbol NULL_BIN_IN
        = Package.LISP.intern("*NULL-BIN-IN*");
    /** Interned symbol whose pname is "*NULL-BIN-OUT*". */
    static public final Symbol NULL_BIN_OUT
        = Package.LISP.intern("*NULL-BIN-OUT*");
    /** Interned symbol whose pname is "*NULL-IN*". */
    static public final Symbol NULL_IN
        = Package.LISP.intern("*NULL-IN*");
    /** Interned symbol whose pname is "*NULL-OUT*". */
    static public final Symbol NULL_OUT
        = Package.LISP.intern("*NULL-OUT*");
    /** Interned symbol whose pname is "*PACKAGE*". */
    static public final Symbol PACKAGE
        = Package.LISP.intern("*PACKAGE*");
    /** Interned symbol whose pname is "*PRINT-CASE*". */
    static public final Symbol PRINT_CASE
        = Package.LISP.intern("*PRINT-CASE*");
    /** Interned symbol whose pname is "*PRINT-CIRCLE*". */
    static public final Symbol PRINT_CIRCLE
        = Package.LISP.intern("*PRINT-CIRCLE*");
    /** Interned symbol whose pname is "*PRINT-ESCAPE*". */
    static public final Symbol PRINT_ESCAPE
        = Package.LISP.intern("*PRINT-ESCAPE*");
    /** Interned symbol whose pname is "*PRINT-EXCEPTION-VERBOSE*". */
    static public final Symbol PRINT_EXCEPTION_VERBOSE
        = Package.LISP.intern("*PRINT-EXCEPTION-VERBOSE*");
    /** Interned symbol whose pname is "*PRINT-BACKTRACE-VERBOSE*". */
    static public final Symbol PRINT_BACKTRACE_VERBOSE
        = Package.LISP.intern("*PRINT-BACKTRACE-VERBOSE*");
    /** Interned symbol whose pname is "*READ-EVAL*". */
    static public final Symbol READ_EVAL
        = Package.LISP.intern("*READ-EVAL*");
    /** Interned symbol whose pname is "*SYSTEM-BIN-ERR*". */
    static public final Symbol SYSTEM_BIN_ERR
        = Package.LISP.intern("*SYSTEM-BIN-ERR*");
    /** Interned symbol whose pname is "*SYSTEM-BIN-IN*". */
    static public final Symbol SYSTEM_BIN_IN
        = Package.LISP.intern("*SYSTEM-BIN-IN*");
    /** Interned symbol whose pname is "*SYSTEM-BIN-OUT*". */
    static public final Symbol SYSTEM_BIN_OUT
        = Package.LISP.intern("*SYSTEM-BIN-OUT*");
    /** Interned symbol whose pname is "*SYSTEM-ERR*". */
    static public final Symbol SYSTEM_ERR
        = Package.LISP.intern("*SYSTEM-ERR*");
    /** Interned symbol whose pname is "*SYSTEM-IN*". */
    static public final Symbol SYSTEM_IN
        = Package.LISP.intern("*SYSTEM-IN*");
    /** Interned symbol whose pname is "*SYSTEM-OUT*". */
    static public final Symbol SYSTEM_OUT
        = Package.LISP.intern("*SYSTEM-OUT*");
    /** Interned symbol whose pname is "*TERMINAL-IN*". */
    static public final Symbol TERMINAL_IN
        = Package.LISP.intern("*TERMINAL-IN*");
    /** Interned symbol whose pname is "*TERMINAL-OUT*". */
    static public final Symbol TERMINAL_OUT
        = Package.LISP.intern("*TERMINAL-OUT*");

    /** Interned symbol whose pname is "&amp;OPTIONAL". */
    static public final Symbol LK_OPTIONAL
        = Package.LISP.intern("&OPTIONAL");
    /** Interned symbol whose pname is "&amp;REST". */
    static public final Symbol LK_REST
        = Package.LISP.intern("&REST");
    /** Interned symbol whose pname is "&amp;KEY". */
    static public final Symbol LK_KEY
        = Package.LISP.intern("&KEY");
    /** Interned symbol whose pname is "&amp;AUX". */
    static public final Symbol LK_AUX
        = Package.LISP.intern("&AUX");
    /** Interned symbol whose pname is "&amp;BODY". */
    static public final Symbol LK_BODY
        = Package.LISP.intern("&BODY");
    /** Interned symbol whose pname is "&amp;WHOLE". */
    static public final Symbol LK_WHOLE
        = Package.LISP.intern("&WHOLE");
    /** Interned symbol whose pname is "&amp;ENVIRONMENT". */
    static public final Symbol LK_ENVIRONMENT
        = Package.LISP.intern("&ENVIRONMENT");
    /** Interned symbol whose pname is "&amp;ALLOW-OTHER-KEYS". */
    static public final Symbol LK_ALLOW_OTHER_KEYS
        = Package.LISP.intern("&ALLOW-OTHER-KEYS");

    /** Keyword whose pname is "ABORT". */
    static public final Symbol KW_ABORT
        = Package.KEYWORD.intern("ABORT");
    /** Keyword whose pname is "APPEND". */
    static public final Symbol KW_APPEND
        = Package.KEYWORD.intern("APPEND");
    /** Keyword whose pname is "ALLOW-OTHER-KEYS". */
    static public final Symbol KW_ALLOW_OTHER_KEYS
        = Package.KEYWORD.intern("ALLOW-OTHER-KEYS");
    ///** Keyword whose pname is "CHARACTER-ENCODING". */
    //static public final Symbol KW_CHARACTER_ENCODING
    //    = Package.KEYWORD.intern("CHARACTER-ENCODING");
    /** Keyword whose pname is "CREATE". */
    static public final Symbol KW_CREATE
        = Package.KEYWORD.intern("CREATE");
    /** Keyword whose pname is "DEFINITION". */
    static public final Symbol KW_DEFINITION
        = Package.KEYWORD.intern("DEFINITION");
    /** Keyword whose pname is "DIRECTION". */
    static public final Symbol KW_DIRECTION
        = Package.KEYWORD.intern("DIRECTION");
    /** Keyword whose pname is "DRYRUN". */
    static public final Symbol KW_DRYRUN
        = Package.KEYWORD.intern("DRYRUN");
    /** Keyword whose pname is "DOWNCASE". */
    static public final Symbol KW_DOWNCASE
        = Package.KEYWORD.intern("DOWNCASE");
    /** Keyword whose pname is "ELEMENT-TYPE". */
    static public final Symbol KW_ELEMENT_TYPE
        = Package.KEYWORD.intern("ELEMENT-TYPE");
    /** Keyword whose pname is "END". */
    static public final Symbol KW_END
        = Package.KEYWORD.intern("END");
    /** Keyword whose pname is "ERROR". */
    static public final Symbol KW_ERROR
        = Package.KEYWORD.intern("ERROR");
    /** Keyword whose pname is "EXPORT". */
    static public final Symbol KW_EXPORT
        = Package.KEYWORD.intern("EXPORT");
    /** Keyword whose pname is "EXTERNAL". */
    static public final Symbol KW_EXTERNAL
        = Package.KEYWORD.intern("EXTERNAL");
    /** Keyword whose pname is "EXTERNAL-FORMAT". */
    static public final Symbol KW_EXTERNAL_FORMAT
        = Package.KEYWORD.intern("EXTERNAL-FORMAT");
    /** Keyword whose pname is "GENERATE-CLASS-FILE". */
    static public final Symbol KW_GENERATE_CLASS_FILE
        = Package.KEYWORD.intern("GENERATE-CLASS-FILE");
    /** Keyword whose pname is "IF-DOES-NOT-EXIST". */
    static public final Symbol KW_IF_DOES_NOT_EXIST
        = Package.KEYWORD.intern("IF-DOES-NOT-EXIST");
    /** Keyword whose pname is "IF-EXISTS". */
    static public final Symbol KW_IF_EXISTS
        = Package.KEYWORD.intern("IF-EXISTS");
    /** Keyword whose pname is "IMPORT-FROM". */
    static public final Symbol KW_IMPORT_FROM
        = Package.KEYWORD.intern("IMPORT-FROM");
    /** Keyword whose pname is "INDEX". */
    static public final Symbol KW_INDEX
        = Package.KEYWORD.intern("INDEX");
    /** Keyword whose pname is "INPUT". */
    static public final Symbol KW_INPUT
        = Package.KEYWORD.intern("INPUT");
    /** Keyword whose pname is "INPUT-FILE". */
    static public final Symbol KW_INPUT_FILE
        = Package.KEYWORD.intern("INPUT-FILE");
    /** Keyword whose pname is "INPUT-FILE-ENCODING". */
    static public final Symbol KW_INPUT_FILE_ENCODING
        = Package.KEYWORD.intern("INPUT-FILE-ENCODING");
    /** Keyword whose pname is "INHERITED". */
    static public final Symbol KW_INHERITED
        = Package.KEYWORD.intern("INHERITED");
    /** Keyword whose pname is "INTERN". */
    static public final Symbol KW_INTERN
        = Package.KEYWORD.intern("INTERN");
    /** Keyword whose pname is "INTERNAL". */
    static public final Symbol KW_INTERNAL
        = Package.KEYWORD.intern("INTERNAL");
    /** Keyword whose pname is "OPTIMIZE". */
    static public final Symbol KW_OPTIMIZE
        = Package.KEYWORD.intern("OPTIMIZE");
    /** Keyword whose pname is "OUTPUT". */
    static public final Symbol KW_OUTPUT
        = Package.KEYWORD.intern("OUTPUT");
    /** Keyword whose pname is "OUTPUT-FILE". */
    static public final Symbol KW_OUTPUT_FILE
        = Package.KEYWORD.intern("OUTPUT-FILE");
    /** Keyword whose pname is "OUTPUT-FILE-ENCODING". */
    static public final Symbol KW_OUTPUT_FILE_ENCODING
        = Package.KEYWORD.intern("OUTPUT-FILE-ENCODING");
    /** Keyword whose pname is "OVERWRITE". */
    static public final Symbol KW_OVERWRITE
        = Package.KEYWORD.intern("OVERWRITE");
    /** Keyword whose pname is "PRESERVE-WHITESPACE". */
    static public final Symbol KW_PRESERVE_WHITESPACE
        = Package.KEYWORD.intern("PRESERVE-WHITESPACE");
    /** Keyword whose pname is "PRINT". */
    static public final Symbol KW_PRINT
        = Package.KEYWORD.intern("PRINT");
    /** Keyword whose pname is "UPCASE". */
    static public final Symbol KW_UPCASE
        = Package.KEYWORD.intern("UPCASE");
    /** Keyword whose pname is "START". */
    static public final Symbol KW_START
        = Package.KEYWORD.intern("START");
    /** Keyword whose pname is "USE". */
    static public final Symbol KW_USE
        = Package.KEYWORD.intern("USE");
    /** Keyword whose pname is "USE-BUFFER". */
    static public final Symbol KW_USE_BUFFER
        = Package.KEYWORD.intern("USE-BUFFER");
    /** Keyword whose pname is "USE-LINENUMBER". */
    static public final Symbol KW_USE_LINENUMBER
        = Package.KEYWORD.intern("USE-LINENUMBER");
    /** Keyword whose pname is "USE-OBJECT". */
    static public final Symbol KW_USE_OBJECT
        = Package.KEYWORD.intern("USE-OBJECT");
    /** Keyword whose pname is "USE-PRINT". */
    static public final Symbol KW_USE_PRINT
        = Package.KEYWORD.intern("USE-PRINT");
    /** Keyword whose pname is "USE-UNREAD". */
    static public final Symbol KW_USE_UNREAD
        = Package.KEYWORD.intern("USE-UNREAD");
    /** Keyword whose pname is "VERBOSE". */
    static public final Symbol KW_VERBOSE
        = Package.KEYWORD.intern("VERBOSE");
    /** Keyword whose pname is "WRAP-OPTIONS". */
    static public final Symbol KW_WRAP_OPTIONS
        = Package.KEYWORD.intern("WRAP-OPTIONS");

    static final Object LAMBDA_LIST_KEYWORD_LIST
        = Lists.toList(new Object[] {
                Symbols.LK_OPTIONAL,
                Symbols.LK_REST,
                Symbols.LK_KEY,
                Symbols.LK_ALLOW_OTHER_KEYS,
                Symbols.LK_AUX,
                Symbols.LK_BODY,
                Symbols.LK_WHOLE,
                Symbols.LK_ENVIRONMENT,
            });

    static public boolean isLambdaListKeyword(Object sym) {
        return Data.toBoolean(Lists.memq(sym, LAMBDA_LIST_KEYWORD_LIST));
    }

    static final Object FUNCTION_INDICATOR_LIST
        = Lists.toList(new Object[] {
                Symbols.SUBR,Symbols.EXPR,
                Symbols.MACRO,Symbols.ARRAY,
                Symbols.FSUBR,Symbols.FEXPR
            });

    private Symbols() {}
}
