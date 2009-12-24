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
package lapin.io;
import lapin.eval.Evaluator;
import lapin.function.Function;
import lapin.function.Expr;
import lapin.function.LambdaList;
import lapin.lang.Data;
import lapin.lang.Env;
import lapin.lang.Hunk;
//import lapin.lang.IllegalDataTypeException;
import lapin.lang.Lists;
import lapin.lang.Obarray;
import lapin.lang.Package;
import lapin.lang.Pair;
import lapin.lang.Symbol;
import lapin.lang.Symbols;
import lapin.lang.Values;
import lapin.util.FormattableException;
import lapin.util.Logger;
import lapin.util.TracableException;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.IdentityHashMap;

/**
 * Printer functions.
 */
public final class Printer {
    // uninterned symbols: for private use only
    static private final Symbol LABEL_COUNTER
        = Symbol.gensym("_LABEL-COUNTER_");
    static private final Symbol LABEL_MAP
        = Symbol.gensym("_LABEL-MAP_");

    /**
     * Writes the specified byte to <code>stream</code>.
     * @param b The byte
     * @param stream NIL or binary output stream.
     *        NIL means the value of the special variable *SYSTEM-BIN-OUT*.
     * @param env
     * @return The byte passed to this method
     * @throws StreamException
     */
    static public Object writeByte(Integer b, Object stream, Env env) {
        OutputStream out = IO.outputStream(stream, env);
        try {
            out.write(b.intValue());
            return b;
        } catch (IOException e) {
            throw new StreamException
                ("I/O error occurred while writing stream: ~S.",
                 Lists.list(stream), e);
        }
    }

    static private PrintWriter printWriter(Object stream, Env env) {
        java.io.Writer w = IO.writer(stream, env);
        try {
            return (PrintWriter) w;
        }
        catch (ClassCastException e) {
            //throw new IllegalDataTypeException(w, PrintWriter.class);
            Logger.warn("PrintWriter is required: ~S",
                        Lists.list(w), env);
            return new PrintWriter(w);
        }
    }
    /**
     * Writes the specified character to <code>stream</code>.
     * @param ch Character
     * @param stream NIL, T or character output stream.
     *        NIL means the value of the special variable *SYSTEM-OUT*.
     *        T means the value of the special variable *TERMINAL-OUT*.
     * @param env
     * @return The character passed to this method
     */
    static public Object writeChar(Character ch, Object stream, Env env) {
        PrintWriter out = printWriter(stream, env);
        out.print(ch.charValue());
        return ch;
    }
    /**
     * Writes the specified string to <code>stream</code>.
     * @param str String
     * @param stream NIL, T or character output stream.
     *        NIL means the value of the special variable *SYSTEM-OUT*.
     *        T means the value of the special variable *TERMINAL-OUT*.
     * @param env
     * @return The string passed to this method
     */
    static public Object writeString(String str, Object stream, Env env) {
        PrintWriter out = printWriter(stream, env);
        out.print(str);
        return str;
    }
    /**
     * Writes the specified string to <code>stream</code> and then
     * outputs a newline afterwards.
     * @param str String
     * @param stream NIL, T or character output stream.
     *        NIL means the value of the special variable *SYSTEM-OUT*.
     *        T means the value of the special variable *TERMINAL-OUT*.
     * @param env
     * @return The string passed to this method
     */
    static public Object writeLine(String str, Object stream, Env env) {
        PrintWriter out = printWriter(stream, env);
        out.println(str);
        return str;
    }
    /**
     * Outputs a newline to <code>stream</code>.
     * @param stream NIL, T or character output stream.
     *        NIL means the value of the special variable *SYSTEM-OUT*.
     *        T means the value of the special variable *TERMINAL-OUT*.
     * @param env
     * @return NIL
     */
    static public Object terpri(Object stream, Env env) {
        PrintWriter out = printWriter(stream, env);
        out.println();
        out.flush();
        return Symbols.NIL;
    }
    /**
     * Flushs <code>stream</code>.
     * @param stream NIL, T or character output stream.
     *        NIL means the value of the special variable *SYSTEM-OUT*.
     *        T means the value of the special variable *TERMINAL-OUT*.
     * @param env
     * @return NIL
     */
    static public Object finishOutput(Object stream, Env env) {
        PrintWriter out = printWriter(stream, env);
        out.flush();
        return Symbols.NIL;
    }
    /**
     * Produces formatted output by outputting the characters of
     * <code>ctrlstr</code> and observing that a tilde introduces
     * a directive. The character after the tilde specifies what kind of
     * formatting is desired. Most directives use one or more elements of
     * <code>args</code> to create their output. 
     * Note that this implementation supports the following directive:
     * <code>~S, ~A, ~%</code>.
     * @param ctrlstr Format control string
     * @param args List of objects consumed by the directives in
     *        <code>ctrlstr</code>
     * @param stream NIL, T or character output stream.
     *        NIL means the value of the special variable *SYSTEM-OUT*.
     *        T means the value of the special variable *TERMINAL-OUT*.
     * @param env
     * @return NIL
     */
    static public Object format(String ctrlstr, Object args,
                                Object stream, Env env) {
        PrintWriter out = printWriter(stream, env);
        boolean inCtrl = false;
        Object l = args;
        Object o = null;
        char[] chars = ctrlstr.toCharArray();
        int max = chars.length;
        int off = 0;
        int len = 0;
        for (int i = 0; i < max; i++) {
            char c = chars[i];
            if (inCtrl) {
                switch (c) {
                case 'S':
                case 's':
                    if (Lists.isEnd(l))
                        throw new IllegalArgumentException
                            ("args.length less than required.");
                    o = Lists.car(l); l = Lists.cdr(l);
                    prin1(o, out, env);
                    inCtrl = false;
                    off += 2;
                    break;
                case 'A':
                case 'a':
                    if (Lists.isEnd(l))
                        throw new IllegalArgumentException
                            ("args.length less than required.");
                    o = Lists.car(l); l = Lists.cdr(l);
                    princ(o, out, env);
                    off += 2;
                    inCtrl = false;
                    break;
                case '%':
                    out.println();
                    off += 2;
                    inCtrl = false;
                    break;
                case '~':
                    out.write('~');
                    off += 2;
                    inCtrl = false;
                    break;
                default:
                    throw new IllegalArgumentException
                        ("unsupported ctrl char: '"+c+"'");
                }
            }
            else {
                switch (c) {
                case '~':
                    if (i >= max-1)
                        throw new IllegalArgumentException
                            ("ctrl string terminates "+
                             "within a format directive.");
                    out.write(chars, off, len);
                    off = off+len;
                    len = 0;
                    inCtrl = true;
                    break;
                default:
                    len++;
                    break;
                }
            }
        }
        out.write(chars, off, len);
        out.flush();
        return Symbols.NIL;
    }
    /**
     * This method is just like {@link #prin1 prin1} except that
     * the output has no escape characters.
     * It binds <code>*PRINT-ESCAPE*</code> to NIL.
     * @param stream NIL, T or character output stream.
     *        NIL means the value of the special variable *SYSTEM-OUT*.
     *        T means the value of the special variable *TERMINAL-OUT*.
     * @param env
     * @return Object passed to this method
     */
    static public Object princ
        (Object o, Object stream, Env env) {
        PrintWriter out = printWriter(stream, env);
        {
            Env newenv = env.child();
            initEnv(newenv);
            newenv.bind(Symbols.PRINT_ESCAPE, Symbols.NIL);
            checkCircle(o, newenv);
            printInternal(o, out, newenv);
            return o;
        }
    }
    /**
     * Produces output suitable for input to {@link Reader#read read}.
     * It binds <code>*PRINT-ESCAPE*</code> to T.
     * @param stream NIL, T or character output stream.
     *        NIL means the value of the special variable *SYSTEM-OUT*.
     *        T means the value of the special variable *TERMINAL-OUT*.
     * @param env
     * @return Object passed to this method
     */
    static public Object prin1
        (Object o, Object stream, Env env) {
        PrintWriter out = printWriter(stream, env);
        {
            Env newenv = env.child();
            initEnv(newenv);
            newenv.bind(Symbols.PRINT_ESCAPE, Symbols.T);
            checkCircle(o, newenv);
            printInternal(o, out, newenv);
            return o;
        }
    }
    /**
     * This method is just like {@link #prin1 prin1} except that
     * the printed representation of object is preceded by a newline
     * and followed by a space.
     * It binds <code>*PRINT-ESCAPE*</code> to T.
     * @param stream NIL, T or character output stream.
     *        NIL means the value of the special variable *SYSTEM-OUT*.
     *        T means the value of the special variable *TERMINAL-OUT*.
     * @param env
     * @return Object passed to this method
     */
    static public Object print
        (Object o, Object stream, Env env) {
        PrintWriter out = printWriter(stream, env);
        {
            Env newenv = env.child();
            initEnv(newenv);
            newenv.bind(Symbols.PRINT_ESCAPE, Symbols.T);
            out.println();
            checkCircle(o, newenv);
            printInternal(o, out, newenv);
            out.print(' ');
            return o;
        }
    }
    static public String princToString(Object exp, Env env) {
        java.io.Writer w = IO.makeStringWriter();
        princ(exp, w, env);
        return IO.getStringWriter(w).toString();
    }
    static public String prin1ToString(Object exp, Env env) {
        java.io.Writer w = IO.makeStringWriter();
        prin1(exp, w, env);
        return IO.getStringWriter(w).toString();
    }
    static public String formatToString(String ctrlstr, Object args, Env env) {
        java.io.Writer w = IO.makeStringWriter();
        format(ctrlstr, args, w, env);
        return IO.getStringWriter(w).toString();
    }
    static public Object printBackTrace
        (TracableException e, Object stream, Env env) {
        PrintWriter out = printWriter(stream, env);
        Object l = Lists.reverse(e.backTrace());
        int i = 0;
        if (Data.isNot(env.get(Symbols.PRINT_BACKTRACE_VERBOSE))) {
            out.print("[trace] ");
            for (; !Lists.isEnd(l); l = Lists.cdr(l)) {
                if ((i++) > 0)
                    out.print(" << ");
                Object o = Lists.car(l);
                if (Data.isPair(o))
                    prin1(Lists.car(o), out, env);
                else
                    prin1(o, out, env);
            }
            out.println();
        }
        else {
            out.println("[trace]");
            for (; !Lists.isEnd(l); l = Lists.cdr(l)) {
                out.print((i++)+":\t");
                prin1(Lists.car(l), out, env);
                out.println();
            }
        }
        out.flush();
        return e;
    }
    static public Object printException
        (Throwable t, Object stream, Env env) {
        PrintWriter out = printWriter(stream, env);
        out.print("[error] ");
        _printException(t, out, env);
        out.println();
        if (Data.isNot(env.get(Symbols.PRINT_EXCEPTION_VERBOSE))) {
            for (Throwable cause = t.getCause();
                 cause != null; cause = cause.getCause()) {
                out.print("[cause] ");
                _printException(cause, out, env);
                out.println();
            }
        } else {
            FormattableException.fillInMessage(t, env);
            out.println("[detail]");
            t.printStackTrace(out);
        }
        out.flush();
        return t;
    }
    static private void _printException
        (Throwable t, PrintWriter out, Env env) {
        if (t instanceof FormattableException) {
            FormattableException fe = (FormattableException) t;
            out.print(fe.getClass().getName());
            out.print(": ");
            format(fe.ctrlstr(), fe.args(), out, env);
        }
        else {
            out.print(t);
        }
    }
    static private void checkCircle(Object o, Env env) {
        if (!isPrintCircle(env)) {
            return;
        }
        if (Data.isPair(o)) {
            Label label = getLabel(o, env);
            if (label == null) {
                label = new Label();
                //System.out.println("put pair: "+o+"-> "+label);
                putLabel(o, label, env);
                checkCircle(Lists.car(o), env);
                checkCircle(Lists.cdr(o), env);
            } else {
                //System.out.println("pair found: "+p);
                label.setCircle(true);
            }
        }
        else if (Data.isSymbol(o) &&
                 Data.symbol(o).pkg() == null) {
                 /*!Symbol.isInternedSymbol(o)) {*/
            Label label = getLabel(o, env);
            if (label == null) {
                label = new Label();
                //System.out.println("put uninterned symbol: "+o+"-> "+label);
                putLabel(o, label, env);
            } else {
                //System.out.println("uninterned symbol found: "+o);
                label.setCircle(true);
            }
        }
    }
    static private void printInternal
        (Object o, PrintWriter out, Env env) {
        if (Data.isAtom(o)) {
            // for atoms
            printAtom(o, out, env);
        } else if (Data.isPair(o)) {
            // for pair
            Pair p = Data.pair(o);
            printPair(p, out, env);
        }
        else {
            // for others
            out.print(o);
        }
        out.flush();
    }
    static private void printAtom
        (Object o, PrintWriter out, Env env) {
        if (Data.isSymbol(o)) {
            printSymbol(Data.symbol(o), out, env);
        }
        else if (Data.isJavaNumber(o)) {
            printNumber(Data.javaNumber(o), out, env);
        }
        else if (Data.isString(o)) {
            printString(Data.string(o), out, env);
        }
        else if (Data.isCharacter(o)) {
            printCharacter(Data.character(o), out, env);
        }
        else if (Data.isFunction(o)) {
            printFunction(Data.function(o), out, env);
        }
        else if (Data.isLambdaList(o)) {
            printLambdaList(Data.lambdaList(o), out, env);
        }
        else if (Data.isHunk(o)) {
            printHunk(Data.hunk(o), out, env);
        }
        else {
            out.print("#<"); out.print(o); out.print('>');
        }
    }
    static private void printPair
        (Pair p, PrintWriter out, Env env) {
        Label label = getLabel(p, env);
        if (label != null && label.isCircle()) {
            if (label.isProcessed()) {
                out.print(label);
                out.print('#');
            }
            else {
                label.setProcessed(true);
                label.setIndex(incCount(env));
                out.print(label);
                out.print('=');
                _printPair(p, out, env);
            }
        }
        else {
            _printPair(p, out, env);
        }
    }
    static private void _printPair
        (Pair p, PrintWriter out, Env env) {
        out.print('(');
        while (true) {
            Object car = Lists.car(p);
            Object cdr = Lists.cdr(p);
            // print car
            printInternal(car, out, env);
            // process cdr
            if (cdr == Symbols.NIL) {
                out.print(')');
                break;
            }
            else if (Data.isPair(cdr)) {
                Label label = getLabel(cdr, env);
                if (label != null && label.isCircle()) {
                    out.print(" . ");
                    printInternal(cdr, out, env);
                    out.print(')');
                    break;
                }
                else {
                    out.print(' ');
                    p = Data.pair(cdr);
                    continue;
                }
            }
            else {
                out.print(" . ");
                printInternal(cdr, out, env);
                out.print(')');
                break;
            }
        }
    }
    static private void printSymbol
        (Symbol sym, PrintWriter out, Env env) {
        Label label = getLabel(sym, env);
        if (label != null && label.isCircle()) {
            if (label.isProcessed()) {
                out.print(label);
                out.print('#');
            }
            else {
                label.setProcessed(true);
                label.setIndex(incCount(env));
                out.print(label);
                out.print('=');
                _printSymbol(sym, out, env);
            }
        }
        else {
            _printSymbol(sym, out, env);
        }
    }
    static private void _printSymbol
        (Symbol sym, PrintWriter out, Env env) {
        Obarray obarray = env.lisp().getObarray();
        Package currpkg = Package.get(env);
        Package pkg = sym.pkg();
        String pname = sym.pname();
        // print package and marker
        if (pkg == null) {
        //if (!Symbol.isInternedSymbol(sym)) {
            out.print("#:");
        }
        else if (pkg == Package.KEYWORD) {
            out.print(":");
        }
        else {
            Values mv = obarray.find(currpkg, pname, false);
            Symbol s = Data.symbol(mv.nth(0));
            Object o = mv.nth(1);
            if (s == sym && o != Symbols.NIL) {
                /*
                 * sym is accessible from current package
                 */
            }
            else {
                /*
                 * sym is not accessible from current package
                 */
                if (isPrintEscape(env)) {
                    printSymbolNamePart(pkg.pkgname(), out, env);
                }
                else {
                    out.print(pkg.pkgname());
                }
                if (obarray.isExternal(pkg, sym)) {
                    out.print(":");
                }
                else {
                    out.print("::");
                }
            }
        }
        // print pname
        if (isPrintEscape(env)) {
            // print-escape is not nil
            //System.out.println("debug: print-escape is not nil");
            printSymbolNamePart(pname, out, env);
        }
        else {
            // print-escape is nil
            //System.out.println("debug: print-escape is nil");
            out.print(pname);
        }
    }
    static private void printSymbolNamePart
        (String name, PrintWriter out, Env env) {
        Readtable rt = env.lisp().getReadtable();
        boolean needsEscape;
        ESCAPE_CHECK:
        while (true) {
            if (name.length() <= 0) {
                //System.out.println("debug: NG <empty>");
                needsEscape = true;
                break ESCAPE_CHECK;
            }
            for (int i = 0; i < name.length(); i++) {
                char c = name.charAt(i);
                if (rt.isConstituent(c)) {
                    if ((rt.getAttr(c) & Readtable.ATTR_ALPHA) == 0) {
                        //System.out.println("debug: NG '"+c+"'");
                        needsEscape = true;
                        break ESCAPE_CHECK;
                    }
                    if (c != IO.toUpcase(c)) {
                        //System.out.println("debug: NG '"+c+"'");
                        needsEscape = true;
                        break ESCAPE_CHECK;
                    }
                    //System.out.println("debug: OK '"+c+"'");
                    continue;
                }
                if (rt.isNonTerminatingMacro(c)) {
                    if (i <= 0) {
                        //System.out.println("debug: NG '"+c+"'");
                        needsEscape = true;
                        break ESCAPE_CHECK;
                    }
                    if ((rt.getAttr(c) & Readtable.ATTR_ALPHA) == 0) {
                        //System.out.println("debug: NG '"+c+"'");
                        needsEscape = true;
                        break ESCAPE_CHECK;
                    }
                    if (c != IO.toUpcase(c)) {
                        //System.out.println("debug: NG '"+c+"'");
                        needsEscape = true;
                        break ESCAPE_CHECK;
                    }
                    //System.out.println("debug: OK '"+c+"'");
                    continue;
                }
                //System.out.println("debug: NG '"+c+"'");
                needsEscape = true;
                break ESCAPE_CHECK;
            }
            if (IO.parseDot(name) != Symbols.NIL) {
                //System.out.println("debug: NG <dots>");
                needsEscape = true;
                break ESCAPE_CHECK;
            }
            if (IO.parseNumber(name) != Symbols.NIL) {
                //System.out.println("debug: NG <number>");
                needsEscape = true;
                break ESCAPE_CHECK;
            }
            needsEscape = false;
            break ESCAPE_CHECK;
        }/* end of infinite loop */
        //System.out.println("debug: needsEscape="+needsEscape);
        if (needsEscape) {
            //// print name with escape
            //int single_escape = rt.getSingleEscapeChar();
            //int multiple_escape = rt.getMultipleEscapeChar();
            //out.print((char) multiple_escape);
            //for (int i = 0; i < name.length(); i++) {
            //    char c = name.charAt(i);
            //    if (c == single_escape ||
            //        c == multiple_escape)
            //        out.print((char) single_escape);
            //    out.print(c);
            //}
            //out.print((char) multiple_escape);
            out.print('|');
            for (int i = 0; i < name.length(); i++) {
                char c = name.charAt(i);
                if (rt.isSingleEscape(c) || rt.isMultipleEscape(c)) {
                    out.print('\\');
                }
                out.print(c);
            }
            out.print('|');
        }
        else {
            // print name without escape
            Symbol printCase = getPrintCase(env);
            if (printCase == Symbols.KW_UPCASE)
                out.print(name.toUpperCase());
            else if (printCase == Symbols.KW_DOWNCASE)
                out.print(name.toLowerCase());
            else
                // XXX
                throw new IllegalStateException
                    ("unsupported print-case: "+printCase);
        }
    }
    static private void printNumber
        (Number n, PrintWriter out, Env env) {
        out.print(n);
    }
    static private void printString
        (String s, PrintWriter out, Env env) {
        if (isPrintEscape(env)) {
            out.print('"');
            for (int i = 0, len = s.length(); i < len; i++) {
                char c = s.charAt(i);
                switch (c) {
                case '\n':
                    out.print("\\n");
                    break;
                case '\t':
                    out.print("\\t");
                    break;
                case '\b':
                    out.print("\\b");
                    break;
                case '\r':
                    out.print("\\r");
                    break;
                 case '\f':
                    out.print("\\f");
                    break;
                case '\\':
                    out.print("\\\\");
                    break;
                case '\'':
                    out.print("\\\'");
                    break;
                case '"':
                    out.print("\\\"");
                    break;
               default:
                    out.print(c);
                    break;
                }
            }
            out.print('"');
        }
        else {
            out.print(s);
        }
    }
    static private void printCharacter
        (Character c, PrintWriter out, Env env) {
        char ch = c.charValue();
        if (isPrintEscape(env)) {
            String name = IO.charToName((int) ch);
            if (name == null) {
                // name not found
                if ((int) ch < Readtable.ASCII_LIMIT) {
                    // format: #\<ch>
                    name = String.valueOf(ch);
                }
                else {
                    // format: #\Uxxxx
                    StringBuffer buf = new StringBuffer();
                    buf.append(Integer.toHexString((int) ch));
                    while (buf.length() < 4)
                        buf.insert(0, "0");
                    name = buf.insert(0, "U").toString();
                }
            }
            else {
                // name found
                // format: #\<name>
            }
            out.print("#\\");
            out.print(name);
        }
        else {
            out.print(ch);
        }
    }
    static private void printFunction
        (Function func, PrintWriter out, Env env) {
        if (Data.isExpr(func)) {
            Expr expr = Data.expr(func);
            String name = expr.name();
            Object params = expr.lambdaList().params();
            Object body = expr.body();
            out.print("#<");
            out.print(name);
            out.print(": ");
            printInternal(Lists.list2(Symbols.LAMBDA, params, body), out, env);
            out.print(">");
        }
        else {
            out.print("#<");
            out.print(func);
            out.print(">");
        }
    }
    static private void printLambdaList
        (LambdaList ll, PrintWriter out, Env env) {
        out.print("#<LAMBDA-LIST:");
        printInternal(ll.params(), out, env);
        out.print(">");
    }
    static private void printHunk
        (Hunk hunk, PrintWriter out, Env env) {
        out.print("#<HUNK:");
        printInternal(hunk.toList(), out, env);
        out.print(">");
    }

    static private void initEnv(Env env) {
        env.bind(LABEL_COUNTER, Symbols.NIL);
        env.bind(LABEL_MAP, Symbols.NIL);
    }
    static private int incCount(Env env) {
        Object v = env.get(LABEL_COUNTER);
        if (v == Symbols.NIL) {
            v = Data.toFixnum(1);
            //env.set(LABEL_COUNTER, v);
        }
        int count = Data.fixnum(v).intValue();
        env.set(LABEL_COUNTER, Data.toFixnum(count+1));
        return count;
    }
    static private Label getLabel(Object o, Env env) {
        Object v = env.get(LABEL_MAP);
        if (v == Symbols.NIL) {
            return null;
        }
        IdentityHashMap objLabelMap = (IdentityHashMap) v;
        return (Label) objLabelMap.get(o);
    }
    static private void putLabel(Object o, Label label, Env env) {
        Object v = env.get(LABEL_MAP);
        if (v == Symbols.NIL) {
            v = new IdentityHashMap();
            env.set(LABEL_MAP, v);
        }
        IdentityHashMap objLabelMap = (IdentityHashMap) v;
        objLabelMap.put(o, label);
    }
    static private Symbol getPrintCase(Env env) {
        return Data.symbol(env.get(Symbols.PRINT_CASE));
    }
    static private boolean isPrintCircle(Env env) {
        return Data.toBoolean(env.get(Symbols.PRINT_CIRCLE));
    }
    static private boolean isPrintEscape(Env env) {
        return Data.toBoolean(env.get(Symbols.PRINT_ESCAPE));
    }

    static private class Label {
        int index = 0;
        boolean circle = false;
        boolean processed = false;

        void setIndex(int i) {
            this.index = i;
        }
        int getIndex() {
            return index;
        }
        void setCircle(boolean f) {
            this.circle = f;
        }
        boolean isCircle() {
            return circle;
        }
        void setProcessed(boolean f) {
            this.processed = f;
        }
        boolean isProcessed() {
            return processed;
        }
        public String toString() {
            //return "#"+index+"("+circle+" "+processed+")";
            return "#"+index;
        }
    } /* end of Label */

    private Printer() {}
}

