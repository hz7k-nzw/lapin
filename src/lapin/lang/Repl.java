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
import lapin.eval.Evaluator;
import lapin.eval.NonLocalExit;
import lapin.io.IO;
import lapin.io.Printer;
import lapin.io.Reader;
import lapin.util.TracableException;

/**
 * Read-eval-print loop.
 */
public class Repl implements Runnable {
    /** Environment used by this REPL. */
    private Env env;

    public Repl(Lisp lisp) {
        this(lisp.getEnv());
    }
    public Repl(Env env) {
        if (env == null)
            throw new NullPointerException("env is null");
        this.env = env;
    }
    public Env getEnv() {
        return env;
    }
    private Object read() {
        return Reader.read(Symbols.NIL, Symbols.NIL,
                           IO.EOF, Symbols.NIL, env);
    }
    private void skipWhitespace() {
        Reader.peekCharNoHang(Symbols.T,
                              Symbols.NIL, Symbols.NIL,
                              IO.EOF, Symbols.NIL, env);
    }
    private Object eval(Object exp) {
        return Evaluator.eval(exp, env);
    }
    private void prin1(Object exp) {
        Printer.prin1(exp, Symbols.NIL, env);
    }
    private void princ(Object exp) {
        Printer.princ(exp, Symbols.NIL, env);
    }
    private void terpri() {
        Printer.terpri(Symbols.NIL, env);
    }
    private void finishOutput() {
        Printer.finishOutput(Symbols.NIL, env);
    }
    private void printBackTrace(TracableException e) {
        Printer.printBackTrace(e, Symbols.NIL, env);
    }
    private void printException(Throwable t) {
        Printer.printException(t, Symbols.NIL, env);
    }

    /*
     * XXX:
     * Sharing a single lisp instance with multiple threads
     * is NOT safe!
     */
    public void run() {
        //princ("hello!"); terpri();
        //finishOutput();
        Object exp = Symbols.NIL;
        while (true) {
            try {
                princ(Package.get(env).pkgname());
                princ("> ");
                finishOutput();
                exp = read();
                if (exp == IO.EOF) {
                    //terpri(); princ("bye!"); terpri();
                    terpri();
                    finishOutput();
                    return;
                }
                skipWhitespace();
                exp = eval(exp);
                if (exp instanceof Values) {
                    Values mv = (Values) exp;
                    for (Object l = mv.toList();
                         !Lists.isEnd(l); l = Lists.cdr(l)) {
                         terpri(); prin1(Lists.car(l));
                    }
                }
                else {
                     terpri(); prin1(exp);
                }
                terpri();
            } catch (NonLocalExit e) {
                terpri();
                printException(e.exitFailed());
            }
            catch (TracableException e) {
                e.push(this);
                terpri();
                printException(e);
                printBackTrace(e);
            }
            catch (java.lang.RuntimeException e) {
                terpri();
                printException(e);
            }
            catch (java.lang.Error err) {
                terpri();
                printException(err);
            }
            finishOutput();
        }
    }
    public String toString() {
        return "REPL["+env+"]";
    }
}
