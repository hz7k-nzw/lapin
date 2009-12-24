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
package lapin.tool;
import lapin.io.Printer;
import lapin.lang.Env;
import lapin.lang.Lisp;
import lapin.lang.Lists;
import lapin.lang.Repl;
import lapin.lang.Symbols;
import lapin.load.Loader;
import lapin.util.FormattableException;

/**
 * Provides a main method which activates a lisp session
 * or loads the specified files.
 */
public final class Main {
    static public void main(String[] args) {
        Lisp lisp = new Lisp();
        Env env = lisp.getEnv();
        if (args.length <= 0) {
            // run repl
            Printer.format("hello!~%", Symbols.NIL, Symbols.NIL, env);
            new Repl(env).run();
            Printer.format("bye!~%", Symbols.NIL, Symbols.NIL, env);
        }
        else {
            // load files
            for (int i = 0; i < args.length; i++) {
                Printer.format("loadfile: ~A~%",
                               Lists.list(args[i]), Symbols.NIL, env);
                try {
                    Loader.loadFile(args[i], Symbols.NIL, env);
                    Printer.format("load successful~%",
                                   Symbols.NIL, Symbols.NIL, env);
                } catch (java.lang.RuntimeException e) {
                    Printer.format("load failed~%",
                                   Symbols.NIL, Symbols.NIL, env);
                    FormattableException.fillInMessage(e, env);
                    throw e;
                }
            }
        }
    }
    private Main() {}
}
