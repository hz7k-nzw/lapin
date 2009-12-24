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
import lapin.eval.Def;
//import lapin.eval.Evaluator;
import lapin.function.Subr;
import lapin.function.SystemSubr;
import lapin.lang.Env;
import lapin.lang.Symbols;
import lapin.lang.Values;

public final class LispFSubrs {
    static public final Subr DEFPROP = new DEFPROP();
    static public final Subr DEFUN = new DEFUN();
    static public final Subr DEFMACRO = new DEFMACRO();
    static public final Subr DEFCMACRO = new DEFCMACRO();
    static public final Subr DECLARE = new DECLARE();
    static public final Subr DEFVAR = new DEFVAR();
    static public final Subr DEFCONST = new DEFCONST();
    static public final Subr DEFPACKAGE = new DEFPACKAGE();
    static public final Subr COMMENT = new COMMENT();
    //static public final Subr PROGN = new PROGN();

    static class DEFPROP extends SystemSubr.SUBR1 {
        DEFPROP() { super("DEFPROP"); }
        public Object call1(Object r0, Env env) {
            return Def.defprop(r0, env);
        }
    }
    static class DEFUN extends SystemSubr.SUBR1 {
        DEFUN() { super("DEFUN"); }
        public Object call1(Object r0, Env env) {
            return Def.defun(r0, env);
        }
    }
    static class DEFMACRO extends SystemSubr.SUBR1 {
        DEFMACRO() { super("DEFMACRO"); }
        public Object call1(Object r0, Env env) {
            return Def.defmacro(r0, env);
        }
    }
    static class DEFCMACRO extends SystemSubr.SUBR1 {
        DEFCMACRO() { super("DEFINE-COMPILER-MACRO"); }
        public Object call1(Object r0, Env env) {
            return Def.defcmacro(r0, env);
        }
    }
    static class DECLARE extends SystemSubr.SUBR1 {
        DECLARE() { super("DECLARE"); }
        public Object call1(Object r0, Env env) {
            return Symbols.DECLARE;
        }
    }
    static class DEFVAR extends SystemSubr.SUBR1 {
        DEFVAR() { super("DEFVAR"); }
        public Object call1(Object r0, Env env) {
            return Def.defvar(r0, env);
        }
    }
    static class DEFCONST extends SystemSubr.SUBR1 {
        DEFCONST() { super("DEFCONST"); }
        public Object call1(Object r0, Env env) {
            return Def.defconst(r0, env);
        }
    }
    static class DEFPACKAGE extends SystemSubr.SUBR1 {
        DEFPACKAGE() { super("DEFPACKAGE"); }
        public Object call1(Object r0, Env env) {
            return Def.defpackage(r0, env);
        }
    }
    static class COMMENT extends SystemSubr.SUBR1 {
        COMMENT() { super("COMMENT"); }
        public Object call1(Object r0, Env env) {
            return Symbols.COMMENT;
        }
    }
    //static class PROGN extends SystemSubr.SUBR1 {
    //    PROGN() { super("PROGN"); }
    //    public Object call1(Object r0, Env env) {
    //        return Evaluator.evalBody(r0, env);
    //    }
    //}

    private LispFSubrs() {}
}
