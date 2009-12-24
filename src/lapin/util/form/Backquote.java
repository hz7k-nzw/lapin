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
package lapin.util.form;
import lapin.lang.Data;
//import lapin.lang.Env;
import lapin.lang.Lists;
import lapin.lang.ProgramException;
import lapin.lang.Symbols;

public final class Backquote {
    /*
     * ported from clisp-2.47/src/backquote.lisp
     */

    //static public Object member(Object elem, Object lst) {
    //    for (Object l = lst; !Data.isAtom(l); l = Lists.cdr(l)) {
    //        //if (Data.equal(Lists.car(l), elem))
    //        if (Lists.car(l) == elem)
    //            return l;
    //    }
    //    return Symbols.NIL;
    //}
    static public Object expand(Object form/*, Env env*/) {
        // `()   -> ()
        if (form == Symbols.NIL) {
            return Symbols.NIL;
        }
        // `pair -> expandPair(pair)
        else if (Data.isPair(form)) {
            return expandPair(form);
        }
        // `other -> 'other
        else {
            return Lists.list(Symbols.QUOTE, form);
        }
    }
    static private Object expandPair(Object form) {
        Object car = Lists.car(form);
        // `,from  -> form
        if (car == Symbols.UNQUOTE) {
            return Lists.cadr(form);
        }
        // `,@form | `,.form -> error
        else if (car == Symbols.SPLICE ||
                 car == Symbols.NSPLICE) {
            throw new ProgramException
                ("non list splice error: ~S.", Lists.list(form));
        }
        // ``form  -> `expand(form)
        else if (car == Symbols.BACKQUOTE) {
            Object expansion = expand(Lists.cadr(form));
            return Lists.list(Symbols.BACKQUOTE, expansion);
        }
        // list -> (append f1 f2 f3 ...)
        //         where (f1 f2 f3 ...) is the output of
        //         expandList(list)
        else {
            Object expansion = expandList(form);
            return appendMultiple(expansion);
        }
    }
    static private Object expandList(Object forms) {
        Object expanded = Symbols.NIL;
        while (true) {
            if (forms == Symbols.NIL) {
                break;
            }
            expanded = Lists.cons
                (transform(Lists.car(forms)), expanded);
            Object tail = Lists.cdr(forms);
            if (tail == Symbols.NIL) {
                break;
            }
            else if (Data.isPair(tail)) {
                Object car = Lists.car(tail);
                // well-defined dotted unquote: `( ... . ,form)
                if (car == Symbols.UNQUOTE) {
                    expanded = Lists.cons
                        (Lists.cadr(tail), expanded);
                    break;
                }
                // undefined dotted splice: `( ... . ,@form)
                else if (car == Symbols.SPLICE ||
                         car == Symbols.NSPLICE) {
                    throw new ProgramException
                        ("non dotted splice error: ~S.",
                         Lists.list(car));
                }
                else {
                    forms = tail;
                }
            }
            else {
                expanded = Lists.cons
                    (Lists.list(Symbols.BACKQUOTE, tail), expanded);
                break;
            }
        }
        return Lists.nreverse(expanded);
    }
    static private Object appendMultiple(Object forms) {
        forms = Lists.reverse(forms);
        if (Lists.isEnd(forms)) {
            return Symbols.NIL;
        }
        Object result;
        boolean nconcable;
        Object form = Lists.car(forms);
        if (Data.isPair(form) &&
            Lists.car(form) == Symbols.NCONCABLE) {
            result = Lists.cadr(form);
            nconcable = true;
        }
        else {
            result = form;
            nconcable = false;
        }
        forms = Lists.cdr(forms);
        while (true) {
            if (Lists.isEnd(forms)) {
                break;
            }
            form = Lists.car(forms);
            if (Data.isPair(form) &&
                Lists.car(form) == Symbols.NCONCABLE) {
                result = _nconc
                    (Lists.cadr(form), 
                     (splicingp(result) && !nconcable)
                     ? Lists.list(Symbols.APPEND, result)
                     : result);
            }
            else {
                result = _append
                    (form, 
                     (splicingp(result) && nconcable)
                     ? Lists.list(Symbols.NCONC, result)
                     : result);
            }
            nconcable = false;
            forms = Lists.cdr(forms);
        }
        return nonSplicing(result);
    }
    static private Object transform(Object form) {
        if (Data.isPair(form)) {
            Object car = Lists.car(form);
            if (car == Symbols.UNQUOTE) {
                return _list(Lists.cadr(form));
            }
            else if (car == Symbols.SPLICE) {
                return Lists.cadr(form);
            }
            else if (car == Symbols.NSPLICE) {
                return Lists.list(Symbols.NCONCABLE, Lists.cadr(form));
            }
            else if (car == Symbols.BACKQUOTE) {
                Object expansion = expand(Lists.cadr(form));
                return _list(Lists.list(Symbols.BACKQUOTE,expansion));
            }
            else {
                return _list(expand(form));
            }
        }
        else {
            return _list(expand(form));
        }
    }
    static private boolean splicingp(Object form) {
        if (!Data.isPair(form)) {
            return false;
        }
        Object car = Lists.car(form);
        if (car == Symbols.UNQUOTE) {
            return splicingp(Lists.cadr(form));
        }
        else if (car == Symbols.SPLICE ||
                 car == Symbols.NSPLICE) {
            return true;
        }
        else {
            return false;
        }
    }
    static private Object nonSplicing(Object form) {
        return splicingp(form)
            ? Lists.list(Symbols.APPEND, form) : form;
    }
    static private Object _cons(Object form1, Object form2) {
        Object op = splicingp(form1)
            ? Symbols.LIST2 : Symbols.CONS;
        return Lists.list(op, form1, form2);
    }
    static private Object _list(Object form) {
        return Lists.list(Symbols.LIST, form);
    }
    static private Object _append(Object form1, Object form2) {
        return Lists.list(Symbols.APPEND, form1, form2);
    }
    static private Object _nconc(Object form1, Object form2) {
        return Lists.list(Symbols.NCONC, form1, form2);
    }

    private Backquote() {
    }
}
