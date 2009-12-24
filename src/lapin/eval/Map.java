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
package lapin.eval;
import lapin.function.Function;
import lapin.lang.Env;
import lapin.lang.Lists;
import lapin.lang.Symbol;
import lapin.lang.Symbols;
import lapin.util.ListBuilder;

/**
 * Mapping functions.
 */
public final class Map {
    /**
     * Executes MAPCAR.
     * @param func Function applied to the arguments in the lists
     * @param lists List of arguments.
     * @param env
     * @return List of the results of each application of the function.
     */
    static public Object mapcar(Function func, Object lists, Env env) {
        ListBuilder lb = new ListBuilder();
        for (Object l = lists; !_isEnd(l); l = _cdr(l)) {
            lb.append(Funcall.funcall(func, _car(l), env));
        }
        return lb.toList();
    }
    /**
     * Executes MAPLIST.
     * @param func Function applied to the arguments in the lists
     * @param lists List of arguments.
     * @param env
     * @return List of the results of each application of the function.
     */
    static public Object maplist(Function func, Object lists, Env env) {
        ListBuilder lb = new ListBuilder();
        for (Object l = lists; !_isEnd(l); l = _cdr(l)) {
            lb.append(Funcall.funcall(func, l, env));
        }
        return lb.toList();
    }
    /**
     * Executes MAPC.
     * @param func Function applied to the arguments in the lists
     * @param lists List of arguments.
     * @param env
     * @return First element of <code>lists</code>.
     */
    static public Object mapc(Function func, Object lists, Env env) {
        for (Object l = lists; !_isEnd(l); l = _cdr(l)) {
            Funcall.funcall(func, _car(l), env);
        }
        return Lists.car(lists);
    }
    /**
     * Executes MAPL.
     * @param func Function applied to the arguments in the lists
     * @param lists List of arguments.
     * @param env
     * @return First element of <code>lists</code>.
     */
    static public Object mapl(Function func, Object lists, Env env) {
        for (Object l = lists; !_isEnd(l); l = _cdr(l)) {
            Funcall.funcall(func, l, env);
        }
        return Lists.car(lists);
    }

    static private boolean _isEnd(Object lists) {
        for (Object l = lists; !Lists.isEnd(l); l = Lists.cdr(l)) {
            if (Lists.car(l) == Symbols.NIL)
                return true;
        }
        return false;
    }
    static private Object _car(Object lists) {
        Object ret = Symbols.NIL;
        for (Object l = lists; !Lists.isEnd(l); l = Lists.cdr(l)) {
            ret = Lists.cons(Lists.caar(l), ret);
        }
        return Lists.nreverse(ret);
    }
    static private Object _cdr(Object lists) {
        Object ret = Symbols.NIL;
        for (Object l = lists; !Lists.isEnd(l); l = Lists.cdr(l)) {
            ret = Lists.cons(Lists.cdar(l), ret);
        }
        return Lists.nreverse(ret);
    }

    private Map() {}
}
