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

/**
 * Operations for property lists.
 */
public final class Plists {
    static public Object get(Object plst, Object indicator, Object defaultVal) {
        Object curr = memq(indicator, plst);
        Object ret;
        if (curr == Symbols.NIL) {
            ret = defaultVal;
        }
        else {
            ret = Lists.cadr(curr);
        }
        return ret;
    }
    static public Object set(Object plst, Object indicator, Object val) {
        Object curr = memq(indicator, plst);
        Object ret;
        if (curr == Symbols.NIL) {
            ret = Lists.list2(indicator, val, plst);
        } else {
            Lists.rplaca(Lists.cdr(curr), val);
            ret = plst;
        }
        return ret;
    }
    static public Object rem(Object plst, Object indicator, Object defaultVal) {
        Object prev = Symbols.NIL;
        Object curr = plst;
        Object next = Lists.cddr(plst);
        Object ret;
        while (true) {
            if (Lists.isEnd(curr)) {
                ret = defaultVal;
                break;
            }
            else if (Lists.isEnd(Lists.cdr(curr))) {
                throw new TypeException
                    ("illegal end of plist", Symbols.NIL);
            }
            else if (Lists.car(curr) == indicator) {
                if (prev == Symbols.NIL) {
                    ret = next;
                }
                else {
                    Lists.rplacd(Lists.cdr(prev), next);
                    ret = plst;
                }
                break;
            }
            else {
                prev = curr;
                curr = next;
                next = Lists.cddr(curr);
            }
        }
        return ret;
    }
    static public Object getl(Object plst, Object indlst) {
        Object l = plst;
        while (true) {
            if (Lists.isEnd(l)) {
                return Symbols.NIL;
            }
            else if (Lists.isEnd(Lists.cdr(l))) {
                throw new TypeException
                    ("illegal end of plist", Symbols.NIL);
            }
            else if (Lists.memq(Lists.car(l), indlst) != Symbols.NIL) {
                return l;
            }
            else {
                l = Lists.cddr(l);
            }
        }
    }
    static public Object memq(Object indicator, Object plst) {
        Object l = plst;
        while (true) {
            if (Lists.isEnd(l)) {
                return Symbols.NIL;
            }
            else if (Lists.isEnd(Lists.cdr(l))) {
                throw new TypeException
                    ("illegal end of plist", Symbols.NIL);
            }
            else if (Lists.car(l) == indicator) {
                return l;
            }
            else {
                l = Lists.cddr(l);
            }
        }
    }

    private Plists() {}
}
