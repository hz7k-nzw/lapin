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
package lapin.util;
import lapin.lang.Data;
import lapin.lang.Lists;
import lapin.lang.Symbols;

public class ListBuilder {
    private Object head = Symbols.NIL;
    private Object tail = Symbols.NIL;

    public ListBuilder append(Object o) {
        if (head == Symbols.NIL) {
            head = tail = Lists.cons(o, Symbols.NIL);
        }
        else {
            Object pair = Lists.cons(o, Symbols.NIL);
            Lists.rplacd(tail, pair);
            tail = pair;
        }
        return this;
    }
    public ListBuilder appendList(Object lst) {
        checkArg(lst);
        for (Object l = lst; !Lists.isEnd(l); l = Lists.cdr(l)) {
            append(Lists.car(l));
        }
        return this;
    }
    public ListBuilder concatList(Object lst) {
        checkArg(lst);
        if (head == Symbols.NIL)
            head = lst;
        else
            Lists.rplacd(tail, lst);
        tail = Lists.last(lst);
        return this;
    }
    private void checkArg(Object arg) {
        Data.list(arg);
    }
    public ListBuilder rplacd(Object o) {
        Lists.rplacd(tail, o);
        return this;
    }
    public ListBuilder clear() {
        head = tail = Symbols.NIL;
        return this;
    }
    public Object toList() {
        Object ret = head;
        head = tail = Symbols.NIL;
        return ret;
    }
//    public String toString() {
//        return head.toString();
//    }

//    static public void main(String[] args) {
//        System.out.println(new ListBuilder());
//        System.out.println(new ListBuilder().append("a"));
//        System.out.println(new ListBuilder()
//                           .append("a").append(Lists.list("b", "c", "d")));
//        System.out.println(new ListBuilder()
//                           .append("a").appendList(Lists.list("b", "c", "d")));
//        System.out.println(new ListBuilder()
//                           .append("a").concatList(Lists.list("b", "c", "d")));
//        System.out.println(new ListBuilder().append("a").clear());
//        System.out.println(new ListBuilder().append("a").rplacd("b"));
//    }

}
