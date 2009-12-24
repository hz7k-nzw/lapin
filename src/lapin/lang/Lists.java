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
import java.util.AbstractSequentialList;
import java.util.Iterator;
import java.util.ListIterator;
import java.util.NoSuchElementException;

/**
 * Operations for lists.
 */
public final class Lists {
    /** empty list, which is equal to (List) Symbols.NIL. */
    static private final List EMPTY_LIST = Data.list(Symbols.NIL);

    /**
     * Constructs a new dotted pair whose car is the first argument
     * to CONS, and whose cdr is the second argument to CONS.
     */
    static public Pair cons(Object o1, Object o2) {
        return new Pair(o1, o2);
    }
    /**
     * Returns the left half of a CONS. 
     */
    static public Object car(Object lst) {
        return Data.list(lst).car();
    }
    /**
     * Returns the right half of a CONS. 
     */
    static public Object cdr(Object lst) {
        return Data.list(lst).cdr();
    }
    static public Object first(Object lst) {
        return car(lst);
    }
    static public Object second(Object lst) {
        return car(cdr(lst));
    }
    static public Object third(Object lst) {
        return car(cdr(cdr(lst)));
    }
    static public Object fourth(Object lst) {
        return car(cdr(cdr(cdr(lst))));
    }
    static public Object rest(Object lst) {
        return cdr(lst);
    }
    static public Object caar(Object lst) {
        return car(car(lst));
    }
    static public Object cadr(Object lst) {
        return car(cdr(lst));
    }
    static public Object cdar(Object lst) {
        return cdr(car(lst));
    }
    static public Object cddr(Object lst) {
        return cdr(cdr(lst));
    }
    static public Object caaar(Object lst) {
        return car(car(car(lst)));
    }
    static public Object caadr(Object lst) {
        return car(car(cdr(lst)));
    }
    static public Object cadar(Object lst) {
        return car(cdr(car(lst)));
    }
    static public Object cdaar(Object lst) {
        return cdr(car(car(lst)));
    }
    static public Object caddr(Object lst) {
        return car(cdr(cdr(lst)));
    }
    static public Object cdadr(Object lst) {
        return cdr(car(cdr(lst)));
    }
    static public Object cddar(Object lst) {
        return cdr(cdr(car(lst)));
    }
    static public Object cdddr(Object lst) {
        return cdr(cdr(cdr(lst)));
    }
    static public Object caaaar(Object lst) {
        return car(car(car(car(lst))));
    }
    static public Object caaadr(Object lst) {
        return car(car(car(cdr(lst))));
    }
    static public Object caadar(Object lst) {
        return car(car(cdr(car(lst))));
    }
    static public Object cadaar(Object lst) {
        return car(cdr(car(car(lst))));
    }
    static public Object cdaaar(Object lst) {
        return cdr(car(car(car(lst))));
    }
    static public Object caaddr(Object lst) {
        return car(car(cdr(cdr(lst))));
    }
    static public Object cadadr(Object lst) {
        return car(cdr(car(cdr(lst))));
    }
    static public Object cdaadr(Object lst) {
        return cdr(car(car(cdr(lst))));
    }
    static public Object caddar(Object lst) {
        return car(cdr(cdr(car(lst))));
    }
    static public Object cdadar(Object lst) {
        return cdr(car(cdr(car(lst))));
    }
    static public Object cddaar(Object lst) {
        return cdr(cdr(car(car(lst))));
    }
    static public Object cadddr(Object lst) {
        return car(cdr(cdr(cdr(lst))));
    }
    static public Object cdaddr(Object lst) {
        return cdr(car(cdr(cdr(lst))));
    }
    static public Object cddadr(Object lst) {
        return cdr(cdr(car(cdr(lst))));
    }
    static public Object cdddar(Object lst) {
        return cdr(cdr(cdr(car(lst))));
    }
    static public Object cddddr(Object lst) {
        return cdr(cdr(cdr(cdr(lst))));
    }
    /**
     * Returns the empty list.
     */
    static public List list() {
        return EMPTY_LIST;
    }
    static public List list(Object o1) {
        return cons(o1, EMPTY_LIST);
    }
    static public List list(Object o1, Object o2) {
        return cons(o1, list(o2));
    }
    static public List list(Object o1, Object o2, Object o3) {
        return cons(o1, list(o2, o3));
    }
    static public List list(Object o1, Object o2, Object o3, Object o4) {
        return cons(o1, list(o2, o3, o4));
    }
    static public List list(Object o1, Object o2, Object o3, Object o4,
                            Object o5) {
        return cons(o1, list(o2, o3, o4, o5));
    }
    static public Object list2(Object o1) {
        return o1;
    }
    static public Object list2(Object o1, Object o2) {
        return cons(o1, o2);
    }
    static public Object list2(Object o1, Object o2, Object o3) {
        return cons(o1, list2(o2, o3));
    }
    static public Object list2(Object o1, Object o2, Object o3, Object o4) {
        return cons(o1, list2(o2, o3, o4));
    }
    static public Object list2(Object o1, Object o2, Object o3, Object o4,
                               Object o5) {
        return cons(o1, list2(o2, o3, o4, o5));
    }
    /**
     * Returns true if <code>lst</code> is a empty list,
     * false if <code>lst</code> is conses. 
     */
    static public boolean isEnd(Object lst) {
        return Data.list(lst).isEnd();
    }
    /**
     * Returns the length of its argument list;
     */
    static public int length(Object lst) {
        return Data.list(lst).length();
    }
    /**
     * Returns the last dotted pair of its argument list, 
     * which must be NIL or a well-formed list. 
     */
    //static public Object last(Object lst) {
    //    if (lst == Symbols.NIL) {
    //        return lst;
    //    }
    //    else {
    //        while (true) {
    //            if (cdr(lst) == Symbols.NIL) {
    //                break;
    //            }
    //            else {
    //                lst = cdr(lst);
    //            }
    //        }
    //        return lst;
    //    }
    //}
    static public List last(Object lst) {
        if (lst == EMPTY_LIST) {
            return EMPTY_LIST;
        }
        else {
            Pair p = Data.pair(lst);
            while (true) {
                if (p.cdr() == EMPTY_LIST) {
                    break;
                }
                else {
                    p = Data.pair(p.cdr());
                }
            }
            return p;
        }
    }
    /** Returns all elements but the last. */
    static public List butlast(Object lst) {
        if (lst == EMPTY_LIST) {
            return EMPTY_LIST;
        }
        else {
            Pair p = Data.pair(lst);
            List head = null;
            Pair tail = null;
            while (true) {
                if (p.cdr() == EMPTY_LIST) {
                    if (head == null) {
                        head = EMPTY_LIST;
                    }
                    else {
                        tail.rplacd(EMPTY_LIST);
                    }
                    break;
                }
                else {
                    if (tail == null) {
                        tail = cons(p.car(), null);
                        head = tail;
                    }
                    else {
                        tail.rplacd(cons(p.car(), null));
                        tail = Data.pair(tail.cdr());
                    }
                    p = Data.pair(p.cdr());
                }
            }
            return head;
        }
    }
    /**
     * Returns the nth element of the list, lst,
     * where n=0 denotes the first element. 
     */
    static public Object nth(int n, Object lst) {
        return car(nthcdr(n, lst));
    }
    /**
     * Returns the nth tail of the list, lst,
     * where n=0 means return the whole list.
     */
    static public Object nthcdr(int n, Object lst) {
        if (n < 0)
            throw new TypeException
                ("n must be non negative integer: "+n, Symbols.NIL);
        for (int i = 0; i < n; i++) {
            lst = cdr(lst);
        }
        return lst;
    }
    /**
     * Returns the concatenation of the arguments.
     * To avoid modifying the arguments, copies are made of all but the last.
     */
    //static public Object append(Object lst, Object o) {
    //    if (lst == Symbols.NIL)
    //        return o;
    //    Object head = cons(car(lst), null);
    //    Object tail = head;
    //    Object l = cdr(lst);
    //    while (true) {
    //        if (isEnd(l)) {
    //            rplacd(tail, o);
    //            return head;
    //        }
    //        else {
    //            rplacd(tail, cons(car(l), null));
    //            tail = cdr(tail);
    //            l = cdr(l);
    //        }
    //    }
    //}
    static public Object append(Object lst, Object o) {
        List l = Data.list(lst);
        if (l.isEnd())
            return o;
        Pair head = cons(l.car(), null);
        Pair tail = head;
        l = Data.list(l.cdr());
        while (true) {
            if (l.isEnd()) {
                tail.rplacd(o);
                return head;
            }
            else {
                tail.rplacd(cons(l.car(), null));
                tail = Data.pair(tail.cdr());
                l = Data.list(l.cdr());
            }
        }
    }
    static public List reverse(Object lst) {
        List ret = EMPTY_LIST;
        for (Object l = lst; !isEnd(l); l = cdr(l)) {
            ret = cons(car(l), ret);
        }
        return ret;
    }
    //static public Object copyList(Object lst) {
    //    if (lst == Symbols.NIL)
    //        return lst;
    //    Object head = cons(car(lst), null);
    //    Object tail = head;
    //    Object l = cdr(lst);
    //    while (true) {
    //        if (Data.isAtom(l)) {
    //            rplacd(tail, l);
    //            return head;
    //        }
    //        else {
    //            rplacd(tail, cons(car(l), null));
    //            tail = cdr(tail);
    //            l = cdr(l);
    //        }
    //    }
    //}
    static public List copyList(Object lst) {
        List l = Data.list(lst);
        if (l.isEnd())
            return l;
        Pair head = cons(l.car(), null);
        Pair tail = head;
        l = Data.list(l.cdr());
        while (true) {
            if (Data.isAtom(l)) {
                tail.rplacd(l);
                return head;
            }
            else {
                tail.rplacd(cons(l.car(), null));
                tail = Data.pair(tail.cdr());
                l = Data.list(l.cdr());
            }
        }
    }
    //static public Object member(Object o, Object lst) {
    //    Object l = lst;
    //    while (true) {
    //        if (isEnd(l))
    //            return Symbols.NIL;
    //        else if (Data.isEqual(car(l), o))
    //            return l;
    //        else
    //            l = cdr(l);
    //    }
    //}
    static public List member(Object o, Object lst) {
        List l = Data.list(lst);
        while (true) {
            if (l.isEnd())
                return l;
            else if (Data.isEqual(l.car(), o))
                return l;
            else
                l = Data.list(l.cdr());
        }
    }
    //static public Object memq(Object o, Object lst) {
    //    Object l = lst;
    //    while (true) {
    //        if (isEnd(l))
    //            return Symbols.NIL;
    //        else if (car(l) == o)
    //            return l;
    //        else
    //            l = cdr(l);
    //    }
    //}
    static public List memq(Object o, Object lst) {
        List l = Data.list(lst);
        while (true) {
            if (l.isEnd())
                return l;
            else if (l.car() == o)
                return l;
            else
                l = Data.list(l.cdr());
        }
    }

    /*
     * destructive operations
     */

    static public Pair rplaca(Object pair, Object o) {
        return Data.pair(pair).rplaca(o);
    }
    static public Pair rplacd(Object pair, Object o) {
        return Data.pair(pair).rplacd(o);
    }
    //static public Object nconc(Object lst, Object o) {
    //    if (lst == Symbols.NIL)
    //        return o;
    //    Object head = lst;
    //    Object tail = head;
    //    while (true) {
    //        if (Data.isAtom(cdr(tail))) {
    //            rplacd(tail, o);
    //            return head;
    //        }
    //        else {
    //            tail = cdr(tail);
    //        }
    //    }
    //}
    static public Object nconc(Object lst, Object o) {
        if (lst == EMPTY_LIST)
            return o;
        Pair head = Data.pair(lst);
        Pair tail = head;
        while (true) {
            if (Data.isAtom(tail.cdr())) {
                tail.rplacd(o);
                return head;
            }
            else {
                tail = Data.pair(tail.cdr());
            }
        }
    }
    //static public Object nreverse(Object lst) {
    //    if (lst == Symbols.NIL)
    //        return lst;
    //    Object new_cdr = Symbols.NIL;
    //    Object p = lst;
    //    Object org_cdr = cdr(p);
    //    while (true) {
    //        if (org_cdr == Symbols.NIL) {
    //            return cons(car(p), new_cdr);
    //        }
    //        else {
    //            rplacd(p, new_cdr);
    //            new_cdr = p;
    //            p = org_cdr;
    //            org_cdr = cdr(p);
    //        }
    //    }
    //}
    static public List nreverse(Object lst) {
        if (lst == EMPTY_LIST)
            return EMPTY_LIST;
        List new_cdr = EMPTY_LIST;
        Pair p = Data.pair(lst);
        List org_cdr = Data.list(p.cdr());
        while (true) {
            if (org_cdr.isEnd()) {
                return cons(p.car(), new_cdr);
            }
            else {
                p.rplacd(new_cdr);
                new_cdr = p;
                p = Data.pair(org_cdr);
                org_cdr = Data.list(p.cdr());
            }
        }
    }
    //static public Object delete(Object o, Object lst) {
    //    Object l = lst;
    //    while (true) {
    //        if (l == Symbols.NIL) {
    //            return l;
    //        }
    //        else if (Data.isEqual(o, car(l))) {
    //            l = cdr(l);
    //        }
    //        else {
    //            rplacd(l, delete(o, cdr(l)));
    //            return l;
    //        }
    //    }
    //}
    static public List delete(Object o, Object lst) {
        List l = Data.list(lst);
        while (true) {
            if (l.isEnd()) {
                return l;
            }
            else if (Data.isEqual(o, l.car())) {
                l = Data.list(l.cdr());
            }
            else {
                rplacd(l, delete(o, l.cdr()));
                return l;
            }
        }
    }
    //static public Object delete(Object o, Object lst, int n) {
    //    Object l = lst;
    //    int i = n;
    //    while (true) {
    //        if (l == Symbols.NIL || i < 1) {
    //            return l;
    //        }
    //        else if (Data.isEqual(o, car(l))) {
    //            l = cdr(l);
    //            i--;
    //        }
    //        else {
    //            rplacd(l, delete(o, cdr(l), i));
    //            return l;
    //        }
    //    }
    //}
    static public List delete(Object o, Object lst, int n) {
        List l = Data.list(lst);
        int i = n;
        while (true) {
            if (l.isEnd() || i < 1) {
                return l;
            }
            else if (Data.isEqual(o, l.car())) {
                l = Data.list(l.cdr());
                i--;
            }
            else {
                rplacd(l, delete(o, l.cdr(), i));
                return l;
            }
        }
    }
    //static public Object delq(Object o, Object lst) {
    //    Object l = lst;
    //    while (true) {
    //        if (l == Symbols.NIL) {
    //            return l;
    //        }
    //        else if (o == car(l)) {
    //            l = cdr(l);
    //        }
    //        else {
    //            rplacd(l, delq(o, cdr(l)));
    //            return l;
    //        }
    //    }
    //}
    static public List delq(Object o, Object lst) {
        List l = Data.list(lst);
        while (true) {
            if (l.isEnd()) {
                return l;
            }
            else if (o == l.car()) {
                l = Data.list(l.cdr());
            }
            else {
                rplacd(l, delq(o, l.cdr()));
                return l;
            }
        }
    }
    //static public Object delq(Object o, Object lst, int n) {
    //    Object l = lst;
    //    int i = n;
    //    while (true) {
    //        if (l == Symbols.NIL || i < 1) {
    //            return l;
    //        }
    //        else if (o == car(l)) {
    //            l = cdr(l);
    //            i--;
    //        }
    //        else {
    //            rplacd(l, delq(o, cdr(l), i));
    //            return l;
    //        }
    //    }
    //}
    static public List delq(Object o, Object lst, int n) {
        List l = Data.list(lst);
        int i = n;
        while (true) {
            if (l.isEnd() || i < 1) {
                return l;
            }
            else if (o == l.car()) {
                l = Data.list(l.cdr());
                i--;
            }
            else {
                rplacd(l, delq(o, l.cdr(), i));
                return l;
            }
        }
    }

    /*
     * alist operations
     */

    static public Object assoc(Object o, Object alst) {
        Object l = alst;
        while (true) {
            if (isEnd(l))
                return Symbols.NIL;
            else if (Data.isEqual(caar(l), o))
                return car(l);
            else
                l = cdr(l);
        }
    }
    static public Object assq(Object o, Object alst) {
        Object l = alst;
        while (true) {
            if (isEnd(l))
                return Symbols.NIL;
            else if (caar(l) == o)
                return car(l);
            else
                l = cdr(l);
        }
    }
    static public List pairlis(Object lst1, Object lst2, Object lst3) {
        List i = Data.list(lst1);
        List j = Data.list(lst2);
        List l = Data.list(lst3);
        while (true) {
            if (i.isEnd() && j.isEnd())
                return l;
            else if (i.isEnd() || j.isEnd())
                throw new UndefinedOperationException
                    ("wrong number of arguments", Symbols.NIL);
            else {
                l = cons(cons(i.car(), j.car()), l);
                i = Data.list(i.cdr());
                j = Data.list(j.cdr());
            }
        }
    }

    /*
     * java-array operations
     */

    static public List toList(Object[] array) {
        if (array.length <= 0)
            return EMPTY_LIST;
        List l = EMPTY_LIST;
        for (int i = array.length-1; i>=0; i--)
            l = cons(array[i], l);
        return l;
    }
    static public Object[] toArray(Object lst) {
        int len = length(lst);
        Object[] array = new Object[len];
        Object l =lst;
        for (int i = 0; i < len; i++) {
            array[i] = car(l); l = cdr(l);
        }
        return array;
    }

    /*
     * java-list operations
     */

    /** Returns a fixed-size java-list backed by the specified lisp-list. */
    static public java.util.List asJavaList(Object lst) {
        return new JavaListWrapper(lst);
    }
    static class JavaListWrapper extends AbstractSequentialList {
        private Object lst;
        JavaListWrapper(Object o) {
            this.lst = Data.list(o);
        }
        public int size() {
            return Lists.length(lst);
        }
        public ListIterator listIterator(int index) {
            return new ListItr(Lists.nthcdr(index, lst), index);
        }
        static class ListItr implements ListIterator {
            private Object prev,next;
            private int index;
            ListItr(Object next, int index) {
                this.prev = null;
                this.next = next;
                this.index = index;
            }
            public boolean hasNext() {
                return !Lists.isEnd(next);
            }
            public Object next() {
                if (Lists.isEnd(next))
                    throw new NoSuchElementException();
                Object o = Lists.car(next);
                prev = next;
                next = Lists.cdr(next);
                index++;
                return o;
            }
            public int nextIndex() {
                return index;
            }
            public boolean hasPrevious() {
                throw new UnsupportedOperationException();
            }
            public Object previous() {
                throw new UnsupportedOperationException();
            }
            public int previousIndex() {
                throw new UnsupportedOperationException();
            }
            public void add(Object o) {
                throw new UnsupportedOperationException();
            }
            public void remove() {
                throw new UnsupportedOperationException();
            }
            public void set(Object o) {
                if (prev == null)
                    throw new IllegalStateException("next not called");
                Lists.rplaca(prev, o);
            }
        }/* end of ListItr */
    }/* end of JavaListWrapper */

    /** Returns a iterator backed by the specified lisp-list. */
    static public Iterator asIterator(Object lst) {
        return new SimpleItr(lst);
    }
    static class SimpleItr implements Iterator {
        private Object next;
        SimpleItr(Object o) {
            next = Data.list(o);
        }
        public boolean hasNext() {
            return !Lists.isEnd(next);
        }
        public Object next() {
            if (Lists.isEnd(next))
                throw new NoSuchElementException();
            Object o = Lists.car(next);
            next = Lists.cdr(next);
            return o;
        }
        public void remove() {
            throw new UnsupportedOperationException();
        }
    }/* end of SimpleItr */

    private Lists() {}
}
