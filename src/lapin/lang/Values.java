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
import lapin.util.ListBuilder;

/**
 * This class is used as a register that stores multiple-value.
 */
public final class Values {
    static private final ThreadLocal VALUES_REF
        = new ThreadLocal() {
                protected Object initialValue() {
                    int limit = Constants.MULTIPLE_VALUES_LIMIT.intValue();
                    return new Values(limit);
                }
            };
    /** Returns a reference to the fresh MV register. */
    static public Values currentValues() {
        return ((Values) VALUES_REF.get()).clear();
    }
    /** Returns the first value of <code>v</code>
        if <code>v</code> is an instance of Values,
        else <code>v</code> itself. */
    static public Object singleValue(Object v) {
        if (v instanceof Values) {
            v = ((Values) v).nth(0);
        }
        return v;
    }
    /** Returns <code>v</code> itself
        if <code>v</code> is an instance of Values,
        else Values which has one element
        <code>v</code>. */
    static public Values multipleValue(Object v) {
        Values mv;
        if (v instanceof Values) {
            mv = (Values) v;
        }
        else {
            mv = currentValues().push(v);
        }
        return mv;
    }
    /** Returns false if <code>v</code> is an instance of
        Values and length of <code>v</code> is 0. */
    static public boolean hasValue(Object v) {
        return !(v instanceof Values)
            || ((Values) v).length() > 0;
    }

    private int length;
    private Object[] array;

    private Values(int maxLength) {
        length = 0;
        array = new Object[maxLength];
        for (int i = 0; i < maxLength; i++)
            array[i] = Symbols.NIL;
    }
    /** Returns the length of this MV register. */
    public int length() {
        return length;
    }
    /** Returns a value at the specified index.
        If the index is greater than or equal to {@link #length length},
        <code>NIL</code> is returned. */
    public Object nth(int i) {
        if (i < length)
            return array[i];
        else
            return Symbols.NIL;
    }
    /** Adds the specified object to this MV register. */
    public Values push(Object v) {
        array[length++] = v;
        return this;
    }
    /** Remove all values stored in this MV register. */
    public Values clear() {
        for (int i = 0; i < length; i++)
            array[i] = Symbols.NIL;
        length = 0;
        return this;
    }
    /** Returns a list of values stored in this MV register. */
    public Object toList() {
        /*
         * Note that this iteration will not work
         * if the MV is used in the body of following method:
         * ListBuilder#append.
         */
        ListBuilder lb = new ListBuilder();
        for (int i = 0; i < length; i++)
            lb.append(array[i]);
        return lb.toList();
    }
}
