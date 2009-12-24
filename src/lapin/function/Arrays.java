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
package lapin.function;
import lapin.lang.IllegalDataTypeException;
import lapin.lang.Symbol;

/**
 * This class contains various methods for manipulating {@link Array}.
 */
public final class Arrays {
    static public Array make(Symbol name, Symbol type,
                             int d0) {
        return new Array.DIM1(name.pname(), type, d0);
    }
    static public Array make(Symbol name, Symbol type,
                             int d0, int d1) {
        return new Array.DIM2(name.pname(), type, d0, d1);
    }
    static public Array make(Symbol name, Symbol type,
                             int d0, int d1, int d2) {
        return new Array.DIM3(name.pname(), type, d0, d1, d2);
    }
    static public Array make(Symbol name, Symbol type,
                             int d0, int d1, int d2, int d3) {
        return new Array.DIM4(name.pname(), type, d0, d1, d2, d3);
    }
    static public Array make(Symbol name, Symbol type,
                             int d0, int d1, int d2, int d3, int d4) {
        return new Array.DIM5(name.pname(), type, d0, d1, d2, d3, d4);
    }

    static public Object get(Object a,
                             int i0) {
        return dim1(a).data[i0];
    }
    static public Object get(Object a,
                             int i0, int i1) {
        return dim2(a).data[i0][i1];
    }
    static public Object get(Object a,
                             int i0, int i1, int i2) {
        return dim3(a).data[i0][i1][i2];
    }
    static public Object get(Object a,
                             int i0, int i1, int i2, int i3) {
        return dim4(a).data[i0][i1][i2][i3];
    }
    static public Object get(Object a,
                             int i0, int i1, int i2, int i3, int i4) {
        return dim5(a).data[i0][i1][i2][i3][i4];
    }

    static public void set(Object a, Object o,
                           int i0) {
        Array.DIM1 dim1 = dim1(a);
        dim1.data[i0] = dim1.checkValue(o);
    }
    static public void set(Object a, Object o,
                           int i0, int i1) {
        Array.DIM2 dim2 = dim2(a);
        dim2(a).data[i0][i1] = dim2.checkValue(o);
    }
    static public void set(Object a, Object o,
                           int i0, int i1, int i2) {
        Array.DIM3 dim3 = dim3(a);
        dim3.data[i0][i1][i2] = dim3.checkValue(o);
    }
    static public void set(Object a, Object o,
                           int i0, int i1, int i2, int i3) {
        Array.DIM4 dim4 = dim4(a);
        dim4.data[i0][i1][i2][i3] = dim4.checkValue(o);
    }
    static public void set(Object a, Object o,
                           int i0, int i1, int i2, int i3, int i4) {
        Array.DIM5 dim5 = dim5(a);
        dim5.data[i0][i1][i2][i3][i4] = dim5.checkValue(o);
    }

//    static public void fill(Object a, Object q) {
//        if (isDim1(a))
//            dim1(a).fill(q);
//        else if (isDim2(a))
//            dim2(a).fill(q);
//        else if (isDim3(a))
//            dim3(a).fill(q);
//        else if (isDim4(a))
//            dim4(a).fill(q);
//        else if (isDim5(a))
//            dim5(a).fill(q);
//        else
//            throw new IllegalDataTypeException("illegal array type: "+a);
//    }

//    static boolean isDim1(Object a) {
//        return a instanceof Array.DIM1;
//    }
//    static boolean isDim2(Object a) {
//        return a instanceof Array.DIM2;
//    }
//    static boolean isDim3(Object a) {
//        return a instanceof Array.DIM3;
//    }
//    static boolean isDim4(Object a) {
//        return a instanceof Array.DIM4;
//    }
//    static boolean isDim5(Object a) {
//        return a instanceof Array.DIM5;
//    }

    static Array.DIM1 dim1(Object o) {
        try {
            return (Array.DIM1) o;
        } catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, Array.DIM1.class);
        }
    }
    static Array.DIM2 dim2(Object o) {
        try {
            return (Array.DIM2) o;
        } catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, Array.DIM2.class);
        }
    }
    static Array.DIM3 dim3(Object o) {
        try {
            return (Array.DIM3) o;
        } catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, Array.DIM3.class);
        }
    }
    static Array.DIM4 dim4(Object o) {
        try {
            return (Array.DIM4) o;
        } catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, Array.DIM4.class);
        }
    }
    static Array.DIM5 dim5(Object o) {
        try {
            return (Array.DIM5) o;
        } catch (ClassCastException e) {
            throw new IllegalDataTypeException(o, Array.DIM5.class);
        }
    }

    private Arrays() {}
}

