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
import lapin.lang.Data;
import lapin.lang.Env;
import lapin.lang.TypeException;
import lapin.lang.Lists;
import lapin.lang.Symbol;
import lapin.lang.Symbols;

/**
 * Base class for MACLISP array.
 * Note that MACLISP treats array like a function,
 * which means FUNCALL, APPLY accepts array as a function object.
 */
public abstract class Array extends Subr implements lapin.lang.Array {
    private Symbol type;
    private int lenDims = -1;
    Array(String name, Symbol type) {
        super(name);
        if (type != Symbols.T &&
            type != Symbols.FIXNUM &&
            type != Symbols.FLONUM)
            throw new TypeException
                ("illegal array type: ~S", Lists.list(type));
        this.type = type;
    }
    public Symbol type() {
        return type;
    }

    final Object initValue() {
        if (type() == Symbols.FIXNUM)
            return Data.toFixnum(0);
        else if (type() == Symbols.FLONUM)
            return Data.toFlonum(0.0);
        else
            return Symbols.NIL;
    }
    final Object checkValue(Object o) {
        if (type() == Symbols.FIXNUM)
            return Data.fixnum(o);
        else if (type() == Symbols.FLONUM)
            return Data.flonum(o);
        else
            return o;
    }
    //abstract void fill(Object q);

    public final String toString() {
        if (lenDims < 0) {
            lenDims = Lists.length(dims());
        }
        return "ARRAY:"+type()+"-"+lenDims+"-"
            +name()+"-"+Integer.toHexString(hashCode());
    }

    static class DIM1 extends Array implements Callable1 {
        final Object[] data;
        DIM1(String name, Symbol type,
             int d0) {
            super(name, type);
            if (d0 <= 0)
                throw new TypeException
                    ("d0 must be positive: "+d0, Symbols.NIL);
            data = new Object[d0];
            Object q = initValue();
            for (int i0 = 0; i0 < d0; i0++)
                data[i0] = q;
        }
        public Object call1(Object arg0, Env env) {
            int i0 = Data.fixnum(arg0).intValue();
            return data[i0];
        }
        public Object dims() {
            return Lists.list(Data.toFixnum(data.length));
        }
//        void fill(Object q) {
//            q = checkValue(q);
//            Object dims = dims();
//            int d0 = Data.fixnum(Lists.car(dims)).intValue();
//            for (int i0 = 0; i0 < d0; i0++)
//                data[i0] = q;
//        }
    }/* end of DIM1 */

    static class DIM2 extends Array implements Callable2 {
        final Object[][] data;
        DIM2(String name, Symbol type,
             int d0, int d1) {
            super(name, type);
            if (d0 <= 0)
                throw new TypeException
                    ("d0 must be positive: "+d0, Symbols.NIL);
            if (d1 <= 0)
                throw new TypeException
                    ("d1 must be positive: "+d1, Symbols.NIL);
            data = new Object[d0][d1];
            Object q = initValue();
            for (int i0 = 0; i0 < d0; i0++)
                for (int i1 = 0; i1 < d1; i1++)
                    data[i0][i1] = q;
        }
        public Object call2(Object arg0, Object arg1, Env env) {
            int i0 = Data.fixnum(arg0).intValue();
            int i1 = Data.fixnum(arg1).intValue();
            return data[i0][i1];
        }
        public Object dims() {
            return Lists.list(Data.toFixnum(data.length),
                              Data.toFixnum(data[0].length));
        }
//        void fill(Object q) {
//            q = checkValue(q);
//            Object dims = dims();
//            int d0 = Data.fixnum(Lists.car(dims)).intValue();
//            dims = Lists.cdr(dims);
//            int d1 = Data.fixnum(Lists.car(dims)).intValue();
//            for (int i0 = 0; i0 < d0; i0++)
//                for (int i1 = 0; i1 < d1; i1++)
//                    data[i0][i1] = q;
//        }
    }/* end of DIM2 */

    static class DIM3 extends Array implements Callable3 {
        final Object[][][] data;
        DIM3(String name, Symbol type,
             int d0, int d1, int d2) {
            super(name, type);
            if (d0 <= 0)
                throw new TypeException
                    ("d0 must be positive: "+d0, Symbols.NIL);
            if (d1 <= 0)
                throw new TypeException
                    ("d1 must be positive: "+d1, Symbols.NIL);
            if (d2 <= 0)
                throw new TypeException
                    ("d2 must be positive: "+d2, Symbols.NIL);
            data = new Object[d0][d1][d2];
            Object q = initValue();
            for (int i0 = 0; i0 < d0; i0++)
                for (int i1 = 0; i1 < d1; i1++)
                    for (int i2 = 0; i2 < d2; i2++)
                        data[i0][i1][i2] = q;
        }
        public Object call3(Object arg0, Object arg1, Object arg2, Env env) {
            int i0 = Data.fixnum(arg0).intValue();
            int i1 = Data.fixnum(arg1).intValue();
            int i2 = Data.fixnum(arg2).intValue();
            return data[i0][i1][i2];
        }
        public Object dims() {
            return Lists.list(Data.toFixnum(data.length),
                              Data.toFixnum(data[0].length),
                              Data.toFixnum(data[0][0].length));
        }
//        void fill(Object q) {
//            q = checkValue(q);
//            Object dims = dims();
//            int d0 = Data.fixnum(Lists.car(dims)).intValue();
//            dims = Lists.cdr(dims);
//            int d1 = Data.fixnum(Lists.car(dims)).intValue();
//            dims = Lists.cdr(dims);
//            int d2 = Data.fixnum(Lists.car(dims)).intValue();
//            for (int i0 = 0; i0 < d0; i0++)
//                for (int i1 = 0; i1 < d1; i1++)
//                    for (int i2 = 0; i2 < d2; i2++)
//                        data[i0][i1][i2] = q;
//        }
    }/* end of DIM3 */

    static class DIM4 extends Array implements Callable4 {
        final Object[][][][] data;
        DIM4(String name, Symbol type,
             int d0, int d1, int d2, int d3) {
            super(name, type);
            if (d0 <= 0)
                throw new TypeException
                    ("d0 must be positive: "+d0, Symbols.NIL);
            if (d1 <= 0)
                throw new TypeException
                    ("d1 must be positive: "+d1, Symbols.NIL);
            if (d2 <= 0)
                throw new TypeException
                    ("d2 must be positive: "+d2, Symbols.NIL);
            if (d3 <= 0)
                throw new TypeException
                    ("d3 must be positive: "+d3, Symbols.NIL);
            data = new Object[d0][d1][d2][d3];
            Object q = initValue();
            for (int i0 = 0; i0 < d0; i0++)
                for (int i1 = 0; i1 < d1; i1++)
                    for (int i2 = 0; i2 < d2; i2++)
                        for (int i3 = 0; i3 < d3; i3++)
                            data[i0][i1][i2][i3] = q;
        }
        public Object call4(Object arg0, Object arg1, Object arg2,
                            Object arg3, Env env) {
            int i0 = Data.fixnum(arg0).intValue();
            int i1 = Data.fixnum(arg1).intValue();
            int i2 = Data.fixnum(arg2).intValue();
            int i3 = Data.fixnum(arg3).intValue();
            return data[i0][i1][i2][i3];
        }
        public Object dims() {
            return Lists.list(Data.toFixnum(data.length),
                              Data.toFixnum(data[0].length),
                              Data.toFixnum(data[0][0].length),
                              Data.toFixnum(data[0][0][0].length));
        }
//        void fill(Object q) {
//            q = checkValue(q);
//            Object dims = dims();
//            int d0 = Data.fixnum(Lists.car(dims)).intValue();
//            dims = Lists.cdr(dims);
//            int d1 = Data.fixnum(Lists.car(dims)).intValue();
//            dims = Lists.cdr(dims);
//            int d2 = Data.fixnum(Lists.car(dims)).intValue();
//            dims = Lists.cdr(dims);
//            int d3 = Data.fixnum(Lists.car(dims)).intValue();
//            for (int i0 = 0; i0 < d0; i0++)
//                for (int i1 = 0; i1 < d1; i1++)
//                    for (int i2 = 0; i2 < d2; i2++)
//                        for (int i3 = 0; i3 < d3; i3++)
//                            data[i0][i1][i2][i3] = q;
//        }
    }/* end of DIM4 */

    static class DIM5 extends Array implements Callable5 {
        final Object[][][][][] data;
        DIM5(String name, Symbol type,
             int d0, int d1, int d2, int d3, int d4) {
            super(name, type);
            if (d0 <= 0)
                throw new TypeException
                    ("d0 must be positive: "+d0, Symbols.NIL);
            if (d1 <= 0)
                throw new TypeException
                    ("d1 must be positive: "+d1, Symbols.NIL);
            if (d2 <= 0)
                throw new TypeException
                    ("d2 must be positive: "+d2, Symbols.NIL);
            if (d3 <= 0)
                throw new TypeException
                    ("d3 must be positive: "+d3, Symbols.NIL);
            if (d4 <= 0)
                throw new TypeException
                    ("d4 must be positive: "+d4, Symbols.NIL);
            data = new Object[d0][d1][d2][d3][d4];
            Object q = initValue();
            for (int i0 = 0; i0 < d0; i0++)
                for (int i1 = 0; i1 < d1; i1++)
                    for (int i2 = 0; i2 < d2; i2++)
                        for (int i3 = 0; i3 < d3; i3++)
                            for (int i4 = 0; i4 < d4; i4++)
                                data[i0][i1][i2][i3][i4] = q;
        }
        public Object call5(Object arg0, Object arg1, Object arg2,
                            Object arg3, Object arg4,Env env) {
            int i0 = Data.fixnum(arg0).intValue();
            int i1 = Data.fixnum(arg1).intValue();
            int i2 = Data.fixnum(arg2).intValue();
            int i3 = Data.fixnum(arg3).intValue();
            int i4 = Data.fixnum(arg4).intValue();
            return data[i0][i1][i2][i3][i4];
        }
        public Object dims() {
            return Lists.list(Data.toFixnum(data.length),
                              Data.toFixnum(data[0].length),
                              Data.toFixnum(data[0][0].length),
                              Data.toFixnum(data[0][0][0].length),
                              Data.toFixnum(data[0][0][0][0].length));
        }
//        void fill(Object q) {
//            q = checkValue(q);
//            Object dims = dims();
//            int d0 = Data.fixnum(Lists.car(dims)).intValue();
//            dims = Lists.cdr(dims);
//            int d1 = Data.fixnum(Lists.car(dims)).intValue();
//            dims = Lists.cdr(dims);
//            int d2 = Data.fixnum(Lists.car(dims)).intValue();
//            dims = Lists.cdr(dims);
//            int d3 = Data.fixnum(Lists.car(dims)).intValue();
//            dims = Lists.cdr(dims);
//            int d4 = Data.fixnum(Lists.car(dims)).intValue();
//            for (int i0 = 0; i0 < d0; i0++)
//                for (int i1 = 0; i1 < d1; i1++)
//                    for (int i2 = 0; i2 < d2; i2++)
//                        for (int i3 = 0; i3 < d3; i3++)
//                            for (int i4 = 0; i4 < d4; i4++)
//                                data[i0][i1][i2][i3][i4] = q;
//        }
    }/* end of DIM5 */

}/* end of Array */
