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
import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * Operations for numbers.
 */
public final class Numbers {
    /** FIXNUM object for {@link Math#E}. */
    static public final Double E = new Double(Math.E);
    /** FIXNUM object for {@link Math#PI}. */
    static public final Double PI = new Double(Math.PI);

    static private Object numtype(Object o) {
        if (Data.isFixnum(o))
            return Symbols.FIXNUM;
        if (Data.isFlonum(o))
            return Symbols.FLONUM;
        if (Data.isBignum(o))
            return Symbols.BIGNUM;
        throw new TypeException
            ("unacceptable numtype: ~S.", Lists.list(o));
    }
    static private Object calcNumtype(Object o1, Object o2) {
        Object t1 = numtype(o1);
        Object t2 = numtype(o2);
        if (t1 == Symbols.FIXNUM) {
            return t2;
        }
        else if (t1 == Symbols.FLONUM) {
            return t1;
        }
        else { /* type of o1 is bignum */
            return t2 == Symbols.FLONUM ? t2 : t1;
        }
    }
    //static private Object calcNumtype(Object o1, Object o2) {
    //    Object t1 = Data.typep(o1);
    //    Object t2 = Data.typep(o2);
    //    int comp;
    //    if (t1 == Symbols.FIXNUM) {
    //        if (t2 == Symbols.FIXNUM)
    //            comp = 0;
    //        else if (t2 == Symbols.BIGNUM)
    //            comp = -1;
    //        else if (t2 == Symbols.FLONUM)
    //            comp = -1;
    //        else
    //            throw new IllegalArgumentException
    //                ("unacceptable numtype: "+o2+"("+t2+")");
    //    }
    //    else if (t1 == Symbols.BIGNUM) {
    //        if (t2 == Symbols.FIXNUM)
    //            comp = +1;
    //        else if (t2 == Symbols.BIGNUM)
    //            comp = 0;
    //        else if (t2 == Symbols.FLONUM)
    //            comp = -1;
    //        else
    //            throw new IllegalArgumentException
    //                ("unacceptable numtype: "+o2+"("+t2+")");
    //    }
    //    else if (t1 == Symbols.FLONUM) {
    //        if (t2 == Symbols.FIXNUM)
    //            comp = +1;
    //        else if (t2 == Symbols.BIGNUM)
    //            comp = +1;
    //        else if (t2 == Symbols.FLONUM)
    //            comp = 0;
    //        else
    //            throw new IllegalArgumentException
    //                ("unacceptable numtype: "+o2+"("+t2+")");
    //    }
    //    else {
    //        throw new IllegalArgumentException
    //            ("unacceptable numtype: "+o1+"("+t1+")");
    //    }
    //    return comp > 0 ? t1 : t2;
    //}
    static private Number toFixOrBignum(long l) {
        int i = (int) l;
        if (i == l)
            return Data.toFixnum(i);
        else
            return BigInteger.valueOf(l);
    }
    static private Number toFixOrBignum(BigInteger bn) {
        if (bn.bitLength() < 32)
            return Data.toFixnum(bn.intValue());
        else
            return bn;
    }
    //// float
    //static public Double flo(Object o) {
    //    return Conv.toFlonum(o);
    //}
    // fix
    static public Number fix(Object o) {
        // expected:
        //  (fix 7.3) => 7
        //  (fix -1.2) => -2
        if (Data.isFixnum(o))
            return Data.fixnum(o);
        else if (Data.isBignum(o))
            return Data.bignum(o);
        else {
            double d = Math.floor(Data.flonum(o).doubleValue());
            return toFixOrBignum(new BigDecimal(d).toBigInteger());
        }
    }
    // ifix
    static public Number ifix(Object o) {
        // expected:
        // (ifix  3.0E5) =>  300000.
        // (ifix -250.3) => -251.
        // (ifix 5) => 5.
        if (Data.isFixnum(o))
            return Data.fixnum(o);
        else if (Data.isBignum(o))
            throw new TypeException
                ("bignum not supported: ~S.", Lists.list(o));
        else {
            double d = Math.floor(Data.flonum(o).doubleValue());
            return Data.toFixnum((int) d);
        }
    }
    // numberp
    static public boolean isNumber(Object o) {
        return Data.isFixnum(o) || Data.isFlonum(o) || Data.isBignum(o);
    }
    // fixnump -> Data.isFixnum(Object)
    // bigp -> Data.isBignum(Object)
    // fixp
    static public boolean isFix(Object o) {
        return Data.isFixnum(o) || Data.isBignum(o);
    }
    // floatp -> Data.isFlonum(Object)
    // plus
    static public Number add(Object o1, Object o2) {
        Object nt = calcNumtype(o1, o2);
        if (nt == Symbols.FIXNUM) {
            long n1 = Conv.toFixnum(o1).longValue();
            long n2 = Conv.toFixnum(o2).longValue();
            return toFixOrBignum(n1+n2);
        }
        else if (nt == Symbols.BIGNUM) {
            BigInteger n1 = Conv.toBignum(o1);
            BigInteger n2 = Conv.toBignum(o2);
            return toFixOrBignum(n1.add(n2));
        }
        else {
            double n1 = Conv.toFlonum(o1).doubleValue();
            double n2 = Conv.toFlonum(o2).doubleValue();
            return Data.toFlonum(n1+n2);
        }
    }
    // +
    //static public Integer addFixnum(Object o1, Object o2) {
    //    int n1 = Data.fixnum(o1).intValue();
    //    int n2 = Data.fixnum(o2).intValue();
    //    return Data.toFixnum(n1+n2);
    //}
    static public int addInt(int n1, int n2) {
        return n1 + n2;
    }
    // +$
    //static public Double addFlonum(Object o1, Object o2) {
    //    double n1 = Data.flonum(o1).doubleValue();
    //    double n2 = Data.flonum(o2).doubleValue();
    //    return Data.toFlonum(n1+n2);
    //}
    static public double addDouble(double n1, double n2) {
        return n1 + n2;
    }
    // add1 -> add + 1
    static public Number add1(Object r0) {
        return add(r0, Data.toFixnum(1));
    }
    // 1+ -> addFixnum + 1
    //static public Integer add1Fixnum(Object r0) {
    //    return addFixnum(r0, Data.toFixnum(1));
    //}
    static public int add1Int(int n) {
        return n + 1;
    }
    // 1+$ -> addFlonum + 1.0
    //static public Double add1Flonum(Object r0) {
    //    return addFlonum(r0, Data.toFlonum(1.0));
    //}
    static public double add1Double(double n) {
        return n + 1.0;
    }
    // difference
    static public Number sub(Object o1, Object o2) {
        Object nt = calcNumtype(o1, o2);
        if (nt == Symbols.FIXNUM) {
            long n1 = Conv.toFixnum(o1).longValue();
            long n2 = Conv.toFixnum(o2).longValue();
            return toFixOrBignum(n1-n2);
        }
        else if (nt == Symbols.BIGNUM) {
            BigInteger n1 = Conv.toBignum(o1);
            BigInteger n2 = Conv.toBignum(o2);
            return toFixOrBignum(n1.subtract(n2));
        }
        else {
            double n1 = Conv.toFlonum(o1).doubleValue();
            double n2 = Conv.toFlonum(o2).doubleValue();
            return Data.toFlonum(n1-n2);
        }
    }
    // minus
    static public Number negate(Object o) {
        if (Data.isFixnum(o)) {
            int n = Data.fixnum(o).intValue();
            return Data.toFixnum(-n);
        }
        else if (Data.isBignum(o)) {
            BigInteger n = Data.bignum(o);
            return n.negate();
        }
        else {
            double n = Data.flonum(o).doubleValue();
            return Data.toFlonum(-n);
        }
    }
    static public int negateInt(int n) {
        return -n;
    }
    static public double negateDouble(double n) {
        return -n;
    }
    // -
    //static public Integer subFixnum(Object o1, Object o2) {
    //    int n1 = Data.fixnum(o1).intValue();
    //    int n2 = Data.fixnum(o2).intValue();
    //    return Data.toFixnum(n1-n2);
    //}
    static public int subInt(int n1, int n2) {
        return n1 - n2;
    }
    // -$
    //static public Double subFlonum(Object o1, Object o2) {
    //    double n1 = Data.flonum(o1).doubleValue();
    //    double n2 = Data.flonum(o2).doubleValue();
    //    return Data.toFlonum(n1-n2);
    //}
    static public double subDouble(double n1, double n2) {
        return n1 - n2;
    }
    // sub1 -> sub - 1
    static public Number sub1(Object r0) {
        return sub(r0, Data.toFixnum(1));
    }
    // 1- -> subFixnum - 1
    //static public Integer sub1Fixnum(Object r0) {
    //    return subFixnum(r0, Data.toFixnum(1));
    //}
    static public int sub1Int(int n) {
        return n - 1;
    }
    // 1-$ -> subFlonum - 1.0
    //static public Double sub1Flonum(Object r0) {
    //    return subFlonum(r0, Data.toFlonum(1.0));
    //}
    static public double sub1Double(double n) {
        return n - 1.0;
    }
    // times
    static public Number mul(Object o1, Object o2) {
        Object nt = calcNumtype(o1, o2);
        if (nt == Symbols.FIXNUM) {
            long n1 = Conv.toFixnum(o1).longValue();
            long n2 = Conv.toFixnum(o2).longValue();
            return toFixOrBignum(n1*n2);
        }
        else if (nt == Symbols.BIGNUM) {
            BigInteger n1 = Conv.toBignum(o1);
            BigInteger n2 = Conv.toBignum(o2);
            return toFixOrBignum(n1.multiply(n2));
        }
        else {
            double n1 = Conv.toFlonum(o1).doubleValue();
            double n2 = Conv.toFlonum(o2).doubleValue();
            return Data.toFlonum(n1*n2);
        }
    }
    // *
    //static public Integer mulFixnum(Object o1, Object o2) {
    //    int n1 = Data.fixnum(o1).intValue();
    //    int n2 = Data.fixnum(o2).intValue();
    //    return Data.toFixnum(n1*n2);
    //}
    static public int mulInt(int n1, int n2) {
        return n1 * n2;
    }
    // *$
    //static public Double mulFlonum(Object o1, Object o2) {
    //    double n1 = Data.flonum(o1).doubleValue();
    //    double n2 = Data.flonum(o2).doubleValue();
    //    return Data.toFlonum(n1*n2);
    //}
    static public double mulDouble(double n1, double n2) {
        return n1 * n2;
    }
    // quotient
    static public Number div(Object o1, Object o2) {
        Object nt = calcNumtype(o1, o2);
        try {
            if (nt == Symbols.FIXNUM) {
                long n1 = Conv.toFixnum(o1).longValue();
                long n2 = Conv.toFixnum(o2).longValue();
                return toFixOrBignum(n1/n2);
            }
            else if (nt == Symbols.BIGNUM) {
                BigInteger n1 = Conv.toBignum(o1);
                BigInteger n2 = Conv.toBignum(o2);
                return toFixOrBignum(n1.divide(n2));
            }
            else {
                double n1 = Conv.toFlonum(o1).doubleValue();
                double n2 = Conv.toFlonum(o2).doubleValue();
                return Data.toFlonum(n1/n2);
            }
        }
        catch (java.lang.ArithmeticException e) {
            throw new lapin.lang.ArithmeticException
                ("div error: "+e.getMessage(), Symbols.NIL);
        }
    }
    // remainder
    static public Number mod(Object o1, Object o2) {
        Object nt = calcNumtype(o1, o2);
        try {
            if (nt == Symbols.FIXNUM) {
                long n1 = Conv.toFixnum(o1).longValue();
                long n2 = Conv.toFixnum(o2).longValue();
                return toFixOrBignum(n1%n2);
            }
            else if (nt == Symbols.BIGNUM) {
                BigInteger n1 = Conv.toBignum(o1);
                BigInteger n2 = Conv.toBignum(o2);
                return toFixOrBignum(n1.mod(n2));
            }
            else {
                throw new TypeException
                    ("mod for flonum not supported: ~S ~S",
                     Lists.list(o1, o2));
            }
        }
        catch (java.lang.ArithmeticException e) {
            throw new lapin.lang.ArithmeticException
                ("mod error: "+e.getMessage(), Symbols.NIL);
        }
    }
    // //
    //static public Integer divFixnum(Object o1, Object o2) {
    //    int n1 = Data.fixnum(o1).intValue();
    //    int n2 = Data.fixnum(o2).intValue();
    //    return Data.toFixnum(n1/n2);
    //}
    static public int divInt(int n1, int n2) {
        try {
            return n1 / n2;
        }
        catch (java.lang.ArithmeticException e) {
            throw new lapin.lang.ArithmeticException
                ("div error: "+e.getMessage(), Symbols.NIL);
        }
    }
    // //$
    //static public Double divFlonum(Object o1, Object o2) {
    //    double n1 = Data.flonum(o1).doubleValue();
    //    double n2 = Data.flonum(o2).doubleValue();
    //    return Data.toFlonum(n1/n2);
    //}
    static public double divDouble(double n1, double n2) {
        return n1 / n2;
    }
    // \
    //static public Integer modFixnum(Object o1, Object o2) {
    //    int n1 = Data.fixnum(o1).intValue();
    //    int n2 = Data.fixnum(o2).intValue();
    //    return Data.toFixnum(n1%n2);
    //}
    static public int modInt(int n1, int n2) {
        try {
            return n1 % n2;
        }
        catch (java.lang.ArithmeticException e) {
            throw new lapin.lang.ArithmeticException
                ("mod error: "+e.getMessage(), Symbols.NIL);
        }
    }
    // 1//$ (not in maclisp)
    //static public Double inverseFlonum(Object o1) {
    //    double n1 = Data.flonum(o1).doubleValue();
    //    return Data.toFlonum(1/n2);
    //}
    static public double inverseDouble(double n) {
        return 1 / n;
    }

    static private int compare(Object o1, Object o2) {
        Object nt = calcNumtype(o1, o2);
        if (nt == Symbols.FIXNUM) {
            int n1 = Conv.toFixnum(o1).intValue();
            int n2 = Conv.toFixnum(o2).intValue();
            return n1 < n2 ? -1 : n1 == n2 ? 0 : +1;
        }
        else if (nt == Symbols.BIGNUM) {
            BigInteger n1 = Conv.toBignum(o1);
            BigInteger n2 = Conv.toBignum(o2);
            return n1.compareTo(n2);
        }
        else {
            double n1 = Conv.toFlonum(o1).doubleValue();
            double n2 = Conv.toFlonum(o2).doubleValue();
            return n1 < n2 ? -1 : n1 == n2 ? 0 : +1;
        }
    }
    static private int compareFixOrFlonum(Object o1, Object o2) {
        Object nt = calcNumtype(o1, o2);
        if (nt == Symbols.FIXNUM) {
            int n1 = Data.fixnum(o1).intValue();
            int n2 = Data.fixnum(o2).intValue();
            return n1 < n2 ? -1 : n1 == n2 ? 0 : +1;
        }
        else if (nt == Symbols.BIGNUM) {
            throw new TypeException
                ("bignum not supported: ~S ~S", Lists.list(o1, o2));
        }
        else {
            double n1 = Data.flonum(o1).doubleValue();
            double n2 = Data.flonum(o2).doubleValue();
            return n1 < n2 ? -1 : n1 == n2 ? 0 : +1;
        }
    }

    // equal -> Data.equal(Object, Object)
    // equalp
    static public boolean isEq(Object o1, Object o2) {
        return compare(o1, o2) == 0;
    }
    // =
    static public boolean isEqFixOrFlonum(Object o1, Object o2) {
        return compareFixOrFlonum(o1, o2) == 0;
    }
    static public boolean isEqInt(int n1, int n2) {
        return n1 == n2;
    }
    static public boolean isEqDouble(double n1, double n2) {
        return n1 == n2;
    }
    // greaterp
    static public boolean isGt(Object o1, Object o2) {
        return compare(o1, o2) > 0;
    }
    // >
    static public boolean isGtFixOrFlonum(Object o1, Object o2) {
        return compareFixOrFlonum(o1, o2) > 0;
    }
    static public boolean isGtInt(int n1, int n2) {
        return n1 > n2;
    }
    static public boolean isGtDouble(double n1, double n2) {
        return n1 > n2;
    }
    // lessp
    static public boolean isLt(Object o1, Object o2) {
        return compare(o1, o2) < 0;
    }
    // <
    static public boolean isLtFixOrFlonum(Object o1, Object o2) {
        return compareFixOrFlonum(o1, o2) < 0;
    }
    static public boolean isLtInt(int n1, int n2) {
        return n1 < n2;
    }
    static public boolean isLtDouble(double n1, double n2) {
        return n1 < n2;
    }
    // greater-or-equalp (not in maclisp)
    static public boolean isGe(Object o1, Object o2) {
        return compare(o1, o2) >= 0;
    }
    // >= (not in maclisp)
    static public boolean isGeFixOrFlonum(Object o1, Object o2) {
        return compareFixOrFlonum(o1, o2) >= 0;
    }
    static public boolean isGeInt(int n1, int n2) {
        return n1 >= n2;
    }
    static public boolean isGeDouble(double n1, double n2) {
        return n1 >= n2;
    }
    // less-or-equalp (not in maclisp)
    static public boolean isLe(Object o1, Object o2) {
        return compare(o1, o2) <= 0;
    }
    // <= (not in maclisp)
    static public boolean isLeFixOrFlonum(Object o1, Object o2) {
        return compareFixOrFlonum(o1, o2) <= 0;
    }
    static public boolean isLeInt(int n1, int n2) {
        return n1 <= n2;
    }
    static public boolean isLeDouble(double n1, double n2) {
        return n1 <= n2;
    }

    // plusp
    static public boolean isPlus(Object o) {
        boolean f;
        if (Data.isFixnum(o))
            f = Data.fixnum(o).intValue() > 0;
        else if (Data.isBignum(o))
            f = Data.bignum(o).compareTo(BigInteger.ZERO) > 0;
        else
            f = Data.flonum(o).doubleValue() > 0.0;
        return f;
    }
    // zerop
    static public boolean isZero(Object o) {
        boolean f;
        if (Data.isFixnum(o))
            f = Data.fixnum(o).intValue() == 0;
        else if (Data.isBignum(o))
            f = Data.bignum(o).equals(BigInteger.ZERO);
        else
            f = Data.flonum(o).doubleValue() == 0.0;
        return f;
    }
    // minusp
    static public boolean isMinus(Object o) {
        boolean f;
        if (Data.isFixnum(o))
            f = Data.fixnum(o).intValue() < 0;
        else if (Data.isBignum(o))
            f = Data.bignum(o).compareTo(BigInteger.ZERO) < 0;
        else
            f = Data.flonum(o).doubleValue() < 0.0;
        return f;
    }
    // abs
    static public Number abs(Object o) {
        if (Data.isFixnum(o))
            return Data.toFixnum(Math.abs(Data.fixnum(o).intValue()));
        else if (Data.isBignum(o))
            return Data.bignum(o).abs();
        else
            return Data.toFlonum(Math.abs(Data.flonum(o).doubleValue()));
    }
    // signp (fsubr) -> x
    // gcd (bignum calculation only)
    static public Number gcd(Object o1, Object o2) {
        BigInteger n1 = Conv.toBignum(o1);
        BigInteger n2 = Conv.toBignum(o2);
        return n1.gcd(n2);
    }
    // \\ -> x
    // oddp
    static public boolean isOdd(Object o) {
        return !isEven(o);
    }
    // evenp (not in maclisp)
    static public boolean isEven(Object o) {
        boolean f;
        if (Data.isFixnum(o))
            f = Data.fixnum(o).intValue() %2 == 0;
        else if (Data.isBignum(o))
            f = Data.bignum(o)
                .mod(BigInteger.valueOf(2L))
                .equals(BigInteger.ZERO);
        else
            throw new TypeException
                ("evenp/oddp for flonum not supported: ~S.",
                 Lists.list(o));
        return f;
    }
    // max
    static public Number max(Object o1, Object o2) {
        Object nt = calcNumtype(o1, o2);
        if (nt == Symbols.FIXNUM) {
            int n1 = Conv.toFixnum(o1).intValue();
            int n2 = Conv.toFixnum(o2).intValue();
            return Data.toFixnum(Math.max(n1,n2));
        }
        else if (nt == Symbols.BIGNUM) {
            BigInteger n1 = Conv.toBignum(o1);
            BigInteger n2 = Conv.toBignum(o2);
            return n1.max(n2);
        }
        else {
            double n1 = Conv.toFlonum(o1).doubleValue();
            double n2 = Conv.toFlonum(o2).doubleValue();
            return Data.toFlonum(Math.max(n1,n2));
        }
    }
    // min
    static public Number min(Object o1, Object o2) {
        Object nt = calcNumtype(o1, o2);
        if (nt == Symbols.FIXNUM) {
            int n1 = Conv.toFixnum(o1).intValue();
            int n2 = Conv.toFixnum(o2).intValue();
            return Data.toFixnum(Math.min(n1,n2));
        }
        else if (nt == Symbols.BIGNUM) {
            BigInteger n1 = Conv.toBignum(o1);
            BigInteger n2 = Conv.toBignum(o2);
            return n1.min(n2);
        }
        else {
            double n1 = Conv.toFlonum(o1).doubleValue();
            double n2 = Conv.toFlonum(o2).doubleValue();
            return Data.toFlonum(Math.min(n1,n2));
        }
    }

    // EXPT (flonum calculation only)
    static public double pow(Object o1, Object o2) {
        double n1 = Data.javaNumber(o1).doubleValue();
        double n2 = Data.javaNumber(o2).doubleValue();
        return Math.pow(n1,n2);
    }
    // ^ -> x
    // ^$ -> x
    // exp
    static public double exp(Object o) {
        double n = Data.javaNumber(o).doubleValue();
        return Math.exp(n);
    }
    // sqrt
    static public double sqrt(Object o) {
        double n = Data.javaNumber(o).doubleValue();
        return Math.sqrt(n);
    }
    // log
    static public double log(Object o) {
        double n = Data.javaNumber(o).doubleValue();
        return Math.log(n);
    }
    // acos (not defined in maclisp)
    static public double acos(Object o) {
        double n = Data.javaNumber(o).doubleValue();
        return Math.acos(n);
    }
    // asin (not defined in maclisp)
    static public double asin(Object o) {
        double n = Data.javaNumber(o).doubleValue();
        return Math.asin(n);
    }
    // atan (not defined in maclisp)
    static public double atan(Object o) {
        double n = Data.javaNumber(o).doubleValue();
        return Math.atan(n);
    }
    // atan
    static public double atan2(Object o1, Object o2) {
        double n1 = Data.javaNumber(o1).doubleValue();
        double n2 = Data.javaNumber(o2).doubleValue();
        return Math.atan2(n1,n2);
    }
    // cos
    static public double cos(Object o) {
        double n = Data.javaNumber(o).doubleValue();
        return Math.cos(n);
    }
    // sin
    static public double sin(Object o) {
        double n = Data.javaNumber(o).doubleValue();
        return Math.sin(n);
    }
    // tan (not defined in maclisp)
    static public double tan(Object o) {
        double n = Data.javaNumber(o).doubleValue();
        return Math.tan(n);
    }
    // random
    //static public Double random() {
    //    return Data.toFlonum(Math.random());
    //}

    private Numbers() {}
}
