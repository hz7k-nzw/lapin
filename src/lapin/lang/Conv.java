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
import java.math.BigInteger;

/** Operations for coercion. */
public final class Conv {
    static public Integer toFixnum(Object o) {
        if (Data.isFixnum(o))
            return Data.fixnum(o);
        else
            return Data.toFixnum(Data.javaNumber(o).intValue());
    }
    static public BigInteger toBignum(Object o) {
        if (Data.isBignum(o))
            return Data.bignum(o);
        else
            return BigInteger.valueOf(Data.javaNumber(o).longValue());
    }
    static public Double toFlonum(Object o) {
        if (Data.isFlonum(o))
            return Data.flonum(o);
        else
            return Data.toFlonum(Data.javaNumber(o).doubleValue());
    }
    static public Character toCharacter(Object o) {
        if (Data.isCharacter(o))
            return Data.character(o);
        else
            return Data.toCharacter((char) Data.fixnum(o).intValue());
    }
    static public int toInt(Object o) {
        return toFixnum(o).intValue();
    }
    static public int toInt(double d) {
        return (int) d;
    }
    static public int toInt(char c) {
        return (int) c;
    }
    static public double toDouble(Object o) {
        return toFlonum(o).doubleValue();
    }
    static public double toDouble(int i) {
        return (double) i;
    }
    static public char toChar(Object o) {
        return toCharacter(o).charValue();
    }
    static public char toChar(int i) {
        return (char) i;
    }
    private Conv() {}
}
