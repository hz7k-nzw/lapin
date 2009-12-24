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
 * A primitive data structure capable of holding exactly two things.
 */
public final class Pair implements lapin.lang.List {
    /** car of this pair */
    private Object car;
    /** cdr of this pair */
    private Object cdr;

    Pair(Object car, Object cdr) {
        this.car = car;
        this.cdr = cdr;
    }
    public Object car() {
        return car;
    }
    public Object cdr() {
        return cdr;
    }
    public boolean isEnd() {
        return false;
    }
    public int length() {
        int len = 0;
        List l = this;
        while (!l.isEnd()) {
            len++;
            l = Data.list(l.cdr());
        }
        return len;
    }
    public Pair rplaca(Object o) {
        car = o;
        return this;
    }
    public Pair rplacd(Object o) {
        cdr = o;
        return this;
    }
    public boolean equals(Object o) {
        if (o == this)
            return true;
        else if ((o == null) || !(o instanceof Pair))
            return false;
        else {
            Pair that = (Pair) o;
            return Data.isEqual(this.car, that.car)
                && Data.isEqual(this.cdr, that.cdr);
        }
    }
    public int hashCode() {
        int hcar = Data.hashEqual(car);
        int hcdr = Data.hashEqual(cdr);
        return hcar ^ hcdr;
    }
}
