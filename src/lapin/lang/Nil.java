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
 * Symbol that denotes both "false" and "empty list". 
 */
public final class Nil extends Symbol implements lapin.lang.List {
    Nil() {
        super(Package.LISP, "NIL", true);
    }
    public Object car() {
        return this;
    }
    public Object cdr() {
        return this;
    }
    public boolean isEnd() {
        return true;
    }
    public int length() {
        return 0;
    }
}
