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
 * Maclisp HUNK. 
 * Note that this code is nothing but a stub for the rabbit
 * compiler (in AITR-474) to run under this lisp system.
 */
public class Hunk {
    private final Object[] data;
    Hunk(int size) {
        if (size < 1)
            throw new TypeException
                ("size must be positive: "+size, Symbols.NIL);
        data = new Object[size];
        for (int i = 0; i < size; i++)
            data[i] = Symbols.NIL;
    }
    public int length() {
        return data.length;
    }
    public Object cxr(int i) {
        return data[i];
    }
    public Hunk rplacx(int i, Object o) {
        data[i] = o;
        return this;
    }
    public List toList() {
        return Lists.toList(data);
    }
}

