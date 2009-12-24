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
 * This interface is implemented by objects classified as LIST.
 */
public interface List {
    /** Returns the <code>car</code> of this list. */
    Object car();
    /** Returns the <code>cdr</code> of this list. */
    Object cdr();
    /** Returns true when this list is <code>NIL</code>. */
    boolean isEnd();
    /** Returns the length of this list. */
    int length();
}
