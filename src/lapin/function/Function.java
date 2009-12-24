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

/**
 * Base class for a function objects.
 */
public abstract class Function {
    /** function name of the function. */
    private final String name;
    /**
     * Constructs a function with the specified name.
     * @param name Name for this function
     * @throws NullPointerException <code>name</code> is null
     */
    protected Function(String name) {
        if (name == null)
            throw new NullPointerException("name is null");
        this.name = name;
    }
    /** Returns the name of this function. */
    public final String name() {
        return name;
    }
} /* end of Function */
