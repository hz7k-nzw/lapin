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
import lapin.lang.Env;

/**
 * This interface is implemented by {@link Subr}, and
 * defines a function which has 5 (req or opt) parameters and
 * rest and/or keyword parameters
 * @see Subr
 */
public interface Callable5r {
    /**
     * Calls this function with the specified arguments
     * <code>arg0, arg1, arg2, arg3, arg4 and rest</code>.
     * @param arg0 Argument
     * @param arg1 Argument
     * @param arg2 Argument
     * @param arg3 Argument
     * @param arg4 Argument
     * @param rest Rest of arguments, {@link lapin.lang.List} is expected
     * @param env
     * @return Result of the function application
     * @throws java.lang.Exception Any exception thrown by this function
     */
    Object call5(Object arg0, Object arg1, Object arg2,
                 Object arg3, Object arg4, Object rest,
                 Env env) throws Exception;
}
