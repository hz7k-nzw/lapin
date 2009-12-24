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
 * This interface is implemented by {@link Subr}.
 * When a {@link Subr} needs argument destructuring or
 * {@link LambdaList} which satisfies
 * <code>
 * {@link LambdaList#reqCount reqCount} +
 * {@link LambdaList#optCount optCount} &gt; 5
 * </code>,
 * it must implements this interface.
 * @see Subr
 * @see LambdaList
 */
public interface Callable {
    /**
     * Calls this function with the specified arguments.
     * @param args {@link lapin.lang.List} of arguments, which may be nested
     * @param env
     * @return Result of the function application
     * @throws java.lang.Exception Any exception thrown by this function
     */
    Object call(Object args, Env env) throws Exception;
}
