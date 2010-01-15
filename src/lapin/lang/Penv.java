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
import lapin.function.Function;
import lapin.io.Printer;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;

/**
 * Repository for property list.
 * @see Prop
 */
public class Penv {
    // uninterned symbol: for private use only
    static private final Symbol HEAD = Symbol.gensym("_PLIST-HEAD_");

    private final HashMap plistMap = new HashMap();

    public /*synchronized*/ Object getPlist(Prop prop, boolean create) {
        Object plst = plistMap.get(prop);
        if (plst == null && create) {
            // create & put new disembodied plist for prop
            plst = Lists.list(HEAD);
            plistMap.put(prop, plst);
        }
        return plst;
    }
    public void dump(Object stream, Env env) {
        Set keySet = plistMap.keySet();
        for (Iterator i = keySet.iterator(); i.hasNext();) {
            Object key = i.next();
            Object val = Lists.cdr(plistMap.get(key));
            Printer.format("~S = ~S~%", Lists.list(key, val), stream, env);
        }
        Printer.terpri(stream, env);
    }
}
