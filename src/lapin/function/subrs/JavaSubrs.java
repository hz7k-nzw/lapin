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
package lapin.function.subrs;
import lapin.function.Subr;
import lapin.function.SystemSubr;

public final class JavaSubrs {
    static public final Subr STRING_CHAR_AT
        = SystemSubr.toSubr1("STRING-CHAR-AT",String.class,
                             "charAt",int.class);
    static public final Subr STRING_CONCAT
        = SystemSubr.toSubr1("STRING-CONCAT",String.class,
                             "concat",String.class);
    static public final Subr STRING_ENDS_WITH
        = SystemSubr.toSubr1("STRING-ENDS-WITH",String.class,
                             "endsWith",String.class);
    static public final Subr STRING_INDEX_OF
        = SystemSubr.toSubr1("STRING-INDEX-OF",String.class,
                             "indexOf",String.class);
    static public final Subr STRING_LAST_INDEX_OF
        = SystemSubr.toSubr1("STRING-LAST-INDEX-OF",String.class,
                             "lastIndexOf",String.class);
    static public final Subr STRING_LENGTH
        = SystemSubr.toSubr0("STRING-LENGTH",String.class,"length");
    static public final Subr STRING_REPLACE
        = SystemSubr.toSubr2("STRING-REPLACE",String.class,
                             "replace",char.class,char.class);
    static public final Subr STRING_STARTS_WITH
        = SystemSubr.toSubr1("STRING-STARTS-WITH",String.class,
                             "startsWith",String.class);
    static public final Subr STRING_SUBSTRING
        = SystemSubr.toSubr2("STRING-SUBSTRING",String.class,
                             "substring",int.class,int.class);

    static public final Subr STRING_MATCHES
        = SystemSubr.toSubr1("STRING-MATCHES",String.class,
                             "matches",String.class);
    static public final Subr STRING_REPLACE_ALL
        = SystemSubr.toSubr2("STRING-REPLACE-ALL",String.class,
                             "replaceAll",String.class,String.class);
    static public final Subr STRING_REPLACE_FIRST
        = SystemSubr.toSubr2("STRING-REPLACE-FIRST",String.class,
                             "replaceFirst",String.class,String.class);

    private JavaSubrs() {}
}
