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

/** Predefined symbols and some operations on those symbols. */
public final class SysSymbols {
    /** Interned symbol whose pname is "C-HANDLER". */
    static public final Symbol C_HANDLER
        = Package.SYS.intern("C-HANDLER");
    /** Interned symbol whose pname is "C-HANDLER-CONV-FORM". */
    static public final Symbol C_HANDLER_CONV_FORM
        = Package.SYS.intern("C-HANDLER-CONV-FORM");
    /** Interned symbol whose pname is "C-HANDLER-BUILD-INSTS". */
    static public final Symbol C_HANDLER_BUILD_INSTS
        = Package.SYS.intern("C-HANDLER-BUILD-INSTS");
    /** Interned symbol whose pname is "LOAD-SUBRS". */
    static public final Symbol LOAD_SUBRS
        = Package.SYS.intern("LOAD-SUBRS");
    /** Interned symbol whose pname is "SUBR-FIELD". */
    static public final Symbol SUBR_FIELD
        = Package.SYS.intern("SUBR-FIELD");

    /** Interned symbol whose pname is "*BYTE-CODE-GENERATOR-CLASSNAME*". */
    static public final Symbol BYTE_CODE_GENERATOR_CLASSNAME
        = Package.SYS.intern("*BYTE-CODE-GENERATOR-CLASSNAME*");
    /** Interned symbol whose pname is "*CLASS-LOADER*". */
    static public final Symbol CLASS_LOADER
        = Package.SYS.intern("*CLASS-LOADER*");
    /** Interned symbol whose pname is "*FUN-ID*". */
    static public final Symbol FUN_ID
        = Package.SYS.intern("*FUN-ID*");

    private SysSymbols() {}
}
