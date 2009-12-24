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
 * A primitive data structure which has pname.
 * @see Package
 */
public class Symbol implements Prop {
    /**
     * generates a uninterned symbol with specified name.
     */
    static public Symbol gensym(String name) {
        return new Symbol(null, name);
    }
    ///**
    // * Returns true if o is a keyword and false otherwise. 
    // */
    //static public boolean isKeyword(Object sym) {
    //    return Data.isSymbol(sym)
    //        && Data.symbol(sym).pkg() == Package.KEYWORD;
    //}
    ///**
    // * Returns true if o is a regular (interned) symbol
    // * and false otherwise.
    // */
    //static public boolean isInternedSymbol(Object sym) {
    //    return Data.isSymbol(sym)
    //        && Data.symbol(sym).pkg() != null;
    //}

    /** home package of this symbol */
    private final Package pkg;
    /** Print name of this symbol */
    private final String pname;
    /** Is this symbol selfeval ? */
    private final boolean selfeval;

    Symbol(Package pkg, String pname, boolean selfeval) {
        this.pkg = pkg;
        this.pname = pname;
        this.selfeval = selfeval;
    }
    Symbol(Package pkg, String pname) {
        this(pkg, pname, false);
    }
    public final Package pkg() {
        return pkg;
    }
    public final String pname() {
        return pname;
    }
    public final boolean isSelfeval() {
        return selfeval;
    }
    public final boolean equals(Object o) {
        return super.equals(o);
    }
    public final int hashCode() {
        return super.hashCode();
    }
    public String toString() {
        return super.toString()+"["+pkg()+":"+pname()+":"+selfeval+"]";
    }
}
