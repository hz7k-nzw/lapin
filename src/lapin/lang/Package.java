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
import java.util.HashMap;

/**
 * Name space of symbols.
 * This class manages mapping of (name, package) and (name, symbol),
 * which can be shared by every lisp instance on the same JVM.
 * Actual state of packages, which is affected by operations on package
 * such as <code>import, export, use, intern</code>, and should be
 * controlled by each lisp instance, is NOT managed by this class,
 * but managed by {@link Obarray}.
 * @see Symbol
 * @see Obarray
 */
public final class Package {
    static private final HashMap pkgMap = new HashMap();

    /** Predefined package: "SYS". */
    static public final Package SYS = forName("SYS");
    /** Predefined package: "LISP". */
    static public final Package LISP = forName("LISP");
    /** Predefined package: "KEYWORD". */
    static public final Package KEYWORD = forName("KEYWORD");

    static {
        Nil nil = new Nil();
        T t = new T();
        LISP.symMap.put(nil.pname(), nil);
        LISP.symMap.put(t.pname(), t);
    }

    static private String toPkgKey(Object pkgname) {
        if (Data.isString(pkgname)) {
            return Data.string(pkgname);
        }
        else if (Data.isSymbol(pkgname)) {
            return Data.symbol(pkgname).pname();
        }
        else {
            throw new TypeException
                ("argument should be a string or a symbol: ~S.",
                 Lists.list(pkgname));
        }
    }
    /** Returns a package for the specified name. */
    static public synchronized Package forName(Object pkgname) {
        if (pkgname == null) {
            throw new NullPointerException("pkgname is null");
        }
        else if (Data.isPkg(pkgname)) {
            return Data.pkg(pkgname);
        }
        else {
            String pkgKey = toPkgKey(pkgname);
            Package pkg = Data.pkg(pkgMap.get(pkgKey));
            if (pkg == null) {
                pkg = new Package(pkgKey);
                pkgMap.put(pkgKey, pkg);
            }
            return pkg;
        }
    }
    /** Returns a package bound to the variable
        <code>*PACKAGE*</code>. */
    static public Package get(Env env) {
        return Data.pkg(env.get(Symbols.PACKAGE));
    }

    /** name of this package */
    private final String pkgname;
    /** repositry of symbols */
    private final HashMap symMap = new HashMap();

    private Package(String pkgname) {
        this.pkgname = pkgname;
    }
    /** Returns the name of this package. */
    public String pkgname() {
        return pkgname;
    }
    /**
     * Returns a symbol for the specified name.
     * Note that lisp function <code>INTERN</code>
     * is NOT equivalent to this method, but equivalent to
     * {@link Obarray#intern Obarray#intern}.
     */
    public synchronized Symbol intern(String name) {
        Symbol sym = (Symbol) symMap.get(name);
        if (sym == null) {
            sym = new Symbol(this, name, (this == KEYWORD));
            symMap.put(name, sym);
        }
        return sym;
    }
    public String toString() {
        return super.toString()+"["+pkgname()+"]";
    }
}
