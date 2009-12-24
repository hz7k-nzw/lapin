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
import java.util.HashMap;
import java.util.Iterator;

/**
 * This class manages actual state of packages.
 * The instance of this class encapsulates the state of packages, that is
 * affected by operations such as <code>import, export, use, intern</code>.
 * @see Symbol
 * @see Package
 * @see Lisp
 */
public class Obarray implements lapin.lang.Array {
    /** map of (Package . PackageState) */
    private final HashMap psMap = new HashMap();
    /** prefix of symbol name used by GENSYM */
    private String gensymPrefix = "G";
    /** counter used by GENSYM */
    private int gensymCount = 0;

    /*
     * array interface
     */

    public Symbol type() {
        return Symbols.OBARRAY;
    }
    public Object dims() {
        int len = 0;
        Iterator it = psMap.keySet().iterator();
        while (it.hasNext()) {
            Object key = it.next();
            PackageState ps = (PackageState) psMap.get(key);
            len += ps.length();
        }
        return Lists.list(Data.toFixnum(len));
    }

    /*
     * package / interned symbol operations
     */

    private PackageState findPs(Object pkgname) {
        Package pkg = Package.forName(pkgname);
        PackageState ps = (PackageState) psMap.get(pkg);
        if (ps == null)
            throw new UnknownPackageException(pkgname);
        return ps;
    }
    public /*synchronized*/ Package findPkg(Object pkgname) {
        Package pkg = Package.forName(pkgname);
        PackageState ps = (PackageState) psMap.get(pkg);
        if (ps == null)
            return null;
        else
            return ps.pkg();
    }
    public /*synchronized*/ Package mkPkg(Object pkgname, Object useList) {
        Package pkg = Package.forName(pkgname);
        PackageState ps = (PackageState) psMap.get(pkg);
        if (ps != null) {
            throw new PackageAlreadyExistsException(pkgname);
        }
        ps = new PackageState(pkg);
        psMap.put(ps.pkg(), ps);
        //ps.use(psLisp);
        for (Object l = useList; !Lists.isEnd(l); l = Lists.cdr(l)) {
            PackageState psUsed = findPs(Lists.car(l));
            ps.use(psUsed);
        }
        return pkg;
    }
    public /*synchronized*/ void dumpPkg(Object pkgname,
                                         Object stream, Env env) {
        PackageState ps = findPs(pkgname);
        ps.dump(stream, env);
    }
    public /*synchronized*/ boolean isInternal(Object pkgname, Symbol sym) {
        PackageState ps = findPs(pkgname);
        return ps.isInternal(sym);
    }
    public /*synchronized*/ boolean isExternal(Object pkgname, Symbol sym) {
        PackageState ps = findPs(pkgname);
        return ps.isExternal(sym);
    }
    public /*synchronized*/ Values find(Object pkgname, String pname,
                                        boolean externalOnly) {
        PackageState ps = findPs(pkgname);
        return ps.find(pname, externalOnly);
    }
    public /*synchronized*/ Values intern(Object pkgname, String pname) {
        PackageState ps = findPs(pkgname);
        return ps.intern(pname);
    }
    public /*synchronized*/ boolean imp(Object pkgname, Symbol sym) {
        PackageState ps = findPs(pkgname);
        return ps.imp(sym);
    }
    public /*synchronized*/ boolean exp(Object pkgname, Symbol sym) {
        PackageState ps = findPs(pkgname);
        return ps.exp(sym);
    }
    public /*synchronized*/ boolean use(Object pkgname, Object pkgnameUsed) {
        PackageState ps = findPs(pkgname);
        PackageState psUsed = findPs(pkgnameUsed);
        return ps.use(psUsed);
    }
    public /*synchronized*/ void mapc(Object pkgname, Function fun, Env env) {
        PackageState ps = findPs(pkgname);
        ps.mapc(fun, env);
    }

    /*
     * uninterned symbol operations
     */

    private /*synchronized*/ int incGensymCount() {
        int count = gensymCount++;
        if (10000 <= gensymCount)
            gensymCount = 0;
        return count;
    }
    public Symbol gensym() {
        return gensym(gensymPrefix);
    }
    public Symbol gensym(String prefix) {
        int count = incGensymCount();
        String pname;
        if (count < 10) {
            pname = prefix+"000"+count;
        }
        else if (count < 100) {
            pname = prefix+"00"+count;
        }
        else if (count < 1000) {
            pname = prefix+"0"+count;
        }
        else {
            pname = prefix+count;
        }
        return Symbol.gensym(pname);
    }
}
