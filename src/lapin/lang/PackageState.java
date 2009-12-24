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
import lapin.eval.Funcall;
import lapin.function.Function;
import lapin.io.Printer;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

final class PackageState {
    /** package to which this state is connected */
    private final Package pkg;
    /** map for internal symbols */
    private final HashMap inSymMap = new HashMap();
    /** map for external symbols */
    private final HashMap exSymMap = new HashMap();
    /** packages which this package uses */
    private Object psUse = Symbols.NIL;
    /** packages by which this package is used */
    private Object psUsedBy = Symbols.NIL;

    PackageState(Package pkg) {
        this.pkg = pkg;
    }
    Package pkg() {
        return pkg;
    }
    int length() {
        return inSymMap.size();
    }
    private Symbol getIn(String pname) {
        return (Symbol) inSymMap.get(pname);
    }
    private Symbol putIn(Symbol sym) {
        return (Symbol) inSymMap.put(sym.pname(), sym);
    }
    private Symbol getEx(String pname) {
        return (Symbol) exSymMap.get(pname);
    }
    private Symbol putEx(Symbol sym) {
        return (Symbol) exSymMap.put(sym.pname(), sym);
    }
    private Symbol getIh(String pname) {
        for (Object l = psUse; !Lists.isEnd(l); l = Lists.cdr(l)) {
            PackageState ps = (PackageState) Lists.car(l);
            Symbol s = ps.getEx(pname);
            if (s != null) {
                return s;
            }
        }
        return null;
    }
    private void checkConflict(Symbol sym) {
        Values mv = find(sym.pname(), false);
        Symbol s = (Symbol) mv.nth(0);
        Object o = mv.nth(1);
        if (o != Symbols.NIL && s != sym) {
            throw new SymbolConflictException(sym, s);
        }
    }
    //PackageState putNoCheck(Symbol sym, boolean exp) {
    //    putIn(sym);
    //    if (exp) {
    //        putEx(sym);
    //    }
    //    return this;
    //}
    boolean isInternal(Symbol sym) {
        return getIn(sym.pname()) == sym
            || getIh(sym.pname()) == sym;
    }
    boolean isExternal(Symbol sym) {
        return getEx(sym.pname()) == sym;
    }
    Values find(String pname, boolean externalOnly) {
        Symbol s = getEx(pname);
        if (s != null) {
            return Values.currentValues()
                .push(s).push(Symbols.KW_EXTERNAL);
        }
        if (externalOnly) {
            return Values.currentValues()
                .push(Symbols.NIL).push(Symbols.NIL);
        }
        s = getIn(pname);
        if (s != null) {
            return Values.currentValues()
                .push(s).push(Symbols.KW_INTERNAL);
        }
        s = getIh(pname);
        if (s != null) {
            return Values.currentValues()
                .push(s).push(Symbols.KW_INHERITED);
        }
        return Values.currentValues()
            .push(Symbols.NIL).push(Symbols.NIL);
    }
    Values intern(String pname) {
        Symbol s = getEx(pname);
        if (s != null) {
            return Values.currentValues()
                .push(s).push(Symbols.KW_EXTERNAL);
        }
        s = getIn(pname);
        if (s != null) {
            return Values.currentValues()
                .push(s).push(Symbols.KW_INTERNAL);
        }
        s = getIh(pname);
        if (s != null) {
            return Values.currentValues()
                .push(s).push(Symbols.KW_INHERITED);
        }
        s = pkg().intern(pname);
        putIn(s);
        if (pkg() == Package.KEYWORD) {
            putEx(s);
        }
        return Values.currentValues()
            .push(s).push(Symbols.NIL);
    }
    boolean use(PackageState ps) {
        if (ps == this || Lists.memq(ps, psUse) != Symbols.NIL) {
            return false;
        }
        Set exEntries = ps.exSymMap.entrySet();
        Iterator it = exEntries.iterator();
        while (it.hasNext()) {
            Map.Entry entry = (Map.Entry) it.next();
            checkConflict((Symbol) entry.getValue());
        }
        psUse = Lists.cons(ps, psUse);
        ps.psUsedBy = Lists.cons(this, ps.psUsedBy);
        return true;
    }
    boolean imp(Symbol sym) {
        /*
         * Note that this method cannot import NIL.
         */
        Values mv = find(sym.pname(), false);
        Symbol s = (Symbol) mv.nth(0);
        Object o = mv.nth(1);
        if (s == sym) {
            if (o == Symbols.KW_INHERITED) {
                // sym: INHERITED -> INTERNAL
                putIn(sym);
                return true;
            }
            else if (o == Symbols.NIL) {
                // sym (== NIL): NIL -> INTERNAL
                putIn(sym);
                return true;
            }
            else {
                return false;
            }
        }
        else { /* s != sym */
            if (o == Symbols.NIL) {
                // sym: NIL -> INTERNAL
                putIn(sym);
                return true;
            }
            else {
                throw new SymbolConflictException(sym, s);
            }
        }
    }
    boolean exp(Symbol sym) {
        /*
         * Note that this method cannot export NIL.
         */
        Values mv = find(sym.pname(), false);
        Symbol s = (Symbol) mv.nth(0);
        Object o = mv.nth(1);
        if (s == sym) {
            if (o == Symbols.KW_EXTERNAL) {
                return false;
            }
            else {
                for (Object l = psUsedBy; !Lists.isEnd(l); l = Lists.cdr(l)) {
                    PackageState ps = (PackageState) Lists.car(l);
                    ps.checkConflict(sym);
                }
                if (o == Symbols.KW_INHERITED) {
                    // sym: INHERITED -> INTERNAL -> EXTERNAL
                    putIn(sym);
                    putEx(sym);
                    return true;
                }
                else if (o == Symbols.KW_INTERNAL) {
                    // sym: INTERNAL -> EXTERNAL
                    putEx(sym);
                    return true;
                }
                else { /* o == Symbols.NIL */
                    return false;
                }
            }
        }
        else { /* s != sym */
            throw new SymbolNotAccessibleException(sym, pkg());
        }
    }
    void mapc(Function fun, Env env) {
        for (Iterator it = inSymMap.values().iterator();
             it.hasNext();) {
            Funcall.funcall1(fun, it.next(), env);
        }
        for (Object l = psUse; !Lists.isEnd(l); l = Lists.cdr(l)) {
            PackageState ps = (PackageState) Lists.car(l);
            for (Iterator it = ps.exSymMap.values().iterator();
                 it.hasNext();) {
                Funcall.funcall1(fun, it.next(), env);
            }
        }
    }
    void dump(Object stream, Env env) {
        // pkg
        Printer.format("pkg: ~S~%~%", Lists.list(pkg), stream, env);
        // inSymMap
        Printer.format("symbols: ~%", Symbols.NIL, stream, env);
        TreeSet keySet;
        keySet = new TreeSet(inSymMap.keySet());
        for (Iterator i = keySet.iterator(); i.hasNext();) {
            Object key = i.next();
            Object val = inSymMap.get(key);
            Object ext = exSymMap.containsKey(key) ? "external" : "internal";
            Printer.format("~S = ~S (~A)~%",
                           Lists.list(key, val, ext), stream, env);
        }
        Printer.terpri(stream, env);
        // psUse
        Printer.format("use packages: ~%", Symbols.NIL, stream, env);
        for (Object l = psUse; !Lists.isEnd(l); l = Lists.cdr(l)) {
            PackageState ps = (PackageState) Lists.car(l);
            Printer.format("~S~%", Lists.list(ps.pkg()), stream, env);
        }
        Printer.terpri(stream, env);
        // psUsedBy
        Printer.format("used-by packages: ~%", Symbols.NIL, stream, env);
        for (Object l = psUsedBy; !Lists.isEnd(l); l = Lists.cdr(l)) {
            PackageState ps = (PackageState) Lists.car(l);
            Printer.format("~S~%", Lists.list(ps.pkg()), stream, env);
        }
        Printer.terpri(stream, env);
    }
    public String toString() {
        return super.toString()+"["+pkg()+"]";
    }
}
