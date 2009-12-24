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
import lapin.lang.Data;
import lapin.lang.Lisp;
import lapin.lang.Lists;
import lapin.lang.Symbol;
import lapin.io.Printer;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.TreeSet;

/**
 * Lisp environment.
 */
public final class Env {
    // uninterned symbol: for private use only
    static private final Symbol UNBOUND = Symbol.gensym("_UNBOUND_");

    /** Comparator object which compares two symbols by pname. */
    static private final Comparator PNAME_COMPARATOR
        = new Comparator() {
                public int compare(Object o1, Object o2) {
                    Symbol s1 = (Symbol) o1;
                    Symbol s2 = (Symbol) o2;
                    Package p1 = s1.pkg();
                    Package p2 = s2.pkg();
                    String pn1 = p1 == null ? "" : p1.pkgname();
                    String pn2 = p2 == null ? "" : p2.pkgname();
                    int compPkg = pn1.compareTo(pn2);
                    if (compPkg == 0)
                        return s1.pname().compareTo(s2.pname());
                    else
                        return compPkg;
                }
            };

    /** lisp instance held by this environment. */
    private final Lisp lisp;
    /** parent environment. null means global environment. */
    private final Env parent;
    /** hashtable used to hold bindings .*/
    private final HashMap binds = new HashMap();

    /**
     * Public constructor used for creating
     * a global environment.
     */
    public Env(Lisp l) {
        if (l == null)
            throw new NullPointerException
                ("lisp instance must not be null.");
        this.parent = null;
        this.lisp = l;
    }
    /**
     * Private constructor used for creating
     * a local (child) environment. The lisp instance
     * held by parent p is passed (inherited) to the
     * child.
     */
    private Env(Env p) {
        if (p == null)
            throw new NullPointerException
                ("parent env must not be null");
        this.parent = p;
        this.lisp = p.lisp;
    }
    /**
     * Returns the lisp instance held by this environment.
     */
    public Lisp lisp() {
        return lisp;
    }
    /**
     * Creates a new child environment.
     */
    public Env child() {
        return new Env(this);
    }
    /**
     * Returns the parent of this environment.
     */
    public Env parent() {
        return parent;
    }
    /**
     * Returns a value for the specified variable.
     * @param sym Variable
     * @param defaultVal
     * @return A Value for the variable
     *         or <code>defaultVal</code> if the binding not found
     */
    public Object get(Symbol sym, Object defaultVal) {
        if (sym.isSelfeval()) {
            return sym;
        }
        Env e = this;
        while (true) {
            if (e.binds.containsKey(sym)) {
                return e.binds.get(sym);
            }
            else if (e.parent == null) {
                return defaultVal;
            }
            else {
                e = e.parent;
            }
        }
    }
    /**
     * Returns a value for the specified variable.
     * @param sym Variable
     * @return A Value for the variable
     *         or error if the binding not found
     */
    public Object get(Symbol sym) {
        Object val = get(sym, UNBOUND);
        if (val == UNBOUND)
            throw new UnboundVariableException(sym);
        return val;
    }
    /**
     * Returns a value for the specified variable.
     * @param sym Variable
     * @return A Value for the variable
     *         or error if the binding not found
     */
    public int getInt(Symbol sym) {
        return Data.fixnum(get(sym)).intValue();
    }
    /**
     * Returns a value for the specified variable.
     * @param sym Variable
     * @return A Value for the variable
     *         or error if the binding not found
     */
    public double getDouble(Symbol sym) {
        return Data.flonum(get(sym)).doubleValue();
    }
    /**
     * Returns a value for the specified variable.
     * @param sym Variable
     * @return A Value for the variable
     *         or error if the binding not found
     */
    public char getChar(Symbol sym) {
        return Data.character(get(sym)).charValue();
    }
    /** Returns true if the binding exists. */
    public boolean isBound(Symbol sym) {
        return get(sym, UNBOUND) != UNBOUND;
    }
    ///** Returns <code>T</code> if the binding exists,
    //    and <code>NIL</code> otherwise. */
    //public Object boundp(Symbol sym) {
    //    return Data.toPredicate(isBound(sym));
    //}
    /**
     * Creates a new binding in this environment.
     * @param sym Variable
     * @param val Value for the variable
     */
    public void bind(Symbol sym, Object val) {
        if (sym.isSelfeval()) {
            throw new ProgramException
                ("may not be used as a variable: ~S.",
                 Lists.list(sym));
        }
        if (this.lisp.isConstant(sym)) {
            throw new ProgramException
                ("~S is a constant.", Lists.list(sym));
        }
        if (this.binds.containsKey(sym)) {
            throw new ProgramException
                ("binds already exists: ~S.", Lists.list(sym));
        }
        this.binds.put(sym, val);
    }
    /**
     * Creates a new binding in this environment.
     * @param sym Variable
     * @param val Value for the variable
     */
    public void bind(Symbol sym, int val) {
        bind(sym, Data.toFixnum(val));
    }
    /**
     * Creates a new binding in this environment.
     * @param sym Variable
     * @param val Value for the variable
     */
    public void bind(Symbol sym, double val) {
        bind(sym, Data.toFlonum(val));
    }
    /**
     * Creates a new binding in this environment.
     * @param sym Variable
     * @param val Value for the variable
     */
    public void bind(Symbol sym, char val) {
        bind(sym, Data.toCharacter(val));
    }
    /**
     * Remove the binding from this environment.
     * @param sym Variable
     * @return Value for the variable
     */
    public Object unbind(Symbol sym) {
        if (sym.isSelfeval()) {
            throw new ProgramException
                ("may not be used as a variable: ~S.",
                 Lists.list(sym));
        }
        if (this.lisp.isConstant(sym)) {
            throw new ProgramException
                ("~S is a constant.", Lists.list(sym));
        }
        if (!this.binds.containsKey(sym)) {
            throw new ProgramException
                ("binds not found: ~S.", Lists.list(sym));
        }
        return this.binds.remove(sym);
    }
    /**
     * Remove the binding from this environment.
     * @param sym Variable
     * @return Value for the variable
     */
    public int unbindInt(Symbol sym) {
        return Data.fixnum(unbind(sym)).intValue();
    }
    /**
     * Remove the binding from this environment.
     * @param sym Variable
     * @return Value for the variable
     */
    public double unbindDouble(Symbol sym) {
        return Data.flonum(unbind(sym)).doubleValue();
    }
    /**
     * Remove the binding from this environment.
     * @param sym Variable
     * @return Value for the variable
     */
    public char unbindChar(Symbol sym) {
        return Data.character(unbind(sym)).charValue();
    }
    /**
     * Updates a value for the specified variable.
     * When the binding not exists, new binding is created
     * in the global environment.
     * @param sym Variable
     * @param val New value for the variable
     */
    public void set(Symbol sym, Object val) {
        if (sym.isSelfeval()) {
            throw new ProgramException
                ("may not be used as a variable: ~S.",
                 Lists.list(sym));
        }
        if (this.lisp.isConstant(sym)) {
            throw new ProgramException
                ("~S is a constant.", Lists.list(sym));
        }
        Env e = this;
        while (true) {
            if (e.binds.containsKey(sym)) {
                e.binds.put(sym, val);
                return;
            }
            else if (e.parent == null) {
                // bind not found -> make new bind in root env.
                e.binds.put(sym, val);
                return;
            }
            else {
                e = e.parent;
            }
        }
    }
    /**
     * Updates a value for the specified variable.
     * When the binding not exists, new binding is created
     * in the global environment.
     * @param sym Variable
     * @param val New value for the variable
     */
    public void set(Symbol sym, int val) {
        set(sym, Data.toFixnum(val));
    }
    /**
     * Updates a value for the specified variable.
     * When the binding not exists, new binding is created
     * in the global environment.
     * @param sym Variable
     * @param val New value for the variable
     */
    public void set(Symbol sym, double val) {
        set(sym, Data.toFlonum(val));
    }
    /**
     * Updates a value for the specified variable.
     * When the binding not exists, new binding is created
     * in the global environment.
     * @param sym Variable
     * @param val New value for the variable
     */
    public void set(Symbol sym, char val) {
        set(sym, Data.toCharacter(val));
    }
    /** Dump this environment. */
    public void dump(Object stream, Env env) {
        for (Env e = this; e != null; e = e.parent) {
            Printer.format("env: ~S~%", Lists.list(e), stream, env);
            TreeSet keySet = new TreeSet(PNAME_COMPARATOR);
            keySet.addAll(e.binds.keySet());
            for (Iterator i = keySet.iterator(); i.hasNext();) {
                Object key = i.next();
                Object val = e.binds.get(key);
                Printer.format("~S = ~S~%", Lists.list(key, val),
                               stream, env);
            }
            Printer.terpri(stream, env);
        }
    }
}