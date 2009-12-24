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
package lapin.comp;
import lapin.lang.Data;
import lapin.lang.Lists;
import lapin.lang.NotReachedException;
import lapin.lang.SimpleException;
import lapin.lang.Symbol;
import lapin.lang.Symbols;

final class Forms {
    static final Object T = quote(Symbols.T);
    static final Object NIL = quote(Symbols.NIL);

    static final Object quote(Object obj) {
        return Lists.list(Symbols.QUOTE, obj);
    }
    static final Object quote(int i) {
        return quote(Data.toFixnum(i));
    }
    static final Object quote(double d) {
        return quote(Data.toFlonum(d));
    }
    static final Object quote(char c) {
        return quote(Data.toCharacter(c));
    }
    static final Object car(Object obj) {
        return Lists.list(Symbols.CAR, obj);
    }
    static final Object cdr(Object obj) {
        return Lists.list(Symbols.CDR, obj);
    }
    static final Object progn(Object body) {
        return Lists.cons(Symbols.PROGN, body);
    }
    static final boolean isConstant(Object obj) {
        return isConstant(obj, true);
    }
    static final boolean isConstant(Object obj, boolean quoteRequired) {
        if (Data.isSymbol(obj)) {
            if (Data.symbol(obj).isSelfeval()) {
                if (quoteRequired) {
                    throw new SimpleException
                        ("selfeval symbol must be quoted: ~S",
                         Lists.list(obj));
                }
                return true;
            }
            else {
                return false;
            }
        }
        else if (Data.isAtom(obj)) {
            if (quoteRequired) {
                throw new SimpleException
                    ("atom must be quoted: ~S",
                     Lists.list(obj));
            }
            return true;
        }
        else if (Data.isList(obj)) {
            if (Lists.car(obj) == Symbols.QUOTE) {
                if (Lists.length(obj) != 2) {
                    throw new SimpleException
                        ("illegal quote form: ~S",
                         Lists.list(obj));
                }
                return true;
            }
            else {
                return false;
            }
        }
        else {
            throw new NotReachedException
                ("unknown object: ~S", Lists.list(obj));
        }
    }
    //static final boolean isT(Object obj) {
    //    return isT(obj, true);
    //}
    //static final boolean isT(Object obj, boolean quoteRequired) {
    //    return isConstant(obj, quoteRequired)
    //        && ((!quoteRequired && obj == Symbols.T)
    //            || (Data.isList(obj) && Lists.cadr(obj) == Symbols.T));
    //}
    static final boolean isNil(Object obj) {
        return isNil(obj, true);
    }
    static final boolean isNil(Object obj, boolean quoteRequired) {
        return isConstant(obj, quoteRequired)
            && ((!quoteRequired && obj == Symbols.NIL)
                || Data.isList(obj) && Lists.cadr(obj) == Symbols.NIL);
    }
    static final boolean isFixnum(Object obj) {
        return isFixnum(obj, true);
    }
    static final boolean isFixnum(Object obj, boolean quoteRequired) {
        return isConstant(obj, quoteRequired)
            && ((!quoteRequired && Data.isFixnum(obj))
                || Data.isList(obj) && Data.isFixnum(Lists.cadr(obj)));
    }
    static final boolean isFlonum(Object obj) {
        return isFlonum(obj, true);
    }
    static final boolean isFlonum(Object obj, boolean quoteRequired) {
        return isConstant(obj, quoteRequired)
            && ((!quoteRequired && Data.isFlonum(obj))
                || Data.isList(obj) && Data.isFlonum(Lists.cadr(obj)));
    }
    static final boolean isCharacter(Object obj) {
        return isCharacter(obj, true);
    }
    static final boolean isCharacter(Object obj, boolean quoteRequired) {
        return isConstant(obj, quoteRequired)
            && ((!quoteRequired && Data.isCharacter(obj))
                || Data.isList(obj) && Data.isCharacter(Lists.cadr(obj)));
    }
}
