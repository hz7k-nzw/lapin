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
package lapin.eval;
import lapin.lang.ControlException;
import lapin.lang.Lists;
import lapin.lang.Symbols;

/**
 * Throwable object thrown when the non local exit occurs.
 */
public abstract class NonLocalExit extends RuntimeException {
    NonLocalExit() {
        super();
    }
    /** Creates a exception which indicates
        that the non local exit failed. */
    public abstract ControlException exitFailed();

    /**
     * NonLocalExit for GO form.
     */
    static public class GO extends NonLocalExit {
        private final Object tag;
        GO(Object tag) {
            super();
            this.tag = tag;
        }
        Object tag() { return tag; }

        public ControlException exitFailed() {
            String ctrlstr = "no tag named ~S is currently visible.";
            Object args = Lists.list(tag);
            return new ControlException(ctrlstr, args, this);
        }
    }

    /**
     * NonLocalExit for RETURN form.
     */
    static public class RETURN extends NonLocalExit {
        private final Object val;
        RETURN(Object val) {
            super();
            this.val = val;
        }
        Object val() { return val; }

        public ControlException exitFailed() {
            String ctrlstr = "no block is currently visible.";
            Object args = Symbols.NIL;
            return new ControlException(ctrlstr, args, this);
        }
    }

    /**
     * NonLocalExit for *THROW form.
     */
    static public class THROW extends NonLocalExit {
        private final Object tag;
        private final Object val;
        THROW(Object tag, Object val) {
            super();
            this.tag = tag;
            this.val = val;
        }
        Object tag() { return tag; }
        Object val() { return val; }

        public ControlException exitFailed() {
            String ctrlstr = "no catcher for ~S.";
            Object args = Lists.list(tag);
            return new ControlException(ctrlstr, args, this);
        }
    }
}

