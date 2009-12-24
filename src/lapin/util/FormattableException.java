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
package lapin.util;
import lapin.lang.Env;
import lapin.io.Printer;
public class FormattableException extends RuntimeException {
    static public final void fillInMessage(Throwable t, Env env) {
        for (; t != null; t = t.getCause()) {
            if (t instanceof FormattableException) {
                FormattableException fe = (FormattableException) t;
                fe.fillInMessage(env);
            }
        }
    }
    private final String ctrlstr;
    private final Object args;
    private String msg = null;
    public FormattableException(String ctrlstr, Object args) {
        super();
        this.ctrlstr = ctrlstr;
        this.args = args;
    }
    public FormattableException(String ctrlstr, Object args,
                                Throwable cause) {
        super(cause);
        this.ctrlstr = ctrlstr;
        this.args = args;
    }
    public final String ctrlstr() {
        return ctrlstr;
    }
    public final Object args() {
        return args;
    }
    public final String getMessage() {
        return msg;
    }
    public FormattableException fillInMessage(Env env) {
        if (msg == null) {
            msg = Printer.formatToString(ctrlstr, args, env);
        }
        return this;
    }
}
