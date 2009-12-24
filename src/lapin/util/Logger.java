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
import lapin.lang.Data;
import lapin.lang.Env;
import lapin.lang.Symbols;
import lapin.io.Printer;

public final class Logger {
    static private final Integer TRACE = Data.toFixnum(1);
    static private final Integer DEBUG = Data.toFixnum(2);
    static private final Integer  INFO = Data.toFixnum(3);
    static private final Integer  WARN = Data.toFixnum(4);
    static private final Integer ERROR = Data.toFixnum(5);
    static private final Integer FATAL = Data.toFixnum(6);

    static private String levelToString(Integer level) {
        switch (level.intValue()) {
        case 1:
            return "[TRACE] ";
        case 2:
            return "[DEBUG] ";
        case 3:
            return "[INFO] ";
        case 4:
            return "[WARN] ";
        case 5:
            return "[ERROR] ";
        case 6:
            return "[FATAL] ";
        default:
            return "[OTHER] ";
        }
    }
    static private boolean loglevelp(Integer level, Env env) {
        Integer l = Data.fixnum(env.get(Symbols.LOG_LEVEL));
        return l.compareTo(level) <= 0;
    }
    static private void log(Integer level, String msg, Object args,
                            Throwable t, Env env) {
        if (!loglevelp(level, env))
            return;

        Object stream = env.get(Symbols.LOG_OUT);
        Printer.princ(levelToString(level), stream, env);
        Printer.format(msg, args, stream, env);
        Printer.terpri(stream, env);
        if (t != null) {
            Printer.printException(t, stream, env);
        }
    }

    static public boolean tracelevelp(Env env) {
        return loglevelp(TRACE, env);
    }
    static public void trace(String msg, Env env) {
        log(TRACE, msg, Symbols.NIL, null, env);
    }
    static public void trace(String msg, Object args, Env env) {
        log(TRACE, msg, args, null, env);
    }
    static public void trace(String msg, Object args, Throwable t, Env env) {
        log(TRACE, msg, args, t, env);
    }

    static public boolean debuglevelp(Env env) {
        return loglevelp(DEBUG, env);
    }
    static public void debug(String msg, Env env) {
        log(DEBUG, msg, Symbols.NIL, null, env);
    }
    static public void debug(String msg, Object args, Env env) {
        log(DEBUG, msg, args, null, env);
    }
    static public void debug(String msg, Object args, Throwable t, Env env) {
        log(DEBUG, msg, args, t, env);
    }

    static public boolean infolevelp(Env env) {
        return loglevelp(INFO, env);
    }
    static public void info(String msg, Env env) {
        log(INFO, msg, Symbols.NIL, null, env);
    }
    static public void info(String msg, Object args, Env env) {
        log(INFO, msg, args, null, env);
    }
    static public void info(String msg, Object args, Throwable t, Env env) {
        log(INFO, msg, args, t, env);
    }

    static public boolean warnlevelp(Env env) {
        return loglevelp(WARN, env);
    }
    static public void warn(String msg, Env env) {
        log(WARN, msg, Symbols.NIL, null, env);
    }
    static public void warn(String msg, Object args, Env env) {
        log(WARN, msg, args, null, env);
    }
    static public void warn(String msg, Object args, Throwable t, Env env) {
        log(WARN, msg, args, t, env);
    }

    static public boolean errorlevelp(Env env) {
        return loglevelp(ERROR, env);
    }
    static public void error(String msg, Env env) {
        log(ERROR, msg, Symbols.NIL, null, env);
    }
    static public void error(String msg, Object args, Env env) {
        log(ERROR, msg, args, null, env);
    }
    static public void error(String msg, Object args, Throwable t, Env env) {
        log(ERROR, msg, args, t, env);
    }

    static public boolean fatallevelp(Env env) {
        return loglevelp(FATAL, env);
    }
    static public void fatal(String msg, Env env) {
        log(FATAL, msg, Symbols.NIL, null, env);
    }
    static public void fatal(String msg, Object args, Env env) {
        log(FATAL, msg, args, null, env);
    }
    static public void fatal(String msg, Object args, Throwable t, Env env) {
        log(FATAL, msg, args, t, env);
    }

    private Logger() {}
}
