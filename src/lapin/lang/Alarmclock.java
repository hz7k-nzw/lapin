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
import lapin.eval.Evaluator;
import lapin.eval.Funcall;
import lapin.io.Printer;
import lapin.lang.Data;
import lapin.lang.Env;
import lapin.lang.Lists;
import lapin.lang.Symbol;
import lapin.lang.Symbols;
import lapin.util.Logger;
import java.util.Date;
import java.util.Timer;
import java.util.TimerTask;

/**
 * Maclisp ALARMCLOCK.
 * Note that this code is nothing but a stub for the archaic
 * scheme interpreter (in AIM-349) to run under this lisp system.
 */
public class Alarmclock {
    private Timer timer = null;

    private /*synchronized*/ Timer getTimer(Env env) {
        if (timer == null) {
            // initialize timer
            timer = new Timer(true);
            Logger.trace("alarmclock: timer created: ~S",
                         Lists.list(timer), env);
        }
        return timer;
    }
    /**
     * Controls this timer.
     * @param timername Symbol which specifies the timer type, 
     *        <code>TIME</code> for real-time timer,
     *        <code>RUNTIME</code> for cpu-time timer
     * @param q delay before handler is to be executed,
     *        the unit of <code>q</code> is second for real-time timer,
     *        microsecond for the cpu-time timer.
     * @param env
     */
    public void schedule(final Symbol timername, Integer q,
                         final Env env) {

        Logger.trace("alarmclock: schedule: ~S ~S",
                     Lists.list(timername, q), env);

        if (q.intValue() <= 0)
            throw new TypeException
                ("q must be positive: "+q, Symbols.NIL);

        // calc time
        Date time;
        if (timername == Symbols.TIME)
             // unit of q: sec
            time = new Date(System.currentTimeMillis()
                            + q.intValue() * 1000);
        else if (timername == Symbols.RUNTIME)
             // unit of q: micro sec
            time = new Date(System.currentTimeMillis()
                            + q.intValue() / 1000);
        else
            throw new TypeException
                ("timername must be either TIME or RUNTIME: ~S.",
                 Lists.list(timername));
        // regist timer task
        getTimer(env).schedule(new TimerTask() {
                /*
                 * XXX:
                 * Sharing a single lisp instance with multiple threads
                 * is NOT safe!
                 */
                public void run() {
                    try {
                        Object o = env.get(Symbols.ALARMCLOCK);
                        Logger.trace("alarmclock: funcall: ~S",
                                     Lists.list(o), env);
                        if (Data.isFunction(o)) {
                            Funcall.funcall1(Data.function(o),
                                             Lists.list(timername), env);
                        }
                        else if (Data.isSymbol(o)) {
                            Funcall.funcall1(Evaluator.function(o, env),
                                             Lists.list(timername), env);
                        }
                        else {
                            Logger.warn("alarmclock: "+
                                        "value of ALARMCLOCK "+
                                        "must be function: ~S",
                                        Lists.list(o), env);
                        }
                    } catch (lapin.eval.NonLocalExit e) {
                        Printer.printException(e.exitFailed(),
                                               Symbols.NIL, env);
                    } catch (lapin.util.TracableException e) {
                        e.push(this);
                        Printer.printException(e, Symbols.NIL, env);
                        Printer.printBackTrace(e, Symbols.NIL, env);
                    } catch (java.lang.RuntimeException e) {
                        Printer.printException(e, Symbols.NIL, env);
                    } catch (java.lang.Error e) {
                        Printer.printException(e, Symbols.NIL, env);
                    }
                }}, time);
    }
}
