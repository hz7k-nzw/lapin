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
import lapin.function.Callable;
import lapin.function.Callable0;
import lapin.function.Callable1;
import lapin.function.Callable2;
import lapin.function.Callable3;
import lapin.function.Callable4;
import lapin.function.Callable5;
import lapin.function.Callable0r;
import lapin.function.Callable1r;
import lapin.function.Callable2r;
import lapin.function.Callable3r;
import lapin.function.Callable4r;
import lapin.function.Callable5r;
import lapin.function.Function;
import lapin.function.Expr;
import lapin.function.Subr;
import lapin.lang.Data;
import lapin.lang.Env;
import lapin.lang.Lists;
import lapin.lang.ProgramException;
import lapin.lang.Symbols;
//import lapin.util.Logger;
import lapin.util.TracableException;

/**
 * Funcall functions.
 */
public final class Funcall {
    /**
     * Calls the specified function with the specified arguments.
     * @param func Function
     * @param args List of arguments
     * @param env
     * @return Return value of the function
     */
    static public Object funcall(Function func, Object args, Env env) {
        //if (Logger.tracelevelp(env))
        //    Logger.trace("funcall:~S -> ~S",
        //                 Lists.list(func, args), env);
        int nargs = Lists.length(args);
        try {
            switch (nargs) {
            case 0:
                if (func instanceof Callable0) {
                    Callable0 c0 = (Callable0) func;
                    return c0.call0(env);
                }
                else if (func instanceof Callable0r) {
                    Callable0r c0r = (Callable0r) func;
                    return c0r.call0(Symbols.NIL, env);
                }
                break;
            case 1:
                if (func instanceof Callable1) {
                    Callable1 c1 = (Callable1) func;
                    Object arg0 = Lists.car(args);
                    return c1.call1(arg0, env);
                }
                else if (func instanceof Callable0r) {
                    Callable0r c0r = (Callable0r) func;
                    return c0r.call0(args, env);
                }
                else if (func instanceof Callable1r) {
                    Callable1r c1r = (Callable1r) func;
                    Object arg0 = Lists.car(args);
                    return c1r.call1(arg0, Symbols.NIL, env);
                }
                break;
            case 2:
                if (func instanceof Callable2) {
                    Callable2 c2 = (Callable2) func;
                    Object arg0 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg1 = Lists.car(args);
                    return c2.call2(arg0, arg1, env);
                }
                else if (func instanceof Callable0r) {
                    Callable0r c0r = (Callable0r) func;
                    return c0r.call0(args, env);
                }
                else if (func instanceof Callable1r) {
                    Callable1r c1r = (Callable1r) func;
                    Object arg0 = Lists.car(args);
                    args = Lists.cdr(args);
                    return c1r.call1(arg0, args, env);
                }
                else if (func instanceof Callable2r) {
                    Callable2r c2r = (Callable2r) func;
                    Object arg0 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg1 = Lists.car(args);
                    return c2r.call2(arg0, arg1, Symbols.NIL, env);
                }
                break;
            case 3:
                if (func instanceof Callable3) {
                    Callable3 c3 = (Callable3) func;
                    Object arg0 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg1 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg2 = Lists.car(args);
                    return c3.call3(arg0, arg1, arg2, env);
                }
                else if (func instanceof Callable0r) {
                    Callable0r c0r = (Callable0r) func;
                    return c0r.call0(args, env);
                }
                else if (func instanceof Callable1r) {
                    Callable1r c1r = (Callable1r) func;
                    Object arg0 = Lists.car(args);
                    args = Lists.cdr(args);
                    return c1r.call1(arg0, args, env);
                }
                else if (func instanceof Callable2r) {
                    Callable2r c2r = (Callable2r) func;
                    Object arg0 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg1 = Lists.car(args);
                    args = Lists.cdr(args);
                    return c2r.call2(arg0, arg1, args, env);
                }
                else if (func instanceof Callable3r) {
                    Callable3r c3r = (Callable3r) func;
                    Object arg0 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg1 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg2 = Lists.car(args);
                    return c3r.call3(arg0, arg1, arg2, Symbols.NIL, env);
                }
                break;
            case 4:
                if (func instanceof Callable4) {
                    Callable4 c4 = (Callable4) func;
                    Object arg0 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg1 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg2 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg3 = Lists.car(args);
                    return c4.call4(arg0, arg1, arg2, arg3, env);
                }
                else if (func instanceof Callable0r) {
                    Callable0r c0r = (Callable0r) func;
                    return c0r.call0(args, env);
                }
                else if (func instanceof Callable1r) {
                    Callable1r c1r = (Callable1r) func;
                    Object arg0 = Lists.car(args);
                    args = Lists.cdr(args);
                    return c1r.call1(arg0, args, env);
                }
                else if (func instanceof Callable2r) {
                    Callable2r c2r = (Callable2r) func;
                    Object arg0 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg1 = Lists.car(args);
                    args = Lists.cdr(args);
                    return c2r.call2(arg0, arg1, args, env);
                }
                else if (func instanceof Callable3r) {
                    Callable3r c3r = (Callable3r) func;
                    Object arg0 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg1 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg2 = Lists.car(args);
                    args = Lists.cdr(args);
                    return c3r.call3(arg0, arg1, arg2, args, env);
                }
                else if (func instanceof Callable4r) {
                    Callable4r c4r = (Callable4r) func;
                    Object arg0 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg1 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg2 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg3 = Lists.car(args);
                    return c4r.call4(arg0, arg1, arg2, arg3, Symbols.NIL, env);
                }
                break;
            case 5:
                if (func instanceof Callable5) {
                    Callable5 c5 = (Callable5) func;
                    Object arg0 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg1 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg2 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg3 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg4 = Lists.car(args);
                    return c5.call5(arg0, arg1, arg2, arg3, arg4, env);
                }
                else if (func instanceof Callable0r) {
                    Callable0r c0r = (Callable0r) func;
                    return c0r.call0(args, env);
                }
                else if (func instanceof Callable1r) {
                    Callable1r c1r = (Callable1r) func;
                    Object arg0 = Lists.car(args);
                    args = Lists.cdr(args);
                    return c1r.call1(arg0, args, env);
                }
                else if (func instanceof Callable2r) {
                    Callable2r c2r = (Callable2r) func;
                    Object arg0 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg1 = Lists.car(args);
                    args = Lists.cdr(args);
                    return c2r.call2(arg0, arg1, args, env);
                }
                else if (func instanceof Callable3r) {
                    Callable3r c3r = (Callable3r) func;
                    Object arg0 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg1 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg2 = Lists.car(args);
                    args = Lists.cdr(args);
                    return c3r.call3(arg0, arg1, arg2, args, env);
                }
                else if (func instanceof Callable4r) {
                    Callable4r c4r = (Callable4r) func;
                    Object arg0 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg1 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg2 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg3 = Lists.car(args);
                    args = Lists.cdr(args);
                    return c4r.call4(arg0, arg1, arg2, arg3, args, env);
                }
                else if (func instanceof Callable5r) {
                    Callable5r c5r = (Callable5r) func;
                    Object arg0 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg1 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg2 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg3 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg4 = Lists.car(args);
                    return c5r.call5(arg0, arg1, arg2, arg3, arg4, Symbols.NIL, env);
                }
                break;
            default:
                if (func instanceof Callable0r) {
                    Callable0r c0r = (Callable0r) func;
                    return c0r.call0(args, env);
                }
                else if (func instanceof Callable1r) {
                    Callable1r c1r = (Callable1r) func;
                    Object arg0 = Lists.car(args);
                    args = Lists.cdr(args);
                    return c1r.call1(arg0, args, env);
                }
                else if (func instanceof Callable2r) {
                    Callable2r c2r = (Callable2r) func;
                    Object arg0 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg1 = Lists.car(args);
                    args = Lists.cdr(args);
                    return c2r.call2(arg0, arg1, args, env);
                }
                else if (func instanceof Callable3r) {
                    Callable3r c3r = (Callable3r) func;
                    Object arg0 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg1 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg2 = Lists.car(args);
                    args = Lists.cdr(args);
                    return c3r.call3(arg0, arg1, arg2, args, env);
                }
                else if (func instanceof Callable4r) {
                    Callable4r c4r = (Callable4r) func;
                    Object arg0 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg1 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg2 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg3 = Lists.car(args);
                    args = Lists.cdr(args);
                    return c4r.call4(arg0, arg1, arg2, arg3, args, env);
                }
                else if (func instanceof Callable5r) {
                    Callable5r c5r = (Callable5r) func;
                    Object arg0 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg1 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg2 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg3 = Lists.car(args);
                    args = Lists.cdr(args);
                    Object arg4 = Lists.car(args);
                    args = Lists.cdr(args);
                    return c5r.call5(arg0, arg1, arg2, arg3, arg4, args, env);
                }
                break;
            }

            return funcallDefault(func, args, env);

        } catch (NonLocalExit e) {
            throw e;
        } catch (Evaluator.Exception e) {
            throw e.push(func);
        } catch (TracableException e) {
            throw e;
        } catch (java.lang.Exception e) {
            throw new Evaluator.Exception("funcall error", e).push(func);
        }
    }
    /**
     * Calls the specified function with no arguments.
     * @param func Function
     * @param env
     * @return Return value of the function
     */
    static public Object funcall0(Function func, Env env) {
        try {
            if (func instanceof Callable0) {
                Callable0 c0 = (Callable0) func;
                return c0.call0(env);
            }
            else if (func instanceof Callable0r) {
                Callable0r c0r = (Callable0r) func;
                return c0r.call0(Symbols.NIL, env);
            }
            else {
                return funcallDefault(func, Symbols.NIL, env);
            }
        } catch (NonLocalExit e) {
            throw e;
        } catch (Evaluator.Exception e) {
            throw e.push(func);
        } catch (TracableException e) {
            throw e;
        } catch (java.lang.Exception e) {
            throw new Evaluator.Exception("funcall0 error", e).push(func);
        }
    }
    /**
     * Calls the specified function with 1 argument.
     * @param func Function
     * @param arg0
     * @param env
     * @return Return value of the function
     */
    static public Object funcall1(Function func, Object arg0, Env env) {
        try {
            if (func instanceof Callable1) {
                Callable1 c1 = (Callable1) func;
                return c1.call1(arg0, env);
            }
            else if (func instanceof Callable0r) {
                Callable0r c0r = (Callable0r) func;
                return c0r.call0(Lists.list(arg0), env);
            }
            else if (func instanceof Callable1r) {
                Callable1r c1r = (Callable1r) func;
                return c1r.call1(arg0, Symbols.NIL, env);
            }
            else {
                // inefficient!
                return funcallDefault(func, Lists.list(arg0), env);
            }
        } catch (NonLocalExit e) {
            throw e;
        } catch (Evaluator.Exception e) {
            throw e.push(func);
        } catch (TracableException e) {
            throw e;
        } catch (java.lang.Exception e) {
            throw new Evaluator.Exception("funcall1 error", e).push(func);
        }
    }
    /**
     * Calls the specified function with 2 arguments.
     * @param func Function
     * @param arg0
     * @param arg1
     * @param env
     * @return Return value of the function
     */
    static public Object funcall2
        (Function func, Object arg0, Object arg1, Env env) {
        try {
            if (func instanceof Callable2) {
                Callable2 c2 = (Callable2) func;
                return c2.call2(arg0, arg1, env);
            }
            else if (func instanceof Callable0r) {
                Callable0r c0r = (Callable0r) func;
                return c0r.call0(Lists.list(arg0, arg1), env);
            }
            else if (func instanceof Callable1r) {
                Callable1r c1r = (Callable1r) func;
                return c1r.call1(arg0, Lists.list(arg1), env);
            }
            else if (func instanceof Callable2r) {
                Callable2r c2r = (Callable2r) func;
                return c2r.call2(arg0, arg1, Symbols.NIL, env);
            }
            else {
                // inefficient!
                return funcallDefault(func, Lists.list(arg0, arg1), env);
            }
        } catch (NonLocalExit e) {
            throw e;
        } catch (Evaluator.Exception e) {
            throw e.push(func);
        } catch (TracableException e) {
            throw e;
        } catch (java.lang.Exception e) {
            throw new Evaluator.Exception("funcall2 error", e).push(func);
        }
    }
    /**
     * Calls the specified function with 3 arguments.
     * @param func Function
     * @param arg0
     * @param arg1
     * @param arg2
     * @param env
     * @return Return value of the function
     */
    static public Object funcall3
        (Function func, Object arg0, Object arg1, Object arg2, Env env) {
        try {
            if (func instanceof Callable3) {
                Callable3 c3 = (Callable3) func;
                return c3.call3(arg0, arg1, arg2, env);
            }
            else if (func instanceof Callable0r) {
                Callable0r c0r = (Callable0r) func;
                return c0r.call0(Lists.list(arg0, arg1, arg2), env);
            }
            else if (func instanceof Callable1r) {
                Callable1r c1r = (Callable1r) func;
                return c1r.call1(arg0, Lists.list(arg1, arg2), env);
            }
            else if (func instanceof Callable2r) {
                Callable2r c2r = (Callable2r) func;
                return c2r.call2(arg0, arg1, Lists.list(arg2), env);
            }
            else if (func instanceof Callable3r) {
                Callable3r c3r = (Callable3r) func;
                return c3r.call3(arg0, arg1, arg2, Symbols.NIL, env);
            }
            else {
                // inefficient!
                return funcallDefault
                    (func, Lists.list(arg0, arg1, arg2), env);
            }
        } catch (NonLocalExit e) {
            throw e;
        } catch (Evaluator.Exception e) {
            throw e.push(func);
        } catch (TracableException e) {
            throw e;
        } catch (java.lang.Exception e) {
            throw new Evaluator.Exception("funcall3 error", e).push(func);
        }
    }
    /**
     * Calls the specified function with 4 arguments.
     * @param func Function
     * @param arg0
     * @param arg1
     * @param arg2
     * @param arg3
     * @param env
     * @return Return value of the function
     */
    static public Object funcall4
        (Function func, Object arg0, Object arg1, Object arg2,
         Object arg3, Env env) {
        try {
            if (func instanceof Callable4) {
                Callable4 c4 = (Callable4) func;
                return c4.call4(arg0, arg1, arg2, arg3, env);
            }
            else if (func instanceof Callable0r) {
                Callable0r c0r = (Callable0r) func;
                return c0r.call0(Lists.list(arg0, arg1, arg2, arg3), env);
            }
            else if (func instanceof Callable1r) {
                Callable1r c1r = (Callable1r) func;
                return c1r.call1(arg0, Lists.list(arg1, arg2, arg3), env);
            }
            else if (func instanceof Callable2r) {
                Callable2r c2r = (Callable2r) func;
                return c2r.call2(arg0, arg1, Lists.list(arg2, arg3), env);
            }
            else if (func instanceof Callable3r) {
                Callable3r c3r = (Callable3r) func;
                return c3r.call3(arg0, arg1, arg2, Lists.list(arg3), env);
            }
            else if (func instanceof Callable4r) {
                Callable4r c4r = (Callable4r) func;
                return c4r.call4(arg0, arg1, arg2, arg3, Symbols.NIL, env);
            }
            else {
                // inefficient!
                return funcallDefault
                    (func, Lists.list(arg0, arg1, arg2, arg3), env);
            }
        } catch (NonLocalExit e) {
            throw e;
        } catch (Evaluator.Exception e) {
            throw e.push(func);
        } catch (TracableException e) {
            throw e;
        } catch (java.lang.Exception e) {
            throw new Evaluator.Exception("funcall4 error", e).push(func);
        }
    }
    /**
     * Calls the specified function with 5 arguments.
     * @param func Function
     * @param arg0
     * @param arg1
     * @param arg2
     * @param arg3
     * @param arg4
     * @param env
     * @return Return value of the function
     */
    static public Object funcall5
        (Function func, Object arg0, Object arg1, Object arg2,
         Object arg3, Object arg4, Env env) {
        try {
            if (func instanceof Callable5) {
                Callable5 c5 = (Callable5) func;
                return c5.call5(arg0, arg1, arg2, arg3, arg4, env);
            }
            else if (func instanceof Callable0r) {
                Callable0r c0r = (Callable0r) func;
                return c0r.call0(Lists.list(arg0, arg1, arg2, arg3, arg4), env);
            }
            else if (func instanceof Callable1r) {
                Callable1r c1r = (Callable1r) func;
                return c1r.call1(arg0, Lists.list(arg1, arg2, arg3, arg4), env);
            }
            else if (func instanceof Callable2r) {
                Callable2r c2r = (Callable2r) func;
                return c2r.call2(arg0, arg1, Lists.list(arg2, arg3, arg4), env);
            }
            else if (func instanceof Callable3r) {
                Callable3r c3r = (Callable3r) func;
                return c3r.call3(arg0, arg1, arg2, Lists.list(arg3, arg4), env);
            }
            else if (func instanceof Callable4r) {
                Callable4r c4r = (Callable4r) func;
                return c4r.call4(arg0, arg1, arg2, arg3, Lists.list(arg4), env);
            }
            else if (func instanceof Callable5r) {
                Callable5r c5r = (Callable5r) func;
                return c5r.call5(arg0, arg1, arg2, arg3, arg4, Symbols.NIL, env);
            }
            else {
                // inefficient!
                return funcallDefault
                    (func, Lists.list(arg0, arg1, arg2, arg3, arg4), env);
            }
        } catch (NonLocalExit e) {
            throw e;
        } catch (Evaluator.Exception e) {
            throw e.push(func);
        } catch (TracableException e) {
            throw e;
        } catch (java.lang.Exception e) {
            throw new Evaluator.Exception("funcall5 error", e).push(func);
        }
    }
    static private Object funcallDefault(Function func, Object args, Env env)
        throws java.lang.Exception {
        if (func instanceof Callable) {
            Callable c = (Callable) func;
            return c.call(args, env);
        }
        else if (Data.isExpr(func)) {
            Expr expr = Data.expr(func);
            Env newenv = env.child();
            Evaluator.bindArgs(expr.lambdaList(), args, newenv);
            return Evaluator.evalBody(expr.body(), newenv);
        }
        else {
            throw new ProgramException
                ("function ~S cannot be applied to arguments: ~S",
                 Lists.list(func, args));
        }
    }

    private Funcall() {}
}
