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
import lapin.function.LambdaList;
import lapin.lang.Data;
import lapin.lang.Env;
import lapin.lang.Lists;
import lapin.lang.Symbol;
import java.lang.reflect.Method;

/**
 * Uninterned symbols which represent the instructions of
 * the intermediate code.
 */
public final class Insts {
    /** Push a lambdaList object (pooled in the compiled-expr)
        onto the operand stack.
        (nargs:1, stack pop:0/push:1) */
    static public final Symbol LAMBDA_LIST = Symbol.gensym("LAMBDA-LIST");
    /** Push a constant object (pooled in the compiled-expr)
        onto the operand stack.
        (nargs:1, stack pop:0/push:1) */
    static public final Symbol CONST = Symbol.gensym("CONST");
    /** Push a symbol (pooled in the compiled-expr)
        onto the operand stack.
        (nargs:1, stack pop:0/push:1) */
    static public final Symbol VAR = Symbol.gensym("VAR");
    /** Get a value from a special variable
        and push it onto the operand stack.
        (nargs:0, stack pop:2/push:1) */
    static public final Symbol ENV_GET = Symbol.gensym("ENV-GET");
    /** Pop a value from the oprand stack
        and set it to a special variable.
        (nargs:0, stack pop:3/push:0) */
    static public final Symbol ENV_SET = Symbol.gensym("ENV-SET");
    /** Pop a value from the operand stack
        and bind it to a special variable.
        (nargs:0, stack pop:3/push:0) */
    static public final Symbol ENV_BIND = Symbol.gensym("ENV-BIND");
    /** Unbind a value from a special variable
        and push it onto the operand stack.
        (nargs:0, stack pop:2/push:1) */
    static public final Symbol ENV_UNBIND = Symbol.gensym("ENV-UNBIND");
    /** Switch current env to the child env.
        (nargs:2, stack pop:0/push:0) */
    static public final Symbol ENV_CHILD = Symbol.gensym("ENV-CHILD");
    /** FUNCALL: for a expr or a subr with N arguments.
        The operand stack image is as follows:
        <pre>
        if n == -1 then
         [env,list-of-args,fun,...] / [ret,...]
        else
         [env,arg(n-1),arg(n-2),...,arg(0),fun,...] / [ret,...]
        (where n is an argument passed to this CALL instruction)
        </pre>
        (nargs:1, stack pop:X/push:1) */
    static public final Symbol CALL = Symbol.gensym("CALL");
    /** FUNCALL: for a expr being compiled (including a recursive call).
        The operand stack image depends on a method info
        passed to this CALL-DIRECT instruction.
        (nargs:1, stack pop:X/push:1) */
    static public final Symbol CALL_DIRECT = Symbol.gensym("CALL-DIRECT");
    /** Push a ref to a expr being compiled onto the operand stack.
        (nargs:1, stack pop:0/push:1) */
    static public final Symbol COMPILED_EXPR = Symbol.gensym("COMPILED-EXPR");
    /** Pop a value from the stack
        and return to the caller with the value.
        (nargs:1, stack pop:1/push:0) */
    static public final Symbol RETURN = Symbol.gensym("RETURN");
    /** Conditional jump.
        (nargs:1, stack pop:1/push:0) */
    static public final Symbol IFEQ = Symbol.gensym("IFEQ");
    /** Conditional jump.
        (nargs:1, stack pop:1/push:0) */
    static public final Symbol IFNE = Symbol.gensym("IFNE");
    /** Unconditinal jump.
        (nargs:1, stack pop:0/push:0) */
    static public final Symbol GOTO = Symbol.gensym("GOTO");
    /** Get a value from a local variable
        and push it onto the operand stack.
        (nargs:1, stack pop:0/push:1) */
    static public final Symbol LOAD = Symbol.gensym("LOAD");
    /** Pop a value from the operand stack
        and set it to a local variable.
        (nargs:1or2, stack pop:1/push:0) */
    static public final Symbol STORE = Symbol.gensym("STORE");
    /** Pop a value from the operand stack
        and discard it.
        (nargs:1, stack pop:1/push:0) */
    static public final Symbol POP = Symbol.gensym("POP");
    /** Duplicate a value on the operand stack.
        (nargs:1, stack pop:1/push:2) */
    static public final Symbol DUP = Symbol.gensym("DUP");
    /** Push a constant (pooled in the java constant pool)
        on the operand stack.
        (nargs:1, stack pop:0/push:1) */
    static public final Symbol PUSH = Symbol.gensym("PUSH");
    /** Get a value from the java field
        and push it on the operand stack.
        (nargs:1, stack pop:0 or 1/push:1) */
    static public final Symbol GET = Symbol.gensym("GET");
    /** Pop a value from the operand stack
        and put it to the java field.
        (nargs:1, stack pop:1 or 2/push:0) */
    static public final Symbol PUT = Symbol.gensym("PUT");
    /** Invoke java method.
        Supported methods are those which can be invoked via
        invokevirtual, invokeinterface, or invokestatic.
        (nargs:1, stack pop:N/push:1) */
    static public final Symbol INVOKE = Symbol.gensym("INVOKE");
    /** Cast a reference on the operand stack.
        (nargs:1, stack pop:1/push:1) */
    static public final Symbol CHECKCAST = Symbol.gensym("CHECKCAST");
    /** Throw a exception on the stack.
        (nargs:0, stack pop:1/push:0) */
    static public final Symbol THROW = Symbol.gensym("THROW");
    /** Add an exception info to the exception table.
        (nargs:4, stack pop:0/push:0) */
    static public final Symbol CATCH = Symbol.gensym("CATCH");
    /** Label the start point of a try block.
        (nargs:1, stack pop:0/push:0) */
    static public final Symbol CATCH_FROM = Symbol.gensym("CATCH-FROM");
    /** Label the end point of a try block.
        (nargs:1, stack pop:0/push:0) */
    static public final Symbol CATCH_TO = Symbol.gensym("CATCH-TO");
    /** Label the start point of a exception handler.
        (nargs:1, stack pop:0/push:0) */
    static public final Symbol CATCH_HANDLER = Symbol.gensym("CATCH-HANDLER");

    private Insts() {}
}