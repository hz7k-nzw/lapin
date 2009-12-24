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
package lapin.function;
import lapin.lang.Data;
import lapin.lang.Env;
import lapin.lang.Lists;
import lapin.lang.NotReachedException;
import lapin.lang.Package;
import lapin.lang.ProgramException;
import lapin.lang.Symbol;
import lapin.lang.Symbols;
//import lapin.util.Logger;
import java.util.ArrayList;
import java.util.Iterator;

/**
 * This class represents a parameter spec of function.
 */
public final class LambdaList {

    static private final int REQUIRED = 1;
    static private final int OPTIONAL = 2;
    static private final int     REST = 3;
    static private final int  KEYWORD = 4;
    static private final int ALLOW_OTHER_KEYS = 5;
    static private final int      AUX = 6;
    static private final int    WHOLE = 7;
    static private final int     BODY = 8;
    static private final int    BEGIN = -1;
    static private final int      END = -2;

    /** Uninterned symbol which represents the omitted svar. */
    static public final Symbol NOT_SPECIFIED
        = Symbol.gensym("_LL-VAR-NOT-SPECIFIED_");

    private Whole whole;
    private Required[] required;
    private Optional[] optional;
    private Rest rest;
    private Keyword[] keyword;
    private boolean allowOtherKeys;
    private Aux[] aux;

    private Object params = Symbols.NIL;
    private Object vars = null;
    private int genVarCount = 0;

    /**
     * LambdaList constructor for {@link Subr}.
     * @param reqCount Number of required parameters
     * @param optCount Number of optional parameters
     * @param rest if true, then a function uses rest parameter
     * @param keys List of keywords for keyword parameters
     * @param allowOtherKeys if true, then a function accepts a keyword
     *        not defined in <code>keys</code> 
     */
    public LambdaList(int reqCount, int optCount, boolean rest,
                      Object keys, boolean allowOtherKeys) {
        int keyCount = Lists.length(keys);

        // whole: not supported in SUBR
        this.whole = null;
        // req
        this.required = new Required[reqCount];
        for (int i = 0; i < reqCount; i++) {
            this.required[i] = new Required(genVar());
        }
        // opt
        this.optional = new Optional[optCount];
        for (int i = 0; i < optCount; i++) {
            this.optional[i] = new Optional
                (genVar(), Symbols.NIL, genVar());
        }
        // rest
        if (rest) {
            this.rest = new Rest(genVar());
        } else {
            this.rest = null;
        }
        // key
        this.keyword = new Keyword[keyCount];
        this.allowOtherKeys = false;
        for (int i = 0; i < keyCount; i++) {
            Symbol key = Data.symbol(Lists.car(keys));
            //System.out.println("key = "+key);
            //if (key == Symbols.LK_ALLOW_OTHER_KEYS) {
            //    //System.out.println("allowOtherKeys = true");
            //    this.allowOtherKeys = true;
            //    keys = Lists.cdr(keys);
            //    if (!Lists.isEnd(keys)) {
            //        throw new ProgramException
            //            ("no variable is allowed right after "+
            //             "&allow-other-keys: ~S.",
            //             Lists.list(keys));
            //    }
            //}
            //else {
            //    this.keyword[i] = new Keyword
            //        (genVar(), key, Symbols.NIL, genVar());
            //    keys = Lists.cdr(keys);
            //}
            this.keyword[i] = new Keyword
                (genVar(), key, Symbols.NIL, genVar());
            keys = Lists.cdr(keys);
        }
        //
        this.allowOtherKeys = allowOtherKeys;
        // aux: not supported in SUBR
        this.aux = new Aux[0];
    }
    /**
     * LambdaList constructor for {@link Expr}.
     * @param params List which represents a LambdaList
     * @param env
     */
    public LambdaList(Object params, Env env) {
        //if (Logger.tracelevelp(env))
        //    Logger.trace("[lambdaList] params=~S",
        //                 Lists.list(params), env);
        if (params == null)
            throw new NullPointerException("params is null");
        if (env == null)
            throw new NullPointerException("env is null");
        this.params = params;

        /*
         * parse params
         */
        Object obj = null;
        Object var = null;
        Object initform = null;
        Symbol svar = null;
        Symbol key = null;
        ArrayList tmp = new ArrayList();
        int wholeCount = 0;
        int reqCount = 0;
        int optCount = 0;
        int restCount = 0;
        int keyCount = 0;
        int allowOtherKeysCount = 0;
        int auxCount = 0;
        int step = BEGIN;
        while (true) {
            switch (step) {
            case BEGIN:
                if (params == Symbols.NIL) {
                    step = END;
                }
                else if (Data.isAtom(params)) {
                    step = REST;
                    params = Lists.list(params);
                }
                else {
                    obj = Lists.car(params);
                    if (obj == Symbols.LK_WHOLE) {
                        step = WHOLE;
                        params = Lists.cdr(params);
                    }
                    else if (obj == Symbols.LK_OPTIONAL) {
                        step = OPTIONAL;
                        params = Lists.cdr(params);
                    }
                    else if (obj == Symbols.LK_REST ||
                             obj == Symbols.LK_BODY) {
                        step = REST;
                        params = Lists.cdr(params);
                    }
                    else if (obj == Symbols.LK_KEY) {
                        step = KEYWORD;
                        params = Lists.cdr(params);
                    }
                    else if (obj == Symbols.LK_AUX) {
                        step = AUX;
                        params = Lists.cdr(params);
                    }
                    else {
                        step = REQUIRED;
                    }
                }
                break;
            case WHOLE:
                /*
                 * params pattern:
                 * [&whole var]
                 */
                if (Lists.isEnd(params)) {
                    throw new ProgramException
                        ("whole parameter not exists", Symbols.NIL);
                }
                obj = Lists.car(params);
                var = Data.isPair(obj)
                    ? new LambdaList(obj, env)
                    : obj;
                tmp.add(new Whole(var));
                wholeCount++;
                params = Lists.cdr(params);
                if (params == Symbols.NIL) {
                    step = END;
                }
                else if (Data.isAtom(params)) {
                    step = REST;
                    params = Lists.list(params);
                }
                else {
                    obj = Lists.car(params);
                    if (obj == Symbols.LK_OPTIONAL) {
                        step = OPTIONAL;
                        params = Lists.cdr(params);
                    }
                    else if (obj == Symbols.LK_REST ||
                             obj == Symbols.LK_BODY) {
                        step = REST;
                        params = Lists.cdr(params);
                    }
                    else if (obj == Symbols.LK_KEY) {
                        step = KEYWORD;
                        params = Lists.cdr(params);
                    }
                    else if (obj == Symbols.LK_AUX) {
                        step = AUX;
                        params = Lists.cdr(params);
                    }
                    else {
                        step = REQUIRED;
                    }
                }
                break;
            case REQUIRED:
                /*
                 * params pattern:
                 * {var}*
                 */
                if (params == Symbols.NIL) {
                    step = END;
                }
                else if (Data.isAtom(params)) {
                    step = REST;
                    params = Lists.list(params);
                }
                else { /* !Lists.isEnd(params) */
                    obj = Lists.car(params);
                    if (obj == Symbols.LK_OPTIONAL) {
                        step = OPTIONAL;
                        params = Lists.cdr(params);
                    }
                    else if (obj == Symbols.LK_REST ||
                             obj == Symbols.LK_BODY) {
                        step = REST;
                        params = Lists.cdr(params);
                    }
                    else if (obj == Symbols.LK_KEY) {
                        step = KEYWORD;
                        params = Lists.cdr(params);
                    }
                    else if (obj == Symbols.LK_AUX) {
                        step = AUX;
                        params = Lists.cdr(params);
                    }
                    else {
                        var = Data.isPair(obj)
                            ? new LambdaList(obj, env)
                            : obj;
                        tmp.add(new Required(var));
                        reqCount++;

                        step = REQUIRED;
                        params = Lists.cdr(params);
                    }
                }
                break;
            case OPTIONAL:
                /*
                 * params pattern:
                 * [&optional {var | (var [initform [svar]])}*]
                 */
                if (params == Symbols.NIL) {
                    step = END;
                }
                else if (Data.isAtom(params)) {
                    step = REST;
                    params = Lists.list(params);
                }
                else { /* !Lists.isEnd(params) */
                    obj = Lists.car(params);
                    if (obj == Symbols.LK_REST ||
                        obj == Symbols.LK_BODY) {
                        step = REST;
                        params = Lists.cdr(params);
                    }
                    else if (obj == Symbols.LK_KEY) {
                        step = KEYWORD;
                        params = Lists.cdr(params);
                    }
                    else if (obj == Symbols.LK_AUX) {
                        step = AUX;
                        params = Lists.cdr(params);
                    }
                    else {
                        if (Data.isSymbol(obj)) {
                            /*
                             * obj pattern:
                             * var <symbol>
                             */
                            var = Data.symbol(obj);
                            initform = Symbols.NIL;
                            svar = NOT_SPECIFIED;
                        }
                        else {
                            /*
                             * obj pattern:
                             * (var [initform [svar]])
                             */
                            var = Data.isPair(Lists.car(obj))
                                ? new LambdaList(Lists.car(obj), env)
                                : Lists.car(obj);
                            initform = Lists.cadr(obj);
                            svar = Lists.isEnd(Lists.cddr(obj))
                                ? NOT_SPECIFIED
                                : Data.symbol(Lists.caddr(obj));
                        }
                        tmp.add(new Optional(var, initform, svar));
                        optCount++;
                        step = OPTIONAL;
                        params = Lists.cdr(params);
                    }
                }
                break;
            case REST:
                /*
                 * params pattern:
                 * [&rest var]
                 */
                if (Lists.isEnd(params)) {
                    throw new ProgramException
                        ("rest parameter not exists", Symbols.NIL);
                }
                obj = Lists.car(params);
                var = Data.isPair(obj)
                    ? new LambdaList(obj, env)
                    : obj;
                tmp.add(new Rest(var));
                restCount++;
                params = Lists.cdr(params);
                if (Lists.isEnd(params)) {
                    step = END;
                }
                else {
                    obj = Lists.car(params);
                    if (obj == Symbols.LK_KEY) {
                        step = KEYWORD;
                        params = Lists.cdr(params);
                    }
                    else if (obj == Symbols.LK_AUX) {
                        step = AUX;
                        params = Lists.cdr(params);
                    }
                    else {
                        throw new ProgramException
                            ("unacceptable variable: ~S"+
                             " (expected: &key, &aux or empty)",
                             Lists.list(obj));
                    }
                }
                break;
            case KEYWORD:
                /*
                 * params pattern:
                 * [&key {var | ({var | (keyword var)} [initform [svar]])}*]
                 */
                if (Lists.isEnd(params)) {
                    step = END;
                }
                else {
                    obj = Lists.car(params);
                    if (obj == Symbols.LK_ALLOW_OTHER_KEYS) {
                        step = ALLOW_OTHER_KEYS;
                        params = Lists.cdr(params);
                    }
                    else if (obj == Symbols.LK_AUX) {
                        step = AUX;
                        params = Lists.cdr(params);
                    }
                    else {
                        if (Data.isSymbol(obj)) {
                            /*
                             * obj pattern:
                             * var <symbol>
                             */
                            var = Data.symbol(obj);
                            initform = Symbols.NIL;
                            svar = NOT_SPECIFIED;
                            key = toKeyword(Data.symbol(var), env);
                        }
                        else {
                            /*
                             * obj pattern:
                             * ({var | (keyword var)} [initform [svar]])
                             */
                            var = Data.isSymbol(Lists.car(obj))
                                ? Lists.car(obj)
                                : Data.isSymbol(Lists.cadar(obj))
                                ? Lists.cadar(obj)
                                : new LambdaList(Lists.cadar(obj), env);
                            initform = Lists.cadr(obj);
                            svar = Lists.isEnd(Lists.cddr(obj))
                                ? NOT_SPECIFIED
                                : Data.symbol(Lists.caddr(obj));
                            key = Data.isSymbol(Lists.car(obj))
                                ? toKeyword(Data.symbol(var), env)
                                : Data.symbol(Lists.caar(obj));
                        }
                        tmp.add(new Keyword(var, key, initform, svar));
                        keyCount++;
                        step = KEYWORD;
                        params = Lists.cdr(params);
                    }
                }
                break;
            case ALLOW_OTHER_KEYS:
                //System.out.println("in ALLOW_OTHER_KEYS");
                allowOtherKeysCount++;
                if (Lists.isEnd(params)) {
                    //System.out.println("-> END");
                    step = END;
                }
                else {
                    obj = Lists.car(params);
                    //System.out.println("-> obj="+obj);
                    if (obj == Symbols.LK_AUX) {
                        step = AUX;
                        params = Lists.cdr(params);
                    }
                    else {
                        throw new ProgramException
                            ("unacceptable variable: ~S"+
                             " (expected: &aux or empty)",
                             Lists.list(obj));
                    }
                }
                break;
            case AUX:
                /*
                 * params pattern:
                 * [&aux {var | (var [initform])}*]
                 */
                if (Lists.isEnd(params)) {
                    step = END;
                }
                else {
                    obj = Lists.car(params);
                    if (Data.isSymbol(obj)) {
                        /*
                         * obj pattern:
                         * var <symbol>
                         */
                        var = Data.symbol(obj);
                        initform = Symbols.NIL;
                    }
                    else {
                        /*
                         * obj pattern:
                         * (var [initform])
                         */
                        var = Data.isSymbol(Lists.car(obj))
                            ? Lists.car(obj)
                            : new LambdaList(Lists.car(obj), env);
                        initform = Lists.cadr(obj);
                    }
                    tmp.add(new Aux(var, initform));
                    auxCount++;
                    step = AUX;
                    params = Lists.cdr(params);
                }
                break;
            case END:
                Iterator it = tmp.iterator();
                this.required = new Required[reqCount];
                this.optional = new Optional[optCount];
                this.keyword = new Keyword[keyCount];
                this.aux = new Aux[auxCount];

                // fill whole param
                this.whole = (wholeCount > 0) ? (Whole) it.next() : null;
                // fill required params
                for (int i = 0; i < reqCount; i++)
                    this.required[i] = (Required) it.next();
                // fill optional params
                for (int i = 0; i < optCount; i++)
                    this.optional[i] = (Optional) it.next();
                // fill rest param
                this.rest = (restCount > 0) ? (Rest) it.next() : null;
                // fill keyword params
                for (int i = 0; i < keyCount; i++)
                    this.keyword[i] = (Keyword) it.next();
                // fill allowOtherKeys flag
                this.allowOtherKeys = (allowOtherKeysCount > 0);
                // fill aux params
                for (int i = 0; i < auxCount; i++)
                    this.aux[i] = (Aux) it.next();
                return;
            default:
                throw new NotReachedException
                    ("unacceptable step: "+step, Symbols.NIL);
            }
        }
    }

    private Symbol genVar() {
        return Symbol.gensym("VAR-"+(genVarCount++));
    }
    private Symbol toKeyword(Symbol var, Env env) {
        return Data.symbol
            (env.lisp().getObarray().intern
             (Package.KEYWORD, var.pname()).nth(0));
    }

    /**
     * Returns the parameter spec list passed to the 
     * constructor of this class for the EXPR.
     * If this object is constructed via the constructor 
     * for the SUBR, NIL is returned.
     */
    public Object params() {
        return params;
    }
    /** Returns a flat list of variables held by this LambdaList. */
    public Object vars() {
        if (this.vars != null)
            return this.vars;

        Object ret = Symbols.NIL;
        // whole
        if (this.whole != null)
            ret = Lists.nconc(this.whole.vars(), ret);
        // required
        for (int i = 0; i < this.required.length; i++)
            ret = Lists.nconc(this.required[i].vars(), ret);
        // optional
        for (int i = 0; i < this.optional.length; i++)
            ret = Lists.nconc(this.optional[i].vars(), ret);
        // rest
        if (this.rest != null)
            ret = Lists.nconc(this.rest.vars(), ret);
        // keyword
        for (int i = 0; i < this.keyword.length; i++)
            ret = Lists.nconc(this.keyword[i].vars(), ret);
        // aux
        for (int i = 0; i < this.aux.length; i++)
            ret = Lists.nconc(this.aux[i].vars(), ret);

        this.vars = ret;

        return ret;
    }
    /** Returns true if this LambdaList is nested. */
    public boolean isNested() {
        // check whole param
        if (this.whole != null)
            if (!Data.isSymbol(this.whole.var))
                return true;
        // check required params
        for (int i = 0; i < this.required.length; i++)
            if (!Data.isSymbol(this.required[i].var))
                return true;
        // check optional params
        for (int i = 0; i < this.optional.length; i++)
            if (!Data.isSymbol(this.optional[i].var))
                return true;
        // check rest param
        if (this.rest != null)
            if (!Data.isSymbol(this.rest.var))
                return true;
        // check keyword params
        for (int i = 0; i < this.keyword.length; i++)
            if (!Data.isSymbol(this.keyword[i].var))
                return true;
        // check aux params
        for (int i = 0; i < this.aux.length; i++)
            if (!Data.isSymbol(this.aux[i].var))
                return true;
        return false;
    }
    /**
     * Returns a variable for either required or optional
     * parameter at the specified index.
     * @param i index, which must satisfy
     *        <code>
     *        0 &lt;= i &amp;&amp;
     *        i &lt; {@link #reqCount reqCount} + {@link #optCount optCount}
     *        </code>
     * @return Symbol which represents the variable,
     *         or sub LambdaList if this LambdaList is nested
     */
    public Object var(int i) {
        if (i < reqCount())
            return reqVar(i);
        else
            return optVar(i - reqCount());
    }
    /** Returns true if the whole parameter exists. */
    public boolean isWhole() {
        return whole != null;
    }
    /**
     * Returns a variable for the whole parameter.
     * @return Symbol which represents the variable
     */
    public Object wholeVar() {
        return whole.var;
    }
    /** Returns the number of the required parameters. */
    public int reqCount() {
        return required.length;
    }
    /**
     * Returns a variable for the required parameter
     * at the specified index.
     * @param i index, which must satisfy
     *        <code>
     *        0 &lt;= i &amp;&amp;
     *        i &lt; {@link #reqCount reqCount}
     *        </code>
     * @return Symbol which represents the variable,
     *         or sub LambdaList if this LambdaList is nested
     */
    public Object reqVar(int i) {
        return required[i].var;
    }
    /** Returns the number of the optional parameters. */
    public int optCount() {
        return optional.length;
    }
    /**
     * Returns a variable for the optional parameter
     * at the specified index.
     * @param i index, which must satisfy
     *        <code>
     *        0 &lt;= i &amp;&amp;
     *        i &lt; {@link #optCount optCount}
     *        </code>
     * @return Symbol which represents the variable,
     *         or sub LambdaList if this LambdaList is nested
     */
    public Object optVar(int i) {
        return optional[i].var;
    }
    /**
     * Returns a initform for the optional parameter
     * at the specified index.
     * @param i index, which must satisfy
     *        <code>
     *        0 &lt;= i &amp;&amp;
     *        i &lt; {@link #optCount optCount}
     *        </code>
     * @return Initform
     */
    public Object optInitform(int i) {
        return optional[i].initform;
    }
    /**
     * Returns a suppliedp variable for the optional parameter
     * at the specified index.
     * When the suppliedp variable is not defined in this LambdaList,
     * {@link #NOT_SPECIFIED} is returned.
     * @param i index, which must satisfy
     *        <code>
     *        0 &lt;= i &amp;&amp;
     *        i &lt; {@link #optCount optCount}
     *        </code>
     * @return Symbol which represents the variable
     */
    public Symbol optSvar(int i) {
        return optional[i].svar;
    }
    /** Returns true if the rest parameter exists. */
    public boolean isRest() {
        return rest != null;
    }
    /**
     * Returns a variable for the rest parameter.
     * @return Symbol which represents the variable,
     *         or sub LambdaList if this LambdaList is nested
     */
    public Object restVar() {
        return rest.var;
    }
    /** Returns the number of the keyword parameters. */
    public int keyCount() {
        return keyword.length;
    }
    /** Returns true if keywords not included in the definition
        of this LambdaList are allowed. */
    public boolean allowOtherKeys() {
        return allowOtherKeys;
    }
    /**
     * Returns a variable for the keyword parameter
     * at the specified index.
     * @param i index, which must satisfy
     *        <code>
     *        0 &lt;= i &amp;&amp;
     *        i &lt; {@link #keyCount keyCount}
     *        </code>
     * @return Symbol which represents the variable,
     *         or sub LambdaList if this LambdaList is nested
     */
    public Object keyVar(int i) {
        return keyword[i].var;
    }
    /**
     * Returns a keyword for the keyword parameter
     * at the specified index.
     * @param i index, which must satisfy
     *        <code>
     *        0 &lt;= i &amp;&amp;
     *        i &lt; {@link #keyCount keyCount}
     *        </code>
     * @return Symbol which represents the keyword
     */
    public Symbol keyKey(int i) {
        return keyword[i].key;
    }
    /**
     * Returns a initform for the keyword parameter
     * at the specified index.
     * @param i index, which must satisfy
     *        <code>
     *        0 &lt;= i &amp;&amp;
     *        i &lt; {@link #keyCount keyCount}
     *        </code>
     * @return Initform
     */
    public Object keyInitform(int i) {
        return keyword[i].initform;
    }
    /**
     * Returns a suppliedp variable for the keyword parameter
     * at the specified index.
     * When the suppliedp variable is not defined in this LambdaList,
     * {@link #NOT_SPECIFIED} is returned.
     * @param i index, which must satisfy
     *        <code>
     *        0 &lt;= i &amp;&amp;
     *        i &lt; {@link #keyCount keyCount}
     *        </code>
     * @return Symbol which represents the variable
     */
    public Symbol keySvar(int i) {
        return keyword[i].svar;
    }
    /** Returns the number of the aux parameters. */
    public int auxCount() {
        return aux.length;
    }
    /**
     * Returns a variable for the aux parameter
     * at the specified index.
     * @param i index, which must satisfy
     *        <code>
     *        0 &lt;= i &amp;&amp;
     *        i &lt; {@link #auxCount auxCount}
     *        </code>
     * @return Symbol which represents the variable,
     *         or sub LambdaList if this LambdaList is nested
     */
    public Object auxVar(int i) {
        return aux[i].var;
    }
    /**
     * Returns a initform for the aux parameter
     * at the specified index.
     * @param i index, which must satisfy
     *        <code>
     *        0 &lt;= i &amp;&amp;
     *        i &lt; {@link #auxCount auxCount}
     *        </code>
     * @return Initform
     */
    public Object auxInitform(int i) {
        return aux[i].initform;
    }

    /*
     * nested classes
     */

    static abstract class Param {
        final Object var;
        Param(Object var) {
            this.var = checkVar(var);
        }
        Object checkVar(Object var) {
            if (var == null) {
                throw new NullPointerException("var is null");
            }
            else if (Data.isSymbol(var)) {
                Symbol sym = Data.symbol(var);
                if (sym.isSelfeval() || Symbols.isLambdaListKeyword(sym))
                    throw new ProgramException
                        ("illegal symbol specified for var: ~S.",
                         Lists.list(var));
            }
            else {
                if (!Data.isLambdaList(var))
                    throw new ProgramException
                        ("illegal object specified for var: ~S.",
                         Lists.list(var));
            }
            return var;
        }
        Object checkInitform(Object initform) {
            if (initform == null)
                throw new NullPointerException("initform is null");
            return initform;
        }
        Symbol checkSvar(Symbol svar) {
            if (svar == null)
                throw new NullPointerException("svar is null");
            else if (svar.isSelfeval())
                throw new ProgramException
                    ("illegal symbol specified for svar: ~S.",
                     Lists.list(svar));
            return svar;
        }
        Symbol checkKey(Symbol key) {
            if (key == null)
                throw new NullPointerException("key is null");
            else if (!key.isSelfeval() ||
                     key == Symbols.NIL || key == Symbols.T)
                throw new ProgramException
                    ("illegal symbol specified for key: ~S.",
                     Lists.list(key));
            return key;
        }
        Object vars() {
            if (Data.isSymbol(var))
                return Lists.list(var);
            else
                return Data.lambdaList(var).vars();
        }
    } /* end of Param */

    static class Required extends Param {
        Required(Object var) {
            super(var);
        }
    } /* end of Required */

    static class Optional extends Param {
        final Object initform;
        final Symbol svar;
        Optional(Object var, Object initform, Symbol svar) {
            super(var);
            this.initform = checkInitform(initform);
            this.svar = checkSvar(svar);
        }
        Object vars() {
            if (svar == NOT_SPECIFIED)
                return super.vars();
            else
                if (Data.isSymbol(var))
                    return Lists.list(svar, var);
                else
                    return Lists.cons(svar, Data.lambdaList(var).vars());
        }
    } /* end of Optional */

    static class Rest extends Param {
        Rest(Object var) {
            super(var);
        }
    } /* end of Rest */

    static class Keyword extends Param {
        final Symbol key;
        final Object initform;
        final Symbol svar;
        Keyword(Object var, Symbol key, Object initform, Symbol svar) {
            super(var);
            this.key = checkKey(key);
            this.initform = checkInitform(initform);
            this.svar = checkSvar(svar);
        }
        Object vars() {
            if (svar == NOT_SPECIFIED)
                return super.vars();
            else
                if (Data.isSymbol(var))
                    return Lists.list(svar, var);
                else
                    return Lists.cons(svar, Data.lambdaList(var).vars());
        }
    } /* end of Keyword */

    static class Aux extends Param {
        final Object initform;
        Aux(Object var, Object initform) {
            super(var);
            this.initform = checkInitform(initform);
        }
    } /* end of Aux */

    static class Whole extends Param {
        Whole(Object var) {
            super(var);
        }
    } /* end of Whole */

} /* end of LambdaList */
