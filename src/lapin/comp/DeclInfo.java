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
import lapin.eval.Evaluator;
import lapin.lang.Data;
import lapin.lang.Lists;
import lapin.lang.ProgramException;
import lapin.lang.Symbol;
import lapin.lang.Symbols;
import lapin.lang.Values;

/**
 * Repository for the declaration info.
 */
public final class DeclInfo {
    /**
     * Split the specified list of forms into
     * decl forms and an actual body form.
     * @param declsAndBody List of forms
     * @return 2 values, decl forms and actual body form
     */
    static public Values findDecls(Object declsAndBody) {
        Object decls = Symbols.NIL;
        Object body = declsAndBody;
        while (true) {
            if (Data.isList(body) &&
                Data.isList(Lists.car(body)) && 
                Lists.caar(body) == Symbols.DECLARE) {
                decls = Lists.cons(Lists.car(body), decls);
                body = Lists.cdr(body);
            }
            else {
                if (decls != Symbols.NIL) {
                    decls = Lists.nreverse(decls);
                }
                return Values.currentValues().push(decls).push(body);
            }
        }
    }

    /** function name. */
    private final Symbol name;
    /** lambda list. */
    private final LambdaList ll;
    /**
     * specialVars: (var1 var2 ... varN),
     * type of varN: symbol.
     */
    Object specialVars;
    /**
     * varTypes: ((var1 . type1) (var2 . type2) ... (varN . typeN)),
     * type of varN: symbol,
     * type of typeN: symbol.
     */
    Object varTypes;
    /**
     * funcTypes: ((func1 . (paramTypes1 . retType1)) ...),
     * type of funcN: symbol,
     * type of paramTypesN: list of symbol,
     * type of retTypeN: symbol.
     */
    Object funcTypes;

    private DeclInfo(Symbol name, LambdaList ll) {
        this.name = name;
        this.ll = ll;
        this.specialVars = Symbols.NIL;
        this.varTypes = Symbols.NIL;
        this.funcTypes = Symbols.NIL;
    }
    /** Creates a DeclInfo. */
    public DeclInfo() {
        this(Symbols.NIL, null);
    }
    /**
     * Creates a child of this DeclInfo.
     * This method is used when compiling a function whose name
     * is <code>name</code> and whose lambdaList is <code>ll</code>.
     * @param name Name of a function
     * @param ll LambdaList for a function
     * @return Child of this DeclInfo
     */
    public DeclInfo child(Symbol name, LambdaList ll) {
        /*
         * If the declaration for a function is pushed to the child
         * and the function name of the declaration info is EQ to the
         * name set to the child, then the declaration is analyzed
         * by using the lambdaList set to the child and the resulting
         * (type) declarations for parameters of the function are pushed
         * to the child.
         */
        DeclInfo child = new DeclInfo(name, ll);
        child.specialVars = this.specialVars;
        child.varTypes = this.varTypes;
        child.funcTypes = this.funcTypes;
        return child;
    }
    /**
     * Creates a child of this DeclInfo.
     * <code>name</code> and <code>ll</code> is inherited
     * from this DeclInfo.
     * @return Child of this DeclInfo
     */
    public DeclInfo child() {
        return child(this.name, this.ll);
    }
    /**
     * Push the specified list of decl forms into this DeclInfo.
     * @param decls List of declaration forms
     * @return this DeclInfo
     */
    public DeclInfo pushDecls(Object decls) {
        /*
         * decls: ((DECLARE
         *          (SPECIAL . (spe1 spe2 ... speL))
         *          (FIXNUM . (fix1 fix2 ... fixM))
         *          (FLONUM . (flo1 flo2 ... floN))
         *          (CHARACTER . (char1 char2 ... charN)))
         *         (DECLARE ...)
         *         ...
         *        )
         */
        for (Object l = decls; !Lists.isEnd(l); l = Lists.cdr(l)) {
            Object declform = Lists.car(l);
            if (!Data.isList(declform) ||
                Lists.car(declform) != Symbols.DECLARE) {
                throw new ProgramException
                    ("malformed declare form: ~S", Lists.list(declform));
            }
            Object declbody = Lists.cdr(declform);
            Object specialVars = Lists.cdr
                (Lists.assq(Symbols.SPECIAL, declbody));
            Object  fixnumVars = Lists.cdr
                (Lists.assq(Symbols.FIXNUM, declbody));
            Object  flonumVars = Lists.cdr
                (Lists.assq(Symbols.FLONUM, declbody));
            Object  characterVars = Lists.cdr
                (Lists.assq(Symbols.CHARACTER, declbody));
            pushSpecialVars(specialVars);
            pushVarTypes(fixnumVars, Symbols.FIXNUM);
            pushVarTypes(flonumVars, Symbols.FLONUM);
            pushVarTypes(characterVars, Symbols.CHARACTER);
        }
        return this;
    }
    /**
     * Push the specified symbols into the list of special variables.
     * @param specialVars List of symbols
     * @return this DeclInfo
     */
    public DeclInfo pushSpecialVars(Object specialVars) {
        for (Object l = specialVars; !Lists.isEnd(l); l = Lists.cdr(l)) {
            Object obj = Lists.car(l);
            if (Data.isSymbol(obj))
                pushSpecialVar(Data.symbol(obj));
            else
                throw new ProgramException
                    ("unsupported object: ~S", Lists.list(obj));
        }
        return this;
    }
    /**
     * Push the specified symbols into the list of variables
     * with the specified type.
     * @param vars List of symbols
     * @param type Symbol which represents the type of variable
     * @return this DeclInfo
     */
    public DeclInfo pushVarTypes(Object vars, Symbol type) {
        for (Object l = vars; !Lists.isEnd(l); l = Lists.cdr(l)) {
            Object obj = Lists.car(l);
            if (Data.isSymbol(obj))
                pushVarType(Data.symbol(obj), type);
            else if (Data.isPair(obj))
                pushFuncType(Data.pair(obj), type);
            else
                throw new ProgramException
                    ("unsupported object: ~S", Lists.list(obj));
        }
        return this;
    }
    /**
     * Push the specified symbol into the list of special variables.
     * @param var Symbol
     * @return this DeclInfo
     */
    public DeclInfo pushSpecialVar(Symbol var) {
        specialVars = Lists.cons(var, specialVars);
        return this;
    }
    /**
     * Push the specified symbol into the list of variables
     * with the specified type.
     * @param var Symbol
     * @param type Symbol which represents the type of variable
     * @return this DeclInfo
     */
    public DeclInfo pushVarType(Symbol var, Symbol type) {
        // pair: (var . type)
        Object pair = Lists.cons(var, type);
        varTypes = Lists.cons(pair, varTypes);
        return this;
    }
    /**
     * Push the specified function declaration into
     * the list of function declarations with the specified
     * return type.
     * @param func Function declaration; for example <code>(fib fixnum)</code>
     * @param type Symbol which represents the return type of the function
     * @return this DeclInfo
     */
    public DeclInfo pushFuncType(Object func, Symbol type) {
        // func: (name . paramTypes)
        // -> pair: (name . (paramTypes . type))
        Symbol name = Data.symbol(Lists.car(func));
        Object paramTypes = Lists.cdr(func);
        Object pair = Lists.list2(name, paramTypes, type);
        funcTypes = Lists.cons(pair, funcTypes);
        if (this.name == name)
            pushParamTypes(this.ll, paramTypes);
        return this;
    }
    /**
     * Push variables defined by the specified lambdaList <code>ll</code>
     * into the list of variables with the type specified by the list
     * <code>paramTypes</code>.
     * @param ll LambdaList, in which the pattern of variables is defined
     * @param paramTypes List of types, which matches the pattern defined by
     *        <code>ll</code>.
     * @return this DeclInfo
     */
    public DeclInfo pushParamTypes(LambdaList ll, Object paramTypes) {
        Object var;
        Object initform;
        Symbol svar;
        Symbol key;
        Object paramType;
        //// push whole vars
        //if (ll.isWhole()) {
        //    var = ll.wholeVar();
        //    if (Data.isSymbol(var)) {
        //        pushVarType(Data.symbol(var), Symbols.T);
        //    }
        //    else {
        //        pushParamTypes(Data.lambdaList(var), paramTypes);
        //    }
        //}
        // push required vars
        for (int i = 0; i < ll.reqCount(); i++) {
            var = ll.reqVar(i);
            if (Lists.isEnd(paramTypes)) {
                if (Data.isSymbol(var)) {
                    pushVarType(Data.symbol(var), Symbols.T);
                }
                else {
                    pushParamTypes(Data.lambdaList(var), Symbols.NIL);
                }
            }
            else {
                paramType = Lists.car(paramTypes);
                if (Data.isSymbol(var)) {
                    pushVarType(Data.symbol(var), Data.symbol(paramType));
                }
                else {
                    pushParamTypes(Data.lambdaList(var), paramType);
                }
            }
            paramTypes = Lists.cdr(paramTypes);
        }
        // push optional vars
        for (int i = 0; i < ll.optCount(); i++) {
            var = ll.optVar(i);
            if (Lists.isEnd(paramTypes)) {
                if (Data.isSymbol(var)) {
                    pushVarType(Data.symbol(var), Symbols.T);
                }
                else {
                    pushParamTypes(Data.lambdaList(var), Symbols.NIL);
                }
            }
            else {
                paramType = Lists.car(paramTypes);
                if (Data.isSymbol(var)) {
                    pushVarType(Data.symbol(var), Data.symbol(paramType));
                }
                else {
                    pushParamTypes(Data.lambdaList(var), paramType);
                }
            }
            paramTypes = Lists.cdr(paramTypes);
        }
        //// push rest vars
        //if (ll.isRest()) {
        //    var = ll.restVar();
        //    if (Data.isSymbol(var)) {
        //        pushVarType(Data.symbol(var), Symbols.T);
        //    }
        //    else {
        //        pushParamTypes(Data.lambdaList(var), paramTypes);
        //    }
        //}
        // key vars
        for (int i = 0; i < ll.keyCount(); i++) {
            var = ll.keyVar(i);
            key = ll.keyKey(i);
            svar = ll.keySvar(i);
            Object ret = Lists.memq(key, paramTypes);
            if (ret == Symbols.NIL) {
                if (Data.isSymbol(var)) {
                    pushVarType(Data.symbol(var), Symbols.T);
                }
                else {
                    pushParamTypes(Data.lambdaList(var), Symbols.NIL);
                }
            }
            else {
                paramType = Lists.cadr(ret);
                if (Data.isSymbol(var)) {
                    pushVarType(Data.symbol(var), Data.symbol(paramType));
                }
                else {
                    pushParamTypes(Data.lambdaList(var), paramType);
                }
            }
        }
        return this;
    }
    /** Returns true if the specified symbol <code>var</code> is 
        in the list of the special variables. */
    public boolean isSpecial(Symbol var) {
        Object ret = Lists.memq(var, specialVars);
        return ret != Symbols.NIL;
    }
    /**
     * Returns NIL if for any symbol in <code>vars</code>
     * {@link #isSpecial isSpecial} returns <code>false</code>;
     * otherwise returns the sublist of <code>vars</code>
     * beginning with a variable for which
     * {@link #isSpecial isSpecial} returns <code>true</code>.
     */
    public Object findSpecial(Object vars) {
        for (Object l = vars; !Lists.isEnd(l); l = Lists.cdr(l)) {
            Symbol var = Data.symbol(Lists.car(l));
            if (isSpecial(var))
                return l;
        }
        return Symbols.NIL;
    }
    /** Returns the type for the specified symbol <code>var</code>. */
    public Symbol typeOfVar(Symbol var) {
        Object pair = Lists.assq(var, varTypes);
        if (pair == Symbols.NIL)
            // declaration not found
            // -> returns T (which corresponds to java.lang.Object)
            return Symbols.T;
        else
            // pair: (var . type)
            // -> returns type
            return Data.symbol(Lists.cdr(pair));
    }
    /** Returns the type for the specified symbol <code>var</code>. */
    public Class javaTypeOfVar(Symbol var, boolean allowPrimitive) {
        return toJavaType(typeOfVar(var), allowPrimitive);
    }
    /**
     * Returns the return type of the function whose name is
     * stored in this DeclInfo.
     * @see #child(Symbol,LambdaList)
     */
    public Symbol retTypeOfFunc() {
        Object pair = Lists.assq(this.name, funcTypes);
        if (pair == Symbols.NIL)
            // declaration not found
            // -> returns T (which corresponds to java.lang.Object)
            return Symbols.T;
        else
            // pair: (name . (paramTypes . retType))
            // -> returns retType
            return Data.symbol(Lists.cddr(pair));
    }
    /**
     * Returns the return type of the function whose name is
     * stored in this DeclInfo.
     * @see #child(Symbol,LambdaList)
     */
    public Class javaRetTypeOfFunc(boolean allowPrimitive) {
        return toJavaType(retTypeOfFunc(), allowPrimitive);
    }
    /**
     * Returns the list of parameter types of the function
     * whose name is stored in this DeclInfo.
     * @see #child(Symbol,LambdaList)
     */
    public Object paramTypesOfFunc() {
        Object pair = Lists.assq(this.name, funcTypes);
        if (pair == Symbols.NIL)
            // declaration not found
            // -> returns empty list
            return Symbols.NIL;
        else
            // pair: (name . (paramTypes . retType))
            // -> returns paramTypes (list of types)
            return Lists.cadr(pair);
    }
    /**
     * Returns the list of parameter types of the function
     * whose name is stored in this DeclInfo.
     * @see #child(Symbol,LambdaList)
     */
    public Object javaParamTypesOfFunc(boolean allowPrimitive) {
        Object paramTypes = paramTypesOfFunc();
        Object javaTypes = Symbols.NIL;
        for (Object l = paramTypes; !Lists.isEnd(l); l = Lists.cdr(l)) {
            Class javaType = toJavaType(Data.symbol(Lists.car(l)),
                                        allowPrimitive);
            javaTypes = Lists.cons(javaType, javaTypes);
        }
        return Lists.nreverse(javaTypes);
    }
    private Class toJavaType(Symbol type, boolean allowPrimitive) {
        if (type == Symbols.FIXNUM) {
            return allowPrimitive ? int.class : Integer.class;
        }
        else if (type == Symbols.FLONUM) {
            return allowPrimitive ? double.class : Double.class;
        }
        else if (type == Symbols.CHARACTER) {
            return allowPrimitive ? char.class : Character.class;
        }
        else if (type == Symbols.T) {
            return Object.class;
        }
        else {
            throw new ProgramException
                ("unknown type: ~S", Lists.list(type));
        }
    }
}

