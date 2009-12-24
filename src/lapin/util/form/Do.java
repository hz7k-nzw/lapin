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
package lapin.util.form;
import lapin.comp.DeclInfo;
import lapin.lang.Data;
//import lapin.lang.Env;
import lapin.lang.Lists;
import lapin.lang.ProgramException;
import lapin.lang.Symbol;
import lapin.lang.Symbols;
import lapin.lang.Values;
import lapin.util.ListBuilder;
//import lapin.util.Logger;

public final class Do {
    /*
     * ported from clisp-2.47/src/macros1.lisp
     * [function] do/do*-expand
     */

    static public Object expand(Object varClauseList,
                                Object exitClause, Object declsAndBody,
                                Symbol symLet, Symbol symSetq/*, Env env*/) {
        if (Data.isAtom(exitClause))
            throw new ProgramException
                ("exit clause must be a list: ~S.",
                 Lists.list(exitClause));

        Object bindList = Symbols.NIL;
        Object reInitList = Symbols.NIL;
        //Symbol testTag = env.lisp().getObarray().gensym("DO-TEST");
        //Symbol exitTag = env.lisp().getObarray().gensym("DO-EXIT");
        Symbol testTag = Symbol.gensym("DO-TEST");
        Symbol exitTag = Symbol.gensym("DO-EXIT");

        while (true) {
            if (Data.isAtom(varClauseList)) {
                if (varClauseList == Symbols.NIL) {
                    break;
                }
                else {
                    badSyntax(varClauseList);
                }
            }
            Object varClause = Lists.car(varClauseList);
            varClauseList = Lists.cdr(varClauseList);
            if (Data.isAtom(varClause)) {
                /*
                 * varClause: var
                 */
                bindList = Lists.cons(varClause, bindList);
            }
            else if (Lists.cdr(varClause) == Symbols.NIL) {
                /*
                 * varClause: (var)
                 */
                bindList = Lists.cons(Lists.car(varClause), bindList);
            }
            else if (Data.isAtom(Lists.cdr(varClause))) {
                /*
                 * varClause: (var . atom)
                 */
                badSyntax(varClause);
            }
            else if (Lists.cddr(varClause) == Symbols.NIL) {
                /*
                 * varClause: (var init)
                 */
                bindList = Lists.cons(varClause, bindList);
            }
            else if (Data.isAtom(Lists.cddr(varClause))) {
                /*
                 * varClause: (var init . atom)
                 */
                badSyntax(varClause);
            }
            else if (Lists.cdddr(varClause) == Symbols.NIL) {
                /*
                 * varClause: (var init step)
                 */
                bindList = Lists.cons(Lists.list(Lists.car(varClause),
                                                 Lists.cadr(varClause)),
                                      bindList);
                reInitList = Lists.list2(Lists.caddr(varClause),
                                         Lists.car(varClause),
                                         reInitList);
            }
            else {
                badSyntax(varClause);
            }
        }

        Object decls,body;
        {
            Values mv = DeclInfo.findDecls(declsAndBody);
            decls = mv.nth(0);
            body = mv.nth(1);
        }

        // make prog form:
        //
        //'(,symLet ,(nreverse bindList)
        //  ,@decls
        //  (PROG ()
        //     ,testTag
        //     (IF ,(car exitClause) (GO ,exitTag))
        //     ,@body
        //     (,symSetq ,@(nreverse reInitList))
        //     (GO ,testTag)
        //     ,exitTag
        //     (RETURN (PROGN ,@(rest exitClause)))))
        //
        Object ifForm = new ListBuilder()
            .append(Symbols.IF)
            .append(Lists.car(exitClause))
            .append(Lists.list(Symbols.GO, exitTag))
            .toList();
        Object setqForm = new ListBuilder()
            .append(symSetq)
            .concatList(Lists.nreverse(reInitList))
            .toList();
        Object goForm = Lists.list(Symbols.GO, testTag);
        Object returnForm = new ListBuilder()
            .append(Symbols.RETURN)
            .append(Lists.cons(Symbols.PROGN, Lists.cdr(exitClause)))
            .toList();
        Object progForm = new ListBuilder()
            .append(Symbols.PROG).append(Symbols.NIL)
            .append(testTag)
            .append(ifForm)
            .appendList(body)
            .append(setqForm)
            .append(goForm)
            .append(exitTag)
            .append(returnForm)
            .toList();
        Object letForm = new ListBuilder()
            .append(symLet).append(Lists.nreverse(bindList))
            .appendList(decls)
            .append(progForm)
            .toList();

        //if (Logger.tracelevelp(env))
        //    Logger.trace("[do-expand] ~S", Lists.list(letForm), env);

        return letForm;
    }
    static private void badSyntax(Object form) {
        throw new ProgramException
            ("invalid syntax: ~S.", Lists.list(form));
    }
    private Do() {}
}
