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
package lapin.io;
import lapin.function.Function;
import lapin.lang.Data;
import lapin.lang.Lists;
import lapin.lang.Symbol;
import lapin.lang.Symbols;

/**
 * A data structure that is used to control {@link Reader}.
 */
public class Readtable implements lapin.lang.Array {
    static public final int TYPE_ILLEGAL = 1;
    static public final int TYPE_WHITESPACE = 2;
    static public final int TYPE_CONSTITUENT = 3;
    static public final int TYPE_SINGLE_ESCAPE = 4;
    static public final int TYPE_MULTIPLE_ESCAPE = 5;
    static public final int TYPE_TERMINATING_MACRO = 6;
    static public final int TYPE_NON_TERMINATING_MACRO = 7;

    static public final int ATTR_ILLEGAL = 1;
    static public final int ATTR_ALPHA = 2;
    static public final int ATTR_DIGIT = 4;
    static public final int ATTR_PACKAGE_MARKER = 8;
    static public final int ATTR_ALPHADIGIT = ATTR_ALPHA | ATTR_DIGIT;

    static final int ASCII_LIMIT = 128;

    private final int[] types = new int[ASCII_LIMIT];
    private final int[] attrs = new int[ASCII_LIMIT];
    private final Function[] macros = new Function[ASCII_LIMIT];
    private final Function[] disp_macros = new Function[ASCII_LIMIT];

//    private int single_escape_char = -1;
//    private int multiple_escape_char = -1;

    public Readtable() {
        init();
    }
    private void init() {
        /*
         * syntax type setting
         */
        setType(0x00, 
                0x07, TYPE_ILLEGAL); // NUL - BEL
        setType(0x08, TYPE_CONSTITUENT); // BS
        setType(0x09, 
                0x0a, TYPE_WHITESPACE); // HT LF
        setType(0x0b, TYPE_ILLEGAL); // VT
        setType(0x0c, 
                0x0d, TYPE_WHITESPACE); // FF CR
        setType(0x0e, 
                0x1f, TYPE_ILLEGAL); // SO - US
        setType(0x20, TYPE_WHITESPACE); // ' '
        setType(0x21, TYPE_CONSTITUENT); // '!'
        setType(0x22, TYPE_TERMINATING_MACRO); // '"'
        setType(0x23, TYPE_NON_TERMINATING_MACRO); // '#'
        setType(0x24, 
                0x26, TYPE_CONSTITUENT); // '$' '%' '&'
        setType(0x27, 
                0x29, TYPE_TERMINATING_MACRO); // '\'' '(' ')'
        setType(0x2a, 
                0x2b, TYPE_CONSTITUENT); // '*' '+'
        setType(0x2c, TYPE_TERMINATING_MACRO); // ','
        setType(0x2d, 
                0x2f, TYPE_CONSTITUENT); // '-' '.' '/'
        setType(0x30, 
                0x39, TYPE_CONSTITUENT); // '0' - '9'
        setType(0x3a, TYPE_CONSTITUENT); // ':'
        setType(0x3b, TYPE_TERMINATING_MACRO); // ';'
        setType(0x3c, 
                0x40, TYPE_CONSTITUENT); // '<' '=' '>' '?' '@'
        setType(0x41, 
                0x5a, TYPE_CONSTITUENT); // 'A' - 'Z'
        setType(0x5b, TYPE_CONSTITUENT); // '['
        setType(0x5c, TYPE_SINGLE_ESCAPE); // '\\'
        setType(0x5d, 
                0x5f, TYPE_CONSTITUENT); // ']' '^' '_'
        setType(0x60, TYPE_TERMINATING_MACRO); // '`'
        setType(0x61, 
                0x7a, TYPE_CONSTITUENT); // 'a' - 'z'
        setType(0x7b, TYPE_CONSTITUENT); // '{'
        setType(0x7c, TYPE_MULTIPLE_ESCAPE); // '|'
        setType(0x7d, TYPE_CONSTITUENT); // '}'
        setType(0x7e, TYPE_CONSTITUENT); // '~'
        setType(0x7f, TYPE_CONSTITUENT); // DEL

        /*
         * attribute setting
         */
        setAttr(0x00, 
                0x1f, ATTR_ILLEGAL); // NUL - US
        setAttr(0x20, ATTR_ILLEGAL); // ' '
        setAttr(0x21,
                0x2f, ATTR_ALPHA); // '!' - '/'
        setAttr(0x30,
                0x39, ATTR_ALPHADIGIT); // '0' - '9'
        setAttr(0x3a, ATTR_PACKAGE_MARKER); // ':'
        //setAttr(0x3a, ATTR_ALPHA); // ':'
        setAttr(0x3b,
                0x40, ATTR_ALPHA); // ';' - '@'
        setAttr(0x41,
                0x5a, ATTR_ALPHADIGIT); // 'A' - 'Z'
        setAttr(0x5b,
                0x60, ATTR_ALPHA); // '[' - '`'
        setAttr(0x61,
                0x7a, ATTR_ALPHADIGIT); // 'A' - 'Z'
        setAttr(0x7b,
                0x7e, ATTR_ALPHA); // '{' - '~'
        setAttr(0x7f, ATTR_ILLEGAL); // DEL

        /*
         * reader macro setting
         */
        setMacro(0x22, Reader.DOUBLEQUOTE_READER); // '"'
        setMacro(0x23, Reader.SHARP_READER); // '#'
        setMacro(0x27, Reader.QUOTE_READER); // '\''
        setMacro(0x28, Reader.OPEN_PAREN_READER); // '('
        setMacro(0x29, Reader.CLOSE_PAREN_READER); // ')'
        setMacro(0x2c, Reader.COMMA_READER); // ','
        setMacro(0x3b, Reader.SEMICOLON_READER); // ';'
        setMacro(0x60, Reader.BACKQUOTE_READER); // '`'

        /*
         * dispatch macro setting
         */
        setDispatchMacro(0x23, Reader.SHARP_SHARP_READER); // '#'
        setDispatchMacro(0x27, Reader.SHARP_QUOTE_READER); // '\''
        setDispatchMacro(0x2e, Reader.SHARP_DOT_READER); // '.'
        setDispatchMacro(0x3a, Reader.SHARP_COLON_READER); // ':'
        setDispatchMacro(0x3d, Reader.SHARP_EQUAL_READER); // '='
        setDispatchMacro(0x42, Reader.SHARP_B_READER); // 'B'
        setDispatchMacro(0x4f, Reader.SHARP_O_READER); // 'O'
        setDispatchMacro(0x58, Reader.SHARP_X_READER); // 'X'
        setDispatchMacro(0x52, Reader.SHARP_NUM_R_READER); // 'R'
        setDispatchMacro(0x5c, Reader.SHARP_BACKSLASH_READER); // '\\'
        setDispatchMacro(0x7c, Reader.SHARP_BAR_READER); // '|'
    }

//    boolean isAscii(int ch) {
//        return Character.UnicodeBlock.of((char) ch)
//            == Character.UnicodeBlock.BASIC_LATIN;
//    }
    boolean isAscii(int ch) {
        if (ch < 0)
            throw new IllegalArgumentException
                ("negative value specified: "+ch);
        return 0 <= ch && ch < ASCII_LIMIT;
    }
//    void assertAscii(int ch) {
//        if (!isAscii(ch))
//            throw new IllegalArgumentException
//                ("character out of range: "+ch);
//    }

    /*
     * array interface
     */

    public Symbol type() {
        return Symbols.READTABLE;
    }
    public Object dims() {
        return Lists.list(Data.toFixnum(0));
    }

    /*
     * readtable operations
     */

    public boolean isIllegal(int ch) {
        return getType(ch) == Readtable.TYPE_ILLEGAL;
    }
    public boolean isWhitespace(int ch) {
        return getType(ch) == Readtable.TYPE_WHITESPACE;
    }
    public boolean isMacro(int ch) {
        return isNonTerminatingMacro(ch)
            || isTerminatingMacro(ch);
    }
    public boolean isNonTerminatingMacro(int ch) {
        return getType(ch) == Readtable.TYPE_NON_TERMINATING_MACRO;
    }
    public boolean isTerminatingMacro(int ch) {
        return getType(ch) == Readtable.TYPE_TERMINATING_MACRO;
    }
    public boolean isSingleEscape(int ch) {
        return getType(ch) == Readtable.TYPE_SINGLE_ESCAPE;
    }
    public boolean isMultipleEscape(int ch) {
        return getType(ch) == Readtable.TYPE_MULTIPLE_ESCAPE;
    }
    public boolean isConstituent(int ch) {
        return getType(ch) == Readtable.TYPE_CONSTITUENT;
    }
//    public boolean isAttrIllegal(int ch, boolean inEscape) {
//        int attr = getAttr(ch, inEscape);
//        if (inEscape) {
//            switch (ch) {
//            case '\t': // 0x09(HT)
//            case '\n': // 0x0a(LF)
//            case '\r': // 0x0d(CR)
//            case ' ': // 0x20
//                return false;
//            default:
//                return attr == Readtable.ATTR_ILLEGAL;
//            }
//        }
//        else {
//            return attr == Readtable.ATTR_ILLEGAL;
//        }
//    }
//    public boolean isAttrIllegal(int ch, boolean inEscape) {
//        return inEscape
//            ? false
//            : getAttr(ch) == Readtable.ATTR_ILLEGAL;
//    }

    public void setType(int ch, int type) {
        types[ch] = type;
//        switch (type) {
//        case TYPE_SINGLE_ESCAPE:
//            single_escape_char = ch;
//            break;
//        case TYPE_MULTIPLE_ESCAPE:
//            multiple_escape_char = ch;
//            break;
//        }
    }
    public void setType(int chFrom, int chTo, int type) {
        for (int ch = chFrom; ch <= chTo; ch++) {
            setType(ch, type);
        }
    }
    public void setAttr(int ch, int attr) {
        attrs[ch] = attr;
    }
    public void setAttr(int chFrom, int chTo, int attr) {
        for (int ch = chFrom; ch <= chTo; ch++) {
            setAttr(ch, attr);
        }
    }
    public void setMacro(int ch, Function fun) {
        macros[ch] = fun;
    }
    public void setDispatchMacro(int ch, Function fun) {
        disp_macros[ch] = fun;
    }

    public int getType(int ch) {
        return isAscii(ch) ? types[ch] : TYPE_CONSTITUENT;
    }
    public int getAttr(int ch) {
        return isAscii(ch) ? attrs[ch] : ATTR_ALPHA;
    }
    public Function getMacro(int ch) {
        return isAscii(ch) ? macros[ch] : null;
    }
    public Function getDispatchMacro(int ch) {
        return isAscii(ch) ? disp_macros[ch] : null;
    }
//    public int getSingleEscapeChar() {
//        return single_escape_char;
//    }
//    public int getMultipleEscapeChar() {
//        return multiple_escape_char;
//    }
}
