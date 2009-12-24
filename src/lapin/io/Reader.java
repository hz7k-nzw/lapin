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
import lapin.eval.Evaluator;
import lapin.eval.Funcall;
import lapin.function.Function;
import lapin.function.Subr;
import lapin.function.SystemSubr;
import lapin.lang.Data;
import lapin.lang.Env;
//import lapin.lang.IllegalDataTypeException;
import lapin.lang.Lists;
import lapin.lang.NotReachedException;
import lapin.lang.Numbers;
import lapin.lang.Package;
import lapin.lang.Pair;
import lapin.lang.Symbol;
import lapin.lang.Symbols;
import lapin.lang.TypeException;
import lapin.lang.Values;
import lapin.util.Logger;
//import lapin.util.form.Backquote;
import java.io.InputStream;
import java.io.IOException;
import java.io.PushbackReader;
import java.util.HashMap;

/**
 * Reader functions.
 */
public final class Reader {
    // uninterned symbols: for private use only
    static private final Symbol BACKQUOTE_LEVEL
        = Symbol.gensym("_BACKQUOTE-LEVEL_");
    static private final Symbol READ_PRESERVE_WHITESPACE_P
        = Symbol.gensym("_READ-PRESERVE-WHITESPACE-P_");
    static private final Symbol READ_RECURSIVE_P
        = Symbol.gensym("_READ-RECURSIVE-P_");
    static private final Symbol UNQUOTE_OCCURRED
        = Symbol.gensym("_UNQUOTE-OCCURRED_");
    static private final Symbol LABEL_OBJ_MAP
        = Symbol.gensym("_LABEL-OBJ-MAP_");
    static private final Symbol OBJ_NOT_READ
        = Symbol.gensym("_OBJ-NOT-READ_");

    static private final int DONE = 0;
    static private final int STEP01 = 1;
    static private final int STEP02 = 2;
    static private final int STEP03 = 3;
    static private final int STEP04 = 4;
    static private final int STEP05 = 5;
    static private final int STEP06 = 6;
    static private final int STEP07 = 7;
    static private final int STEP08 = 8;
    static private final int STEP09 = 9;
    static private final int STEP10 = 10;

    /**
     * Reads and returns the next byte from <code>stream</code>.
     * @param stream NIL or Binary input stream.
     *        NIL means the value of the special variable *SYSTEM-BIN-IN*.
     * @param eof_error_p Flag;
     *        if any object other than NIL is specified
     *        and the end of the stream is reached,
     *        then the method throws {@link UnexpectedEofException}
     * @param eof_value Value returned by the method
     *        if the end of the stream is reached
     *        and <code>eof_error_p</code> is NIL
     * @param env
     * @return the next byte of data
     * @throws StreamException
     * @throws UnexpectedEofException
     */
    static public Object readByte
        (Object stream, Object eof_error_p, Object eof_value, Env env) {
        InputStream in = IO.inputStream(stream, env);
        try {
            int x = in.read();
            if (x == -1) {
                if (Data.isNot(eof_error_p)) {
                    return eof_value;
                }
                else {
                    throw new UnexpectedEofException();
                }
            }
            else {
                return Data.toFixnum(x);
            }
        }
        catch (IOException e) {
            throw new StreamException
                ("I/O error occurred while reading stream: ~S.",
                 Lists.list(stream), e);
        }
    }

    static private PushbackReader pushbackReader(Object stream, Env env) {
        java.io.Reader r = IO.reader(stream, env);
        try {
            return (PushbackReader) r;
        }
        catch (ClassCastException e) {
            //throw new IllegalDataTypeException(r, PushbackReader.class);
            Logger.warn("PushbackReader is required: ~S",
                        Lists.list(r), env);
            return new PushbackReader(r);
        }
    }
    /**
     * Reads and returns the next character from <code>stream</code>.
     * @param stream NIL, T, or Character input stream.
     *        NIL means the value of the special variable *SYSTEM-IN*.
     *        T means the value of the special variable *TERMINAL-IN*.
     * @param eof_error_p Flag;
     *        if any object other than NIL is specified
     *        and the end of the stream is reached,
     *        then the method throws {@link UnexpectedEofException}
     * @param eof_value Value returned by the method
     *        if the end of the stream is reached
     *        and <code>eof_error_p</code> is NIL
     * @param recursive_p Flag; if any object other than NIL is specified,
     *        this call is expected to be embedded in a higher-level call
     *        to {@link #read read} or a similar function used by the Lisp
     *        reader.
     * @param env
     * @return the next character of data
     * @throws StreamException
     * @throws UnexpectedEofException
     */
    static public Object readChar
        (Object stream, Object eof_error_p, Object eof_value, 
         Object recursive_p, Env env) {
        try {
            PushbackReader in = pushbackReader(stream, env);
            Object ret = _readChar(in, env);
            if (Data.isNot(recursive_p)) {
                if (ret == null) {
                    if (Data.isNot(eof_error_p)) {
                        return eof_value;
                    }
                    else {
                        throw new UnexpectedEofException();
                    }
                }
                else {
                    return ret;
                }
            }
            else {
                if (ret == null) {
                    throw new UnexpectedEofException();
                }
                else {
                    return ret;
                }
            }
        }
        catch (IOException e) {
            throw new StreamException
                ("I/O error occurred while reading stream: ~S.",
                 Lists.list(stream), e);
        }
    }
    static private Object _readChar(PushbackReader in, Env env)
        throws IOException {
        int x = in.read();
        if (x == -1)
            return null;
        else
            return Data.toCharacter((char) x);
    }
    /**
     * Places character <code>ch</code> back onto the front of
     * <code>stream</code> so that it will again be the next character
     * in <code>stream</code>.
     * @param ch Character; must be the last character that was read
     *        from <code>stream</code>
     * @param stream NIL, T, or Character input stream.
     *        NIL means the value of the special variable *SYSTEM-IN*.
     *        T means the value of the special variable *TERMINAL-IN*.
     * @param env
     * @return NIL
     * @throws StreamException
     */
    static public Object unreadChar
        (Character ch, Object stream, Env env) {
        try {
            PushbackReader in = pushbackReader(stream, env);
            in.unread(ch.charValue());
            return Symbols.NIL;
        }
        catch (IOException e) {
            throw new StreamException
                ("I/O error occurred while reading stream: ~S.",
                 Lists.list(stream), e);
        }
    }
    /**
     * Obtains the next character in <code>stream</code> without actually
     * reading it, thus leaving the character to be read at a later time. 
     * @param peek_type NIL, T or a character.
     *        If NIL is specified, then the method returns the next character
     *        to be read from <code>stream</code>, without actually removing
     *        it from <code>stream</code>.
     *        If T is specified, the method skips over whitespace characters,
     *        but not comments, and then performs the peeking operation on the
     *        next character.
     *        If a character is specified, then the method skips over input
     *        characters until a character that is equal to that character is
     *        found; that character is left in <code>stream</code>. 
     * @param stream NIL, T, or Character input stream.
     *        NIL means the value of the special variable *SYSTEM-IN*.
     *        T means the value of the special variable *TERMINAL-IN*.
     * @param eof_error_p Flag;
     *        if any object other than NIL is specified
     *        and the end of the stream is reached,
     *        then the method throws {@link UnexpectedEofException}
     * @param eof_value Value returned by the method
     *        if the end of the stream is reached
     *        and <code>eof_error_p</code> is NIL
     * @param recursive_p Flag; if any object other than NIL is specified,
     *        this call is expected to be embedded in a higher-level call
     *        to {@link #read read} or a similar function used by the Lisp
     *        reader.
     * @param env
     * @return the next character of data
     * @throws StreamException
     * @throws UnexpectedEofException
     */
    static public Object peekChar
        (Object peek_type, Object stream, Object eof_error_p,
         Object eof_value, Object recursive_p, Env env) {
        try {
            PushbackReader in = pushbackReader(stream, env);
            Object ret = _peekChar(in, peek_type, env);
            if (Data.isNot(recursive_p)) {
                if (ret == null) {
                    if (Data.isNot(eof_error_p)) {
                        return eof_value;
                    }
                    else {
                        throw new UnexpectedEofException();
                    }
                }
                else {
                    return ret;
                }
            }
            else {
                if (ret == null) {
                    throw new UnexpectedEofException();
                }
                else {
                    return ret;
                }
            }
        }
        catch (IOException e) {
            throw new StreamException
                ("I/O error occurred while reading stream: ~S.",
                 Lists.list(stream), e);
        }
    }
    static private Object _peekChar
        (PushbackReader in, Object peek_type, Env env)
        throws IOException {
        Readtable rt = IO.readtable(env);
        if (peek_type == Symbols.NIL) {
            int x = in.read();
            if (x == -1) {
                return null;
            }
            else {
                in.unread(x);
                return Data.toCharacter((char) x);
            }
        }
        else if (peek_type == Symbols.T) {
            int x;
            while (true) {
                x = in.read();
                if (x == -1) {
                    return null;
                }
                else if (rt.isWhitespace(x)) {
                    continue;
                }
                else {
                    in.unread(x);
                    return Data.toCharacter((char) x);
                }
            }
        }
        else if (Data.isCharacter(peek_type)) {
            int x;
            int y = (int) Data.character(peek_type).charValue();
            while (true) {
                x = in.read();
                if (x == -1) {
                    return null;
                }
                else if (x != y) {
                    continue;
                }
                else {
                    in.unread(x);
                    return Data.toCharacter((char) x);
                }
            }
        }
        else {
            throw new TypeException
                ("illegal peek-type: ~S", Lists.list(peek_type));
        }
    }
    /**
     * Returns T if there is a character immediately available from
     * <code>stream</code>; otherwise, returns NIL.
     * @param stream NIL, T, or Character input stream.
     *        NIL means the value of the special variable *SYSTEM-IN*.
     *        T means the value of the special variable *TERMINAL-IN*.
     * @param env
     * @return Flag which indicates whether this stream is ready to be read
     * @throws StreamException
     */
    static public Object listen(Object stream, Env env) {
        try {
            PushbackReader in = pushbackReader(stream, env);
            return Data.toPredicate(in.ready());
        }
        catch (IOException e) {
            throw new StreamException
                ("I/O error occurred while reading stream: ~S.",
                 Lists.list(stream), e);
        }
    }
    /**
     * Reads and returns a character from <code>stream</code> if such a
     * character is available. If no character is available, the method
     * returns NIL. 
     * @param stream NIL, T, or Character input stream.
     *        NIL means the value of the special variable *SYSTEM-IN*.
     *        T means the value of the special variable *TERMINAL-IN*.
     * @param eof_error_p Flag;
     *        if any object other than NIL is specified
     *        and the end of the stream is reached,
     *        then the method throws {@link UnexpectedEofException}
     * @param eof_value Value returned by the method
     *        if the end of the stream is reached
     *        and <code>eof_error_p</code> is NIL
     * @param recursive_p Flag; if any object other than NIL is specified,
     *        this call is expected to be embedded in a higher-level call
     *        to {@link #read read} or a similar function used by the Lisp
     *        reader.
     * @param env
     * @return the next character of data or NIL
     * @throws StreamException
     * @throws UnexpectedEofException
     */
    static public Object readCharNoHang
        (Object stream, Object eof_error_p, Object eof_value, 
         Object recursive_p, Env env) {
        try {
            PushbackReader in = pushbackReader(stream, env);
            Object ret = _readCharNoHang(in, env);
            if (Data.isNot(recursive_p)) {
                if (ret == null) {
                    if (Data.isNot(eof_error_p)) {
                        return eof_value;
                    }
                    else {
                        throw new UnexpectedEofException();
                    }
                }
                else {
                    return ret;
                }
            }
            else {
                if (ret == null) {
                    throw new UnexpectedEofException();
                }
                else {
                    return ret;
                }
            }
        }
        catch (IOException e) {
            throw new StreamException
                ("I/O error occurred while reading stream: ~S.",
                 Lists.list(stream), e);
        }
    }
    static private Object _readCharNoHang(PushbackReader in, Env env)
        throws IOException {
        if (!in.ready())
            return Symbols.NIL;
        int x = in.read();
        if (x == -1)
            return null;
        else
            return Data.toCharacter((char) x);
    }
    /**
     * Obtains the next character in <code>stream</code> without actually
     * reading it, thus leaving the character to be read at a later time. 
     * If no character is available, the method returns NIL. 
     * @param peek_type NIL, T or a character.
     *        If NIL is specified, then the method returns the next character
     *        to be read from <code>stream</code>, without actually removing
     *        it from <code>stream</code>.
     *        If T is specified, the method skips over whitespace characters,
     *        but not comments, and then performs the peeking operation on the
     *        next character.
     *        If a character is specified, then the method skips over input
     *        characters until a character that is equal to that character is
     *        found; that character is left in <code>stream</code>. 
     * @param stream NIL, T, or Character input stream.
     *        NIL means the value of the special variable *SYSTEM-IN*.
     *        T means the value of the special variable *TERMINAL-IN*.
     * @param eof_error_p Flag;
     *        if any object other than NIL is specified
     *        and the end of the stream is reached,
     *        then the method throws {@link UnexpectedEofException}
     * @param eof_value Value returned by the method
     *        if the end of the stream is reached
     *        and <code>eof_error_p</code> is NIL
     * @param recursive_p Flag; if any object other than NIL is specified,
     *        this call is expected to be embedded in a higher-level call
     *        to {@link #read read} or a similar function used by the Lisp
     *        reader.
     * @param env
     * @return the next character of data or NIL
     * @throws StreamException
     * @throws UnexpectedEofException
     */
    static public Object peekCharNoHang
        (Object peek_type, Object stream, Object eof_error_p,
         Object eof_value, Object recursive_p, Env env) {
        try {
            PushbackReader in = pushbackReader(stream, env);
            Object ret = _peekCharNoHang(in, peek_type, env);
            if (Data.isNot(recursive_p)) {
                if (ret == null) {
                    if (Data.isNot(eof_error_p)) {
                        return eof_value;
                    }
                    else {
                        throw new UnexpectedEofException();
                    }
                }
                else {
                    return ret;
                }
            }
            else {
                if (ret == null) {
                    throw new UnexpectedEofException();
                }
                else {
                    return ret;
                }
            }
        }
        catch (IOException e) {
            throw new StreamException
                ("I/O error occurred while reading stream: ~S.",
                 Lists.list(stream), e);
        }
    }
    static private Object _peekCharNoHang
        (PushbackReader in, Object peek_type, Env env)
        throws IOException {
        Readtable rt = IO.readtable(env);
        if (peek_type == Symbols.NIL) {
            if (!in.ready()) {
                return Symbols.NIL;
            }
            int x = in.read();
            if (x == -1) {
                return null;
            }
            else {
                in.unread(x);
                return Data.toCharacter((char) x);
            }
        }
        else if (peek_type == Symbols.T) {
            int x;
            while (true) {
                if (!in.ready()) {
                    return Symbols.NIL;
                }
                x = in.read();
                if (x == -1) {
                    return null;
                }
                else if (rt.isWhitespace(x)) {
                    continue;
                }
                else {
                    in.unread(x);
                    return Data.toCharacter((char) x);
                }
            }
        }
        else if (Data.isCharacter(peek_type)) {
            int x;
            int y = (int) Data.character(peek_type).charValue();
            while (true) {
                if (!in.ready()) {
                    return Symbols.NIL;
                }
                x = in.read();
                if (x == -1) {
                    return null;
                }
                else if (x != y) {
                    continue;
                }
                else {
                    in.unread(x);
                    return Data.toCharacter((char) x);
                }
            }
        }
        else {
            throw new TypeException
                ("illegal peek-type: ~S", Lists.list(peek_type));
        }
    }
    /**
     * Reads from <code>stream</code> a line of text that is terminated
     * by a newline or end of file. 
     * @param stream NIL, T, or Character input stream.
     *        NIL means the value of the special variable *SYSTEM-IN*.
     *        T means the value of the special variable *TERMINAL-IN*.
     * @param eof_error_p Flag;
     *        if any object other than NIL is specified
     *        and the end of the stream is reached,
     *        then the method throws {@link UnexpectedEofException}
     * @param eof_value Value returned by the method
     *        if the end of the stream is reached
     *        and <code>eof_error_p</code> is NIL
     * @param recursive_p Flag; if any object other than NIL is specified,
     *        this call is expected to be embedded in a higher-level call
     *        to {@link #read read} or a similar function used by the Lisp
     *        reader.
     * @param env
     * @return String that denotes a line of text
     * @throws StreamException
     * @throws UnexpectedEofException
     */
    static public Object readLine
        (Object stream, Object eof_error_p, Object eof_value, 
         Object recursive_p, Env env) {
        try {
            PushbackReader in = pushbackReader(stream, env);
            Object ret = _readLine(in, env);
            if (Data.isNot(recursive_p)) {
                if (ret == null) {
                    if (Data.isNot(eof_error_p)) {
                        return eof_value;
                    }
                    else {
                        throw new UnexpectedEofException();
                    }
                }
                else {
                    return ret;
                }
            }
            else {
                if (ret == null) {
                    throw new UnexpectedEofException();
                }
                else {
                    return ret;
                }
            }
        }
        catch (IOException e) {
            throw new StreamException
                ("I/O error occurred while reading stream: ~S.",
                 Lists.list(stream), e);
        }
    }
    static private Object _readLine(PushbackReader in, Env env)
        throws IOException {
        boolean inCR = false;
        boolean loop = true;
        StringBuffer buf = null;
        String ret = null;
        while (loop) {
            int x = in.read();
            //Logger.info("readLine: ~S~%",
            //            Lists.list(Data.toCharacter((char) x)), env);
            switch (x) {
            case -1:
                if (inCR) {
                    // ... CR EOF => unread EOF and return
                    in.unread(x);
                    if (buf == null)
                        ret = "";
                    else
                        ret = buf.toString();
                    loop = false;
                }
                else {
                    // ... XX EOF => return
                    if (buf == null)
                        ret = null;
                    else
                        ret = buf.toString();
                    loop = false;
                }
                break;
            case '\r':
                if (inCR) {
                    // ... CR CR => unread CR and return
                    in.unread(x);
                    if (buf == null)
                        ret = "";
                    else
                        ret = buf.toString();
                    loop = false;
                }
                else {
                    // ... XX CR => set inCR = true and continue
                    inCR = true;
                }
                break;
            case '\n':
                // ... XX LF => return
                if (buf == null)
                    ret = "";
                else
                    ret = buf.toString();
                loop = false;
                break;
            default:
                if (inCR) {
                    // ... CR XX => unread XX and return
                    in.unread(x);
                    if (buf == null)
                        ret = "";
                    else
                        ret = buf.toString();
                    loop = false;
                }
                else {
                    // ... XX YY => append YY and continue
                    if (buf == null)
                        buf = new StringBuffer();
                    buf.append((char) x);
                }
                break;
            }
        }
        return ret;
    }
    /**
     * Parses the printed representation of an object from
     * <code>stream</code> and builds such an object.
     * @param stream NIL, T, or Character input stream.
     *        NIL means the value of the special variable *SYSTEM-IN*.
     *        T means the value of the special variable *TERMINAL-IN*.
     * @param eof_error_p Flag;
     *        if any object other than NIL is specified
     *        and the end of the stream is reached,
     *        then the method throws {@link UnexpectedEofException}
     * @param eof_value Value returned by the method
     *        if the end of the stream is reached
     *        and <code>eof_error_p</code> is NIL
     * @param recursive_p Flag; if any object other than NIL is specified,
     *        this call is expected to be embedded in a higher-level call
     *        to {@link #read read} or a similar function used by the Lisp
     *        reader.
     * @param env
     * @return Lisp object
     * @throws StreamException
     * @throws UnexpectedEofException
     */
    static public Object read
        (Object stream, Object eof_error_p, Object eof_value, 
         Object recursive_p, Env env) {
        return _read(stream, eof_error_p, eof_value, 
                     recursive_p, Symbols.NIL, env);
    }
    /**
     * This method is like {@link #read read} but preserves any whitespace
     * character that delimits the printed representation of the object.
     * @param stream NIL, T, or Character input stream.
     *        NIL means the value of the special variable *SYSTEM-IN*.
     *        T means the value of the special variable *TERMINAL-IN*.
     * @param eof_error_p Flag;
     *        if any object other than NIL is specified
     *        and the end of the stream is reached,
     *        then the method throws {@link UnexpectedEofException}
     * @param eof_value Value returned by the method
     *        if the end of the stream is reached
     *        and <code>eof_error_p</code> is NIL
     * @param recursive_p Flag; if any object other than NIL is specified,
     *        this call is expected to be embedded in a higher-level call
     *        to {@link #read read} or a similar function used by the Lisp
     *        reader.
     * @param env
     * @return Lisp object
     * @throws StreamException
     * @throws UnexpectedEofException
     */
    static public Object readPreservingWhitespace
        (Object stream, Object eof_error_p, Object eof_value, 
         Object recursive_p, Env env) {
        return _read(stream, eof_error_p, eof_value, 
                     recursive_p, Symbols.T, env);
    }
    /**
     * Parses the printed representation of an object from the specified
     * string, as if {@link #read read} had been called on an input
     * stream containing those same characters. 
     * Note that this method does not support the second return value
     * "position" defined in common lisp.
     * @param str String providing the character stream
     * @param eof_error_p Flag;
     *        if any object other than NIL is specified
     *        and the end of the stream is reached,
     *        then the method throws {@link UnexpectedEofException}
     * @param eof_value Value returned by the method
     *        if the end of the stream is reached
     *        and <code>eof_error_p</code> is NIL
     * @param whitespace_p Flag;
     *        if any object other than NIL is specified,
     *        the operation will preserve whitespace as
     *        {@link #readPreservingWhitespace readPreservingWhitespace}
     *         would do. 
     * @param env
     * @return Lisp object
     * @throws StreamException
     * @throws UnexpectedEofException
     */
    static public Object readFromString
        (String str, Object eof_error_p, Object eof_value,
         Object whitespace_p, Env env) {
        java.io.Reader r = IO.makeStringReader(str);
        return _read(r, eof_error_p, eof_value, Symbols.NIL,
                     whitespace_p, env);
    }
    static private Object _read
        (Object stream, Object eof_error_p, Object eof_value, 
         Object recursive_p, Object whitespace_p, Env env) {
        try {
            PushbackReader in = pushbackReader(stream, env);
            if (Data.isNot(recursive_p)) {
                // non recursive call
                Object val = readTop(in, whitespace_p, env);
                if (val == IO.DOT) {
                    // dot_handling
                    throw new UnexpectedDotException();
                }
                else if (val == IO.EOF) {
                    // eof_handling
                    if (Data.isNot(eof_error_p)) {
                        return eof_value;
                    }
                    else {
                        throw new UnexpectedEofException();
                    }
                }
                else {
                    return val;
                }
            } else {
                // recursive call
                return readRecursiveNoDot(in, env);
            }
        }
        catch (IOException e) {
            throw new StreamException
                ("I/O error occurred while reading stream: ~S.",
                 Lists.list(stream), e);
        }
    }
    static private Object readTop
        (PushbackReader in, Object whitespace_p, Env env)
        throws IOException {
        Env newenv = env.child();
        newenv.bind(READ_RECURSIVE_P, Symbols.NIL);
        newenv.bind(READ_PRESERVE_WHITESPACE_P, whitespace_p);
        newenv.bind(BACKQUOTE_LEVEL, Symbols.NIL);
        newenv.bind(LABEL_OBJ_MAP, Symbols.NIL);
        return readInternal(in, newenv);
    }
    static private Object readRecursive
        (PushbackReader in, Env env)
        throws IOException {
        Env newenv;
        if (Data.isNot(env.get(READ_RECURSIVE_P))) {
            newenv = env.child();
            newenv.bind(READ_RECURSIVE_P, Symbols.T);
            newenv.bind(READ_PRESERVE_WHITESPACE_P, Symbols.NIL);
        }
        else {
            newenv = env;
        }
        return readInternal(in, newenv);
    }
    static private Object readRecursiveNoDot
        (PushbackReader in, Env env) throws IOException {
        Object ret = readRecursive(in, env);
        if (ret == IO.DOT) {
            throw new UnexpectedDotException();
        }
        return ret;
    }
    static private Object readDelimitedList
        (PushbackReader in, int endch, Symbol ifdotted, Env env)
        throws IOException {
        Env newenv;
        if (Data.isNot(env.get(READ_RECURSIVE_P))) {
            newenv = env.child();
            newenv.bind(READ_RECURSIVE_P, Symbols.T);
        }
        else {
            newenv = env;
        }
        return readDelimitedListRecursive(in, endch, ifdotted, newenv);
    }
    static private Object readDelimitedListRecursive
        (PushbackReader in, int endch, Symbol ifdotted, Env env)
        throws IOException {
        Readtable rt = IO.readtable(env);
        int x, step;
        boolean process;
        Object obj, ret;
        Pair head, tail;
        // READ_CAR loop
        x = -1;
        step = STEP01;
        obj = null;
        ret = null;
        head = null;
        tail = null;
        READ_CAR:
        while (true) {
            switch (step) {
            case STEP01: // read obj
                // peek one char
                x = peekCharWithEofError(in, env);
                if (x == endch) {
                    // goto DONE
                    step = DONE;
                }
                else if (rt.isMacro(x)) {
                    in.read();
                    ret = readMacro(in, x, env);
                    if (Values.hasValue(ret)) {
                        obj = Values.singleValue(ret);
                        // goto STEP02
                        step = STEP02;
                    }
                    else {
                        // macro returns nothing
                        // goto STEP01
                        step = STEP01;
                    }
                }
                else {
                    obj = readRecursiveNoDot(in, env);
                    // goto STEP02
                    step = STEP02;
                }
                break;
            case STEP02: // append obj to list
                // break READ_CAR loop -> goto READ_CDR loop
                head = Lists.cons(obj, Symbols.NIL);
                tail = head;
                break READ_CAR;
            case DONE: // return empty list
                in.read();
                return Symbols.NIL;
            default:
                throw new NotReachedException
                    ("illegal step detected: "+step, Symbols.NIL);
            }
        }
        // READ_CDR loop
        x = -1;
        step = STEP01;
        obj = null;
        ret = null;
        READ_CDR:
        while (true) {
            switch (step) {
            case STEP01: // read obj
                // peek one char
                x = peekCharWithEofError(in, env);
                if (x == endch) {
                    // goto DONE (finish-list)
                    step = DONE;
                }
                else if (rt.isMacro(x)) {
                    in.read();
                    ret = readMacro(in, x, env);
                    if (Values.hasValue(ret)) {
                        obj = Values.singleValue(ret);
                        // goto STEP02
                        step = STEP02;
                    }
                    else {
                        // macro returns nothing
                        // goto STEP01
                        step = STEP01;
                    }
                }
                else {
                    obj = readRecursive(in, env);
                    if (obj == IO.DOT) {
                        // goto STEP03 (dot)
                        step = STEP03;
                    }
                    else {
                        // goto STEP02
                        step = STEP02;
                    }
                }
                break;
            case STEP02: // append obj to list
                Lists.rplacd(tail, Lists.cons(obj, Symbols.NIL));
                tail = Data.pair(Lists.cdr(tail));
                // goto STEP01
                step = STEP01;
                break;
            case STEP03: // dot
                // accepts dotted list
                // only when ifdotted is equal to IO.DOT
                if (IO.DOT != ifdotted) {
                    throw new UnexpectedDotException();
                }
                // fallthrough
            case STEP04: // read cdr for dotted list
                // peek one char
                x = peekCharWithEofError(in, env);
                if (x == endch) {
                    // goto STEP05 (error-dot)
                    step = STEP05;
                }
                else if (rt.isMacro(x)) {
                    in.read();
                    ret = readMacro(in, x, env);
                    if (Values.hasValue(ret)) {
                        obj = Values.singleValue(ret);
                        step = STEP06;
                    }
                    else {
                        // macro returns nothing
                        // goto STEP04
                        step = STEP04;
                    }
                }
                else {
                    obj = readRecursiveNoDot(in, env);
                    step = STEP06;
                }
                break;
            case STEP05: // error-dot
                throw new ReadException
                    ("illegal end of dotted list", Symbols.NIL);
            case STEP06: // append obj to cdr of list
                Lists.rplacd(tail, obj);
                // fallthrough
            case STEP07: // read comment after last element
                // peek one char
                x = peekCharWithEofError(in, env);
                if (x == endch) {
                    // goto DONE (finish-list)
                    step = DONE;
                }
                else if (rt.isMacro(x)) {
                    in.read();
                    ret = readMacro(in, x, env);
                    if (Values.hasValue(ret)) {
                        // goto STEP05 (error-dot)
                        step = STEP05;
                    }
                    else {
                        // macro returns nothing
                        // goto STEP07
                        step = STEP07;
                    }
                }
                else {
                    // goto STEP05 (error-dot)
                    step = STEP05;
                }
                break;
            case DONE: // finish-list
                in.read();
                return head;
            default:
                throw new NotReachedException
                    ("illegal step detected: "+step, Symbols.NIL);
            }
        }
    }
    static private int peekCharWithEofError
        (PushbackReader in, Env env) throws IOException {
        Readtable rt = IO.readtable(env);
        int x;
        while (true) {
            x = in.read();
            if (x == -1) {
                throw new UnexpectedEofException();
            }
            else if (rt.isWhitespace(x)) {
                continue;
            }
            else {
                in.unread(x);
                return x;
            }
        }
    }
    static private Object readInternal
        (PushbackReader in, Env env) throws IOException {
        Readtable rt = IO.readtable(env);
        int step = STEP01;
        int x = -1;
        int y = -1;
        //int z = -1;
        Token token = null;
        Object ret = OBJ_NOT_READ;
        while (true) {
            switch (step) {
            case STEP01:
                x = in.read();
                if (x == -1) {
                    if (!Data.isNot(env.get(READ_RECURSIVE_P))) {
                        throw new UnexpectedEofException();
                    }
                    ret = IO.EOF;
                    // goto DONE
                    step = DONE;
                    break;
                }
                else {
                    //System.out.println("[debug] STEP01: '"+((char) x)+"'");
                    // fallthrough
                }
            case STEP02:
                if (rt.isIllegal(x)) {
                    throw new UnexpectedCharTypeException(x);
                }
                else {
                    // fallthrough
                }
            case STEP03:
                if (rt.isWhitespace(x)) {
                    // goto STEP01
                    step = STEP01;
                    break;
                }
                else {
                    // fallthrough
                }
            case STEP04:
                if (rt.isMacro(x)) {
                    // invoke function for the macro character x:
                    ret = readMacro(in, x, env);
                    if (Values.hasValue(ret)) {
                        // goto DONE
                        step = DONE;
                    }
                    else {
                        // goto STEP1
                        step = STEP01;
                    }
                    break;
                }
                else {
                    // fallthrough
                }
            case STEP05:
                if (rt.isSingleEscape(x)) {
                    y = in.read();
                    if (y == -1) {
                        throw new UnexpectedEofException();
                    }
                    else {
                        // use y to begin token
                        token = new Token();
                        token.add((char) y, Readtable.ATTR_ALPHA);
                        // escape occurred!
                        token.escapeOccurred = true;
                        // read token
                        readToken(in, token, in.read(), STEP08, env);
                        // goto STEP10
                        step = STEP10;
                        break;
                    }
                }
                else {
                    // fallthrough
                }
            case STEP06:
                if (rt.isMultipleEscape(x)) {
                    // begin token
                    token = new Token();
                    // escape occurred!
                    token.escapeOccurred = true;
                    // read token
                    readToken(in, token, in.read(), STEP09, env);
                    // goto STEP10
                    step = STEP10;
                    break;
                }
                else {
                    // fallthrough
                }
            case STEP07:
                if (rt.isConstituent(x)) {
                    // replace x with the corresponding upcase character
                    x = IO.toUpcase(x);
                    // use x to begin token
                    token = new Token();
                    token.add((char) x, rt.getAttr(x));
                    // read token
                    readToken(in, token, in.read(), STEP08, env);
                    // goto STEP10
                    step = STEP10;
                }
                else {
                    throw new UnexpectedCharTypeException(x);
                }
                break;
            /*
             * STEP08 and STEP09 is moved to Reader#readToken method.
             */
            case STEP10:
                // build lisp object
                ret = parseObject(token, env);
                // fallthrough
            case DONE:
                if (ret == OBJ_NOT_READ) {
                    throw new ReadException
                        ("object not read", Symbols.NIL);
                }
                return ret;
            default:
                throw new NotReachedException
                    ("illegal step detected: "+step, Symbols.NIL);
            }
        }
    }
    /**
     * constructs a extended-token.
     * @param in input stream
     * @param token token to be build
     * @param y initial char
     * @param step initial step, either STEP08 or STEP09
     * @param env environment
     */
    static private void readToken
        (PushbackReader in, Token token, int y, int step, Env env)
        throws IOException {
        Readtable rt = IO.readtable(env);
        int z = -1;
        while (true) {
            switch (step) {
            case STEP08: // enter extended token
                if (y == -1) {
                    // eof
                    return;
                }
                else if (rt.isConstituent(y) || rt.isNonTerminatingMacro(y)) {
                    // replace y with the corresponding upcase character
                    y = IO.toUpcase(y);
                    // append y to token being built
                    token.add((char) y, rt.getAttr(y));
                    // goto STEP08
                    step = STEP08;
                    break;
                }
                else if (rt.isSingleEscape(y)) {
                    z = in.read();
                    if (z == -1) {
                        throw new UnexpectedEofException();
                    }
                    else {
                        // escape occurred!
                        token.escapeOccurred = true;
                        // append z to token being built
                        token.add((char) z, Readtable.ATTR_ALPHA);
                        // goto STEP08
                        step = STEP08;
                        break;
                    }
                }
                else if (rt.isMultipleEscape(y)) {
                    // escape occurred!
                    token.escapeOccurred = true;
                    // goto STEP09
                    step = STEP09;
                    break;
                }
                else if (rt.isIllegal(y)) {
                    throw new UnexpectedCharTypeException(y);
                }
                else if (rt.isTerminatingMacro(y)) {
                    // unread y
                    in.unread(y);
                    // terminate token
                    return;
                }
                else if (rt.isWhitespace(y)) {
                    // terminate token
                    // unread y if appropriate
                    if (!Data.isNot(env.get(READ_PRESERVE_WHITESPACE_P))) {
                        in.unread(y);
                    }
                    return;
                }
                else {
                    throw new UnexpectedCharTypeException(y);
                }
            case STEP09: // enter multiple escape
                if (y == -1) {
                    throw new UnexpectedEofException();
                }
                else if (rt.isConstituent(y) || 
                         rt.isMacro(y) || 
                         rt.isWhitespace(y)) {
                    // append y to token being built
                    token.add((char) y, Readtable.ATTR_ALPHA);
                    // goto STEP09
                    step = STEP09;
                    break;
                }
                else if (rt.isSingleEscape(y)) {
                    z = in.read();
                    if (z == -1) {
                        throw new UnexpectedEofException();
                    }
                    else {
                        // escape occurred!
                        //token.escapeOccurred = true;
                        // append z to token being built
                        token.add((char) z, Readtable.ATTR_ALPHA);
                        // goto STEP09
                        step = STEP09;
                        break;
                    }
                }
                else if (rt.isMultipleEscape(y)) {
                    // goto STEP08 -> exit multiple escape
                    step = STEP08;
                    break;
                }
                else if (rt.isIllegal(y)) {
                    throw new UnexpectedCharTypeException(y);
                }
                else {
                    throw new UnexpectedCharTypeException(y);
                }
            default:
                throw new NotReachedException
                    ("illegal step detected: "+step, Symbols.NIL);
            }
            // read next char
            y = in.read();
        }
    }
    static private Object parseObject(Token token, Env env) {
        //if (Logger.debuglevelp(env))
        //    Logger.debug("token="+token, env);
        // check attr
        token.checkAttr();
        if (token.posIllegal > 0) {
            int c = token.cbuf.charAt(token.posIllegal);
            throw new UnexpectedCharAttributeException(c);
        }
        if (token.posUnexpectedPackageMarker > 0) {
            throw new ReadException
                ("colon appears at unexpected position: ~S",
                 Lists.list(token.cbuf.toString()));
        }
        if (token.escapeOccurred) {
            // token is symbol
            return parseSymbol(token, env);
        }
        else {
            String s = token.cbuf.toString();
            Object o;
            // dot check
            o = IO.parseDot(s);
            if (o == IO.DOT) {
                return o;
            }
            else if (o == IO.MULTIPLE_DOT) {
                throw new ReadException
                    ("multiple dots not allowed.", Symbols.NIL);
            }
            // number check
            o = IO.parseNumber(s);
            if (o instanceof Number) {
                return o;
            }
            // token must be symbol
            return parseSymbol(token, env);
        }
    }
    static private Symbol parseSymbol(Token token, Env env) {
        String s = token.cbuf.toString();
        // colon check
        if (token.posPackageMarker < 0) {
            Package currpkg = Package.get(env);
            Values mv = env.lisp().getObarray().intern(currpkg, s);
            Object v0 = mv.nth(0);
            return Data.symbol(v0);
        }
        //else if (token.posPackageMarker == 0) {
        //    Package pkg = Package.KEYWORD;
        //    String pname = s.substring(1);
        //    Values mv = env.lisp().getObarray().intern(pkg, pname);
        //    Object v0 = mv.nth(0);
        //    Object v1 = mv.nth(1);
        //    Symbol sym = Data.symbol(v0);
        //    if (sym.pkg() == pkg && v1 != Symbols.KW_EXTERNAL) {
        //        env.lisp().getObarray().exp(pkg, sym);
        //    }
        //    return sym;
        //}
        else if (token.posPackageMarker == 0) {
            Package pkg = Package.KEYWORD;
            String pname = s.substring(1);
            Values mv = env.lisp().getObarray().intern(pkg, pname);
            Object v0 = mv.nth(0);
            return Data.symbol(v0);
        }
        else { /* token.posPackageMarker > 0 */
            String pkgname = s.substring(0, token.posPackageMarker);
            String pname = token.symInternal
                ? s.substring(token.posPackageMarker+2)
                : s.substring(token.posPackageMarker+1);
            Values mv = token.symInternal
                ? env.lisp().getObarray().intern(pkgname, pname)
                : env.lisp().getObarray().find(pkgname, pname, true);
            Object v0 = mv.nth(0);
            Object v1 = mv.nth(1);
            Symbol sym = Data.symbol(v0);
            if (sym == Symbols.NIL && v1 == Symbols.NIL)
                throw new ReadException
                    ("package ~S has no external symbol with name ~S.",
                     Lists.list(pkgname, pname));
            return sym;
        }
    }
    static private Object readMacro
        (PushbackReader in, int x, Env env) {
        Readtable rt = IO.readtable(env);
        Function fun = rt.getMacro(x);
        Character ch = Data.toCharacter((char) x);
        if (fun == null) {
            throw new ReadException
                ("unsupported macro char: ~S.", Lists.list(ch));
        }
        return Funcall.funcall2(fun, in, ch, env);
    }
    static private Object readDoublequote
        (PushbackReader in, Character ch, Env env)
        throws IOException {
        Readtable rt = IO.readtable(env);
        StringBuffer buf = new StringBuffer();
        int delim = ch.charValue();
        int x;
        while (true) {
            x = in.read();
            if (x == -1) {
                throw new UnexpectedEofException();
            }
            else if (x == delim) {
                return buf.toString();
            }
            else if (rt.isSingleEscape(x)) {
                x = in.read();
                if (x == -1) {
                    throw new UnexpectedEofException();
                }
                switch (x) {
                case '\n':
                    continue;
                case 'n':
                    buf.append('\n'); break;
                case 't':
                    buf.append('\t'); break;
                case 'b':
                    buf.append('\b'); break;
                case 'r':
                    buf.append('\r'); break;
                case 'f':
                    buf.append('\f'); break;
                case '\\':
                    buf.append('\\'); break;
                case '\'':
                    buf.append('\''); break;
                case '"':
                    buf.append('"'); break;
                default:
                    buf.append((char) x); break;
                }
            }
            else {
                buf.append((char) x);
            }
        }
    }
    static private Object readSharp
        (PushbackReader in, Character ch, Env env)
        throws IOException {
        Readtable rt = IO.readtable(env);
        int x = in.read();
        int value;
        boolean present;
        if (IO.isDigit(x)) {
            value = IO.toDigit(x);
            present = true;
            while (true) {
                x = in.read();
                if (!IO.isDigit(x))
                    break;
                value = value * 10 + IO.toDigit(x);
            }
        }
        else {
            value = 0;
            present = false;
        }
        if (x == -1) {
            throw new UnexpectedEofException();
        }
        x = IO.toUpcase(x);
        Function fun = rt.getDispatchMacro(x);
        Character c = Data.toCharacter((char) x);
        if (fun == null) {
            throw new ReadException
                ("unsupported dispatch macro char: ~S.", Lists.list(c));
        }
        Object num = present
            ? (Object) Data.toFixnum(value)
            : (Object) Symbols.NIL;
        return Funcall.funcall3(fun, in, c, num, env);
    }
    static private Object readQuote
        (PushbackReader in, Character ch, Env env)
        throws IOException {
        Object obj = read(in,Symbols.T,Symbols.NIL,Symbols.T,env);
        return Lists.list(Symbols.QUOTE, obj);
    }
    static private Object readOpenParen
        (PushbackReader in, Character ch, Env env)
        throws IOException {
        // third arg of readDelimitedList is IO.DOT
        // since readOpenParen must accept dotted list
        return readDelimitedList(in, ')', IO.DOT, env);
    }
    static private Object readCloseParen
        (PushbackReader in, Character ch, Env env)
        /*throws IOException*/ {
        throw new ReadException
            ("unexpected close-paren", Symbols.NIL);
    }
    static private Object readComma
        (PushbackReader in, Character ch, Env env)
        throws IOException {
        if (env.get(BACKQUOTE_LEVEL) == Symbols.NIL) {
            throw new ReadException
                ("comma is illegal outside of backquote",
                 Symbols.NIL);
        }
        Number bl = Data.fixnum(env.get(BACKQUOTE_LEVEL));
        if (Numbers.isZero(bl)) {
            throw new ReadException
                ("more commas out than backquotes in, is illegal",
                 Symbols.NIL);
        }
        env.set(UNQUOTE_OCCURRED, Symbols.T);
        Env newenv = env.child();
        newenv.bind(BACKQUOTE_LEVEL, Data.toFixnum(bl.intValue()-1));
        int next = peekCharWithEofError(in, env);
        Object obj;
        if (next == '@') {
            in.read();
            obj = read(in,Symbols.T,Symbols.NIL,Symbols.T,newenv);
            return Lists.list(Symbols.SPLICE, obj);
        }
        else if (next == '.') {
            in.read();
            obj = read(in,Symbols.T,Symbols.NIL,Symbols.T,newenv);
            return Lists.list(Symbols.NSPLICE, obj);
        }
        else {
            obj = read(in,Symbols.T,Symbols.NIL,Symbols.T,newenv);
            return Lists.list(Symbols.UNQUOTE, obj);
        }
    }
    static private Object readSemicolon
        (PushbackReader in, Character ch, Env env)
        throws IOException {
        while (true) {
            int x = in.read();
            if (x == '\n' || x == -1) break;
        }
        return Values.currentValues();
    }
    static private Object readBackquote
        (PushbackReader in, Character ch, Env env)
        throws IOException {
        Number bl;
        if (env.get(BACKQUOTE_LEVEL) == Symbols.NIL)
            bl = Data.toFixnum(0);
        else
            bl = Data.fixnum(env.get(BACKQUOTE_LEVEL));
        Env newenv = env.child();
        newenv.bind(UNQUOTE_OCCURRED, Symbols.NIL);
        newenv.bind(BACKQUOTE_LEVEL, 
                    Data.toFixnum(bl.intValue()+1));
        Object obj = read(in,         // stream
                          Symbols.T,  // eof-error-p
                          Symbols.NIL,// eof-value
                          Symbols.T,  // recursive-p
                          newenv);    // environment
        if (!Data.isList(obj)) {
            if (!Data.isNot(newenv.get(UNQUOTE_OCCURRED)))
                throw new ReadException
                    ("unquote may occur only in (...): ~S.",
                     Lists.list(obj));
        }
        if (Data.isPair(obj)) {
            Object head = Lists.car(obj);
            if (head == Symbols.SPLICE ||
                head == Symbols.NSPLICE)
                throw new ReadException
                    ("non list splice error: ~S.",
                     Lists.list(obj));
            //if (!Data.isNot(Backquote.member(Symbols.SPLICE, obj)))
            //    throw new ReadException
            //        ("non dotted splice error: ~S.",
            //         Lists.list(obj));
            for (Object l = obj; !Data.isAtom(l); l = Lists.cdr(l)) {
                if (Lists.car(l) == Symbols.SPLICE)
                    throw new ReadException
                        ("non dotted splice error: ~S.",
                         Lists.list(obj));
            }
            //if (!Data.isNot(Backquote.member(Symbols.NSPLICE, obj)))
            //    throw new ReadException
            //        ("non dotted splice error: ~S.",
            //         Lists.list(obj));
            for (Object l = obj; !Data.isAtom(l); l = Lists.cdr(l)) {
                if (Lists.car(l) == Symbols.NSPLICE)
                    throw new ReadException
                        ("non dotted splice error: ~S.",
                         Lists.list(obj));
            }
        }
        return Lists.list(Symbols.BACKQUOTE, obj);
    }
    static private Object readSharpQuote
        (PushbackReader in, Character ch, Object param, Env env)
        throws IOException {
        Object obj = read(in,Symbols.T,Symbols.NIL,Symbols.T,env);
        return Lists.list(Symbols.FUNCTION, obj);
    }
    static private Object readSharpBackslash
        (PushbackReader in, Character ch, Object param, Env env)
        throws IOException {
        Token token = new Token();
        readToken(in, token, '\\', STEP08, env);
        if (Logger.tracelevelp(env))
            Logger.trace("readSharpBackslash: token="+token, env);
        String s = token.cbuf.toString();
        int len = s.length();
        int x = -1;
        if (len < 1) {
            throw new ReadException
                ("empty string specified", Symbols.NIL);
        }
        else if (len == 1) {
            // single character: #\a -> 'a'
            x = s.charAt(0);
        }
        else /* len > 1 */ {
            // char name: #\space -> ' '
            s = s.toUpperCase();
            x = IO.nameToChar(s);
            if (x == -1) {
                // nameToChar conversion failed...
                if (s.charAt(0) == 'U') {
                    // unicode escape sequence: #\U61 -> 'a'
                    try {
                        x = Integer.parseInt(s.substring(1), 16);
                    } catch (NumberFormatException e) {
                        x = -1;
                    }
                }
                else if (IO.isDigit(s.charAt(0))) {
                    // octal value: #\141 -> 'a'
                    try {
                        x = Integer.parseInt(s, 8);
                        if (x > 0xff)
                            x = -1; // cannot exceed #\377('\u00ff')
                    } catch (NumberFormatException e) {
                        x = -1;
                    }
                }
            }
            if (x == -1)
                throw new ReadException
                    ("cannot understand: ~S", Lists.list(s));
        }
        return Data.toCharacter((char) x);
    }
    static private Object readSharpDot
        (PushbackReader in, Character ch, Object param, Env env)
        throws IOException {
        if (Data.isNot(env.get(Symbols.READ_EVAL))) {
            throw new ReadException
                ("read-eval not allowed", Symbols.NIL);
        }
        Object obj = read(in,Symbols.T,Symbols.NIL,Symbols.T,env);
        return Evaluator.eval(obj, env);
    }
    static private Object readSharpNumR
        (PushbackReader in, Character ch, Object param, Env env)
        throws IOException {
        Token token = new Token();
        readToken(in, token, in.read(), STEP08, env);
        if (Logger.tracelevelp(env))
            Logger.trace("readSharpNumR: token="+token, env);
        String s = token.cbuf.toString();
        int radix = Data.fixnum(param).intValue();
        return Data.toFixnum(Integer.parseInt(s, radix));
    }
    static private Object readSharpBar
        (PushbackReader in, Character ch, Object param, Env env)
        throws IOException {
        int sub_ch = ch.charValue();
        int depth = 0;
        int step = STEP01;
        int x = -1;
        //READ_COMMENT:
        while (true) {
            switch (step) {
            case STEP01:
                x = in.read();
                // fallthrough
            case STEP02:
                if (x == -1) {
                    throw new UnexpectedEofException();
                }
                else if (x == sub_ch) {
                    x = in.read();
                    if (x == -1) {
                        throw new UnexpectedEofException();
                    }
                    else if (x == '#') {
                        if (depth <= 0) {
                            step = DONE;
                        }
                        else {
                            depth--;
                            step = STEP01;
                        }
                    }
                    else {
                        step = STEP02;
                    }
                }
                else if (x == '#') {
                    x = in.read();
                    if (x == -1) {
                        throw new UnexpectedEofException();
                    }
                    else if (x == sub_ch) {
                        depth++;
                        step = STEP01;
                    }
                    else {
                        step = STEP02;
                    }
                }
                else {
                    step = STEP01;
                }
                break;
            case DONE:
                return Values.currentValues();
            }/* end of switch */
        }/* end of while loop */
    }
    static private Object readSharpColon
        (PushbackReader in, Character ch, Object param, Env env)
        throws IOException {
        Token token = new Token();
        readToken(in, token, in.read(), STEP08, env);
        if (Logger.tracelevelp(env))
            Logger.trace("readSharpColon: token="+token, env);
        String s = token.cbuf.toString();
        return Symbol.gensym(s);
    }
    static private Object readSharpEqual
        (PushbackReader in, Character ch, Object param, Env env)
        throws IOException {
        Number label = Data.fixnum(param);
        Object o = env.get(LABEL_OBJ_MAP);
        HashMap labelObjMap;
        if (o == Symbols.NIL) {
            labelObjMap = new HashMap();
            env.set(LABEL_OBJ_MAP, labelObjMap);
        }
        else {
            labelObjMap = (HashMap) o;
        }
        if (labelObjMap.containsKey(label)) {
            throw new ReadException
                ("may not be defined twice: #"+label+"=",
                 Symbols.NIL);
        }
        // put mapping: (label, ref)
        Ref ref = new Ref();
        labelObjMap.put(label, ref);
        if (Logger.tracelevelp(env))
            Logger.trace("readSharpEqual: #"+label+"="+ref, env);
        // read obj (ref may be contained in obj)
        Object obj = read(in,Symbols.T,Symbols.NIL,Symbols.T,env);
        // replace ref in obj by obj
        ref.replace(obj, obj, env);
        // replace ref in mapping by obj
        labelObjMap.put(label, obj);
        if (Logger.tracelevelp(env))
            Logger.trace("readSharpEqual: #"+label+"="+obj, env);
        return obj;
    }
    static private Object readSharpSharp
        (PushbackReader in, Character ch, Object param, Env env)
        throws IOException {
        Number label = Data.fixnum(param);
        Object o = env.get(LABEL_OBJ_MAP);
        if (o == Symbols.NIL) {
            throw new ReadException
                ("undefined label: #"+label+"#", Symbols.NIL);
        }
        HashMap labelObjMap = (HashMap) env.get(LABEL_OBJ_MAP);
        if (!labelObjMap.containsKey(label)) {
            throw new ReadException
                ("undefined label: #"+label+"#", Symbols.NIL);
        }
        Object obj = labelObjMap.get(label);
        if (Logger.tracelevelp(env))
            Logger.trace("readSharpSharp: #"+label+"# >> "+obj, env);
        return obj;
    }

    /*
     * reader macros (represented as nested classes)
     */

    /** Function associated with a macro character <code>#\"</code>. */
    static public final Subr DOUBLEQUOTE_READER
        = new DOUBLEQUOTE_READER();
    static class DOUBLEQUOTE_READER extends SystemSubr.SUBR2 {
        DOUBLEQUOTE_READER() { super("DOUBLEQUOTE-READER"); }
        public Object call2(Object r0, Object r1, Env env) {
            try {
                PushbackReader in = Reader.pushbackReader(r0, env);
                Character ch = Data.character(r1);
                return Reader.readDoublequote(in, ch, env);
            }
            catch (IOException e) {
                throw new StreamException
                    ("I/O error occurred while reading stream: ~S ~S.",
                     Lists.list(r0, r1), e);
            }
        }
    }
    /** Function associated with a macro character <code>#\#</code>. */
    static public final Subr SHARP_READER
        = new SHARP_READER();
    static class SHARP_READER extends SystemSubr.SUBR2 {
        SHARP_READER() { super("SHARP-READER"); }
        public Object call2(Object r0, Object r1, Env env) {
            try {
                PushbackReader in = Reader.pushbackReader(r0, env);
                Character ch = Data.character(r1);
                return Reader.readSharp(in, ch, env);
            }
            catch (IOException e) {
                throw new StreamException
                    ("I/O error occurred while reading stream: ~S ~S.",
                     Lists.list(r0, r1), e);
            }
        }
    }
    /** Function associated with a macro character <code>#\'</code>. */
    static public final Subr QUOTE_READER
        = new QUOTE_READER();
    static class QUOTE_READER extends SystemSubr.SUBR2 {
        QUOTE_READER() { super("QUOTE-READER"); }
        public Object call2(Object r0, Object r1, Env env) {
            try {
                PushbackReader in = Reader.pushbackReader(r0, env);
                Character ch = Data.character(r1);
                return Reader.readQuote(in, ch, env);
            }
            catch (IOException e) {
                throw new StreamException
                    ("I/O error occurred while reading stream: ~S ~S.",
                     Lists.list(r0, r1), e);
            }
        }
    }
    /** Function associated with a macro character <code>#\(</code>. */
    static public final Subr OPEN_PAREN_READER
        = new OPEN_PAREN_READER();
    static class OPEN_PAREN_READER extends SystemSubr.SUBR2 {
        OPEN_PAREN_READER() { super("OPEN-PAREN-READER"); }
        public Object call2(Object r0, Object r1, Env env) {
            try {
                PushbackReader in = Reader.pushbackReader(r0, env);
                Character ch = Data.character(r1);
                return Reader.readOpenParen(in, ch, env);
            }
            catch (IOException e) {
                throw new StreamException
                    ("I/O error occurred while reading stream: ~S ~S.",
                     Lists.list(r0, r1), e);
            }
        }
    }
    /** Function associated with a macro character <code>#\)</code>. */
    static public final Subr CLOSE_PAREN_READER
        = new CLOSE_PAREN_READER();
    static class CLOSE_PAREN_READER extends SystemSubr.SUBR2 {
        CLOSE_PAREN_READER() { super("CLOSE-PAREN-READER"); }
        public Object call2(Object r0, Object r1, Env env) {
            //try {
                PushbackReader in = Reader.pushbackReader(r0, env);
                Character ch = Data.character(r1);
                return Reader.readCloseParen(in, ch, env);
            //}
            //catch (IOException e) {
            //    throw new StreamException
            //        ("I/O error occurred while reading stream: ~S ~S.",
            //         Lists.list(r0, r1), e);
            //}
        }
    }
    /** Function associated with a macro character <code>#\,</code>. */
    static public final Subr COMMA_READER
        = new COMMA_READER();
    static class COMMA_READER extends SystemSubr.SUBR2 {
        COMMA_READER() { super("COMMA-READER"); }
        public Object call2(Object r0, Object r1, Env env) {
            try {
                PushbackReader in = Reader.pushbackReader(r0, env);
                Character ch = Data.character(r1);
                return Reader.readComma(in, ch, env);
            }
            catch (IOException e) {
                throw new StreamException
                    ("I/O error occurred while reading stream: ~S ~S.",
                     Lists.list(r0, r1), e);
            }
        }
    }
    /** Function associated with a macro character <code>#\;</code>. */
    static public final Subr SEMICOLON_READER
        = new SEMICOLON_READER();
    static class SEMICOLON_READER extends SystemSubr.SUBR2 {
        SEMICOLON_READER() { super("SEMICOLON-READER"); }
        public Object call2(Object r0, Object r1, Env env) {
            try {
                PushbackReader in = Reader.pushbackReader(r0, env);
                Character ch = Data.character(r1);
                return Reader.readSemicolon(in, ch, env);
            }
            catch (IOException e) {
                throw new StreamException
                    ("I/O error occurred while reading stream: ~S ~S.",
                     Lists.list(r0, r1), e);
            }
        }
    }
    /** Function associated with a macro character <code>#\`</code>. */
    static public final Subr BACKQUOTE_READER
        = new BACKQUOTE_READER();
    static class BACKQUOTE_READER extends SystemSubr.SUBR2 {
        BACKQUOTE_READER() { super("BACKQUOTE-READER"); }
        public Object call2(Object r0, Object r1, Env env) {
            try {
                PushbackReader in = Reader.pushbackReader(r0, env);
                Character ch = Data.character(r1);
                return Reader.readBackquote(in, ch, env);
            }
            catch (IOException e) {
                throw new StreamException
                    ("I/O error occurred while reading stream: ~S ~S.",
                     Lists.list(r0, r1), e);
            }
        }
    }
    /** Function associated with a dispatch character
        <code>#\#</code> and <code>#\'</code>. */
    static public final Subr SHARP_QUOTE_READER
        = new SHARP_QUOTE_READER();
    static class SHARP_QUOTE_READER extends SystemSubr.SUBR3 {
        SHARP_QUOTE_READER() { super("SHARP-QUOTE-READER"); }
        public Object call3(Object r0, Object r1, Object r2, Env env) {
            try {
                PushbackReader in = Reader.pushbackReader(r0, env);
                Character ch = Data.character(r1);
                return Reader.readSharpQuote(in, ch, r2, env);
            }
            catch (IOException e) {
                throw new StreamException
                    ("I/O error occurred while reading stream: ~S ~S ~S.",
                     Lists.list(r0, r1, r2), e);
            }
        }
    }
    /** Function associated with a dispatch character
        <code>#\#</code> and <code>#\\</code>. */
    static public final Subr SHARP_BACKSLASH_READER
        = new SHARP_BACKSLASH_READER();
    static class SHARP_BACKSLASH_READER extends SystemSubr.SUBR3 {
        SHARP_BACKSLASH_READER() { super("SHARP-BACKSLASH-READER"); }
        public Object call3(Object r0, Object r1, Object r2, Env env) {
            try {
                PushbackReader in = Reader.pushbackReader(r0, env);
                Character ch = Data.character(r1);
                return Reader.readSharpBackslash(in, ch, r2, env);
            }
            catch (IOException e) {
                throw new StreamException
                    ("I/O error occurred while reading stream: ~S ~S ~S.",
                     Lists.list(r0, r1, r2), e);
            }
        }
    }
    /** Function associated with a dispatch character
        <code>#\#</code> and <code>#\.</code>. */
    static public final Subr SHARP_DOT_READER
        = new SHARP_DOT_READER();
    static class SHARP_DOT_READER extends SystemSubr.SUBR3 {
        SHARP_DOT_READER() { super("SHARP-DOT-READER"); }
        public Object call3(Object r0, Object r1, Object r2, Env env) {
            try {
                PushbackReader in = Reader.pushbackReader(r0, env);
                Character ch = Data.character(r1);
                return Reader.readSharpDot(in, ch, r2, env);
            }
            catch (IOException e) {
                throw new StreamException
                    ("I/O error occurred while reading stream: ~S ~S ~S.",
                     Lists.list(r0, r1, r2), e);
            }
        }
    }
    /** Function associated with a dispatch character
        <code>#\#</code> and <code>#\B</code>. */
    static public final Subr SHARP_B_READER
        = new SHARP_B_READER();
    static class SHARP_B_READER extends SystemSubr.SUBR3 {
        SHARP_B_READER() { super("SHARP-B-READER"); }
        public Object call3(Object r0, Object r1, Object r2, Env env) {
            try {
                PushbackReader in = Reader.pushbackReader(r0, env);
                Character ch = Data.character(r1);
                Integer num = Data.toFixnum(2);
                return Reader.readSharpNumR(in, ch, num, env);
            }
            catch (IOException e) {
                throw new StreamException
                    ("I/O error occurred while reading stream: ~S ~S ~S.",
                     Lists.list(r0, r1, r2), e);
            }
        }
    }
    /** Function associated with a dispatch character
        <code>#\#</code> and <code>#\O</code>. */
    static public final Subr SHARP_O_READER
        = new SHARP_O_READER();
    static class SHARP_O_READER extends SystemSubr.SUBR3 {
        SHARP_O_READER() { super("SHARP-O-READER"); }
        public Object call3(Object r0, Object r1, Object r2, Env env) {
            try {
                PushbackReader in = Reader.pushbackReader(r0, env);
                Character ch = Data.character(r1);
                Integer num = Data.toFixnum(8);
                return Reader.readSharpNumR(in, ch, num, env);
            }
            catch (IOException e) {
                throw new StreamException
                    ("I/O error occurred while reading stream: ~S ~S ~S.",
                     Lists.list(r0, r1, r2), e);
            }
        }
    }
    /** Function associated with a dispatch character
        <code>#\#</code> and <code>#\X</code>. */
    static public final Subr SHARP_X_READER
        = new SHARP_X_READER();
    static class SHARP_X_READER extends SystemSubr.SUBR3 {
        SHARP_X_READER() { super("SHARP-X-READER"); }
        public Object call3(Object r0, Object r1, Object r2, Env env) {
            try {
                PushbackReader in = Reader.pushbackReader(r0, env);
                Character ch = Data.character(r1);
                Integer num = Data.toFixnum(16);
                return Reader.readSharpNumR(in, ch, num, env);
            }
            catch (IOException e) {
                throw new StreamException
                    ("I/O error occurred while reading stream: ~S ~S ~S.",
                     Lists.list(r0, r1, r2), e);
            }
        }
    }
    /** Function associated with a dispatch character
        <code>#\#</code> and <code>#\R</code>. */
    static public final Subr SHARP_NUM_R_READER
        = new SHARP_NUM_R_READER();
    static class SHARP_NUM_R_READER extends SystemSubr.SUBR3 {
        SHARP_NUM_R_READER() { super("SHARP-NUM-R-READER"); }
        public Object call3(Object r0, Object r1, Object r2, Env env) {
            try {
                PushbackReader in = Reader.pushbackReader(r0, env);
                Character ch = Data.character(r1);
                return Reader.readSharpNumR(in, ch, r2, env);
            }
            catch (IOException e) {
                throw new StreamException
                    ("I/O error occurred while reading stream: ~S ~S ~S.",
                     Lists.list(r0, r1, r2), e);
            }
        }
    }
    /** Function associated with a dispatch character
        <code>#\#</code> and <code>#\|</code>. */
    static public final Subr SHARP_BAR_READER
        = new SHARP_BAR_READER();
    static class SHARP_BAR_READER extends SystemSubr.SUBR3 {
        SHARP_BAR_READER() { super("SHARP-BAR-READER"); }
        public Object call3(Object r0, Object r1, Object r2, Env env) {
            try {
                PushbackReader in = Reader.pushbackReader(r0, env);
                Character ch = Data.character(r1);
                return Reader.readSharpBar(in, ch, r2, env);
            }
            catch (IOException e) {
                throw new StreamException
                    ("I/O error occurred while reading stream: ~S ~S ~S.",
                     Lists.list(r0, r1, r2), e);
            }
        }
    }
    /** Function associated with a dispatch character
        <code>#\#</code> and <code>#\:</code>. */
    static public final Subr SHARP_COLON_READER
        = new SHARP_COLON_READER();
    static class SHARP_COLON_READER extends SystemSubr.SUBR3 {
        SHARP_COLON_READER() { super("SHARP-COLON-READER"); }
        public Object call3(Object r0, Object r1, Object r2, Env env) {
            try {
                PushbackReader in = Reader.pushbackReader(r0, env);
                Character ch = Data.character(r1);
                return Reader.readSharpColon(in, ch, r2, env);
            }
            catch (IOException e) {
                throw new StreamException
                    ("I/O error occurred while reading stream: ~S ~S ~S.",
                     Lists.list(r0, r1, r2), e);
            }
        }
    }
    /** Function associated with a dispatch character
        <code>#\#</code> and <code>#\=</code>. */
    static public final Subr SHARP_EQUAL_READER
        = new SHARP_EQUAL_READER();
    static class SHARP_EQUAL_READER extends SystemSubr.SUBR3 {
        SHARP_EQUAL_READER() { super("SHARP-EQUAL-READER"); }
        public Object call3(Object r0, Object r1, Object r2, Env env) {
            try {
                PushbackReader in = Reader.pushbackReader(r0, env);
                Character ch = Data.character(r1);
                return Reader.readSharpEqual(in, ch, r2, env);
            }
            catch (IOException e) {
                throw new StreamException
                    ("I/O error occurred while reading stream: ~S ~S ~S.",
                     Lists.list(r0, r1, r2), e);
            }
        }
    }
    /** Function associated with a dispatch character
        <code>#\#</code> and <code>#\#</code>. */
    static public final Subr SHARP_SHARP_READER
        = new SHARP_SHARP_READER();
    static class SHARP_SHARP_READER extends SystemSubr.SUBR3 {
        SHARP_SHARP_READER() { super("SHARP-SHARP-READER"); }
        public Object call3(Object r0, Object r1, Object r2, Env env) {
            try {
                PushbackReader in = Reader.pushbackReader(r0, env);
                Character ch = Data.character(r1);
                return Reader.readSharpSharp(in, ch, r2, env);
            }
            catch (IOException e) {
                throw new StreamException
                    ("I/O error occurred while reading stream: ~S ~S ~S.",
                     Lists.list(r0, r1, r2), e);
            }
        }
    }

    /*
     * other nested classes
     */

    /** token used by the reader */
    static private class Token {
        final StringBuffer cbuf = new StringBuffer();
        final AttrBuffer abuf = new AttrBuffer();
        boolean escapeOccurred = false;
        boolean symInternal = false;
        int posIllegal = -1;
        int posPackageMarker = -1;
        int posUnexpectedPackageMarker = -1;
        void add(char c, int a) {
            cbuf.append(c);
            abuf.append(a);
        }
        void checkAttr() {
            int ret = -1;
            for (int i = 0, j = abuf.length(); i < j; i++) {
                int a = abuf.attrAt(i);
                if (a == Readtable.ATTR_ILLEGAL) {
                    posIllegal = i;
                    break;
                }
                else if (a == Readtable.ATTR_PACKAGE_MARKER) {
                    if (posPackageMarker < 0) {
                        posPackageMarker = i;
                    }
                    else {
                        if (posPackageMarker == i-1) {
                            symInternal = true;
                        }
                        else {
                            posUnexpectedPackageMarker = i;
                            break;
                        }
                    }
                }
            }
        }
        public String toString() {
            return "[\""+cbuf+"\", "+abuf+" "+escapeOccurred+"]";
        }
    }
    /** attribute(int) vector */
    static private class AttrBuffer {
        static final int SIZE_UNIT = 64;
        int[] attrs = new int[SIZE_UNIT];
        int len = 0;
        AttrBuffer append(int attr) {
            attrs[len] = attr;
            len++;
            if (len == attrs.length) {
                int newlen = len + SIZE_UNIT;
                if (newlen < 0)
                    newlen = Integer.MAX_VALUE;
                if (len < newlen) {
                    int[] oldattrs = attrs;
                    int[] newattrs = new int[newlen];
                    System.arraycopy(oldattrs, 0, newattrs, 0, len);
                    attrs = newattrs;
                }
            }
            return this;
        }
        int attrAt(int i) {
            if (i < len)
                return attrs[i];
            else
                throw new IndexOutOfBoundsException
                    ("index must be less than "+len+": "+i);
        }
        int length() {
            return len;
        }
        public String toString() {
            if (len <= 0)
                return "[]";
            StringBuffer buf = new StringBuffer()
                .append("[").append(attrs[0]);
            for (int i = 0; i < len; i++)
                buf.append(",").append(attrs[i]);
            return buf.append("]").toString();
        }
    }
    ///** reader internal error */
    //static private class NotReached extends IllegalStateException {
    //    NotReached(int step) {
    //        super("illegal step detected: "+step);
    //    }
    //}
    /** stub reference of the labeled object */
    static private class Ref {
        /** replaces this ref contained in form by obj */
        void replace(Object form, Object obj, Env env) {
            if (!Data.isPair(form)) {
                return;
            }
            // car
            Object car = Lists.car(form);
            if (car == this) {
                Lists.rplaca(form, obj);
                if (Logger.tracelevelp(env))
                    Logger.trace("replace:car: "+this+" >> "+obj, env);
            } else {
                replace(car, obj, env);
            }
            // cdr
            Object cdr = Lists.cdr(form);
            if (cdr == this) {
                Lists.rplacd(form, obj);
                if (Logger.tracelevelp(env))
                    Logger.trace("replace:cdr: "+this+" >> "+obj, env);
            } else {
                replace(cdr, obj, env);
            }
        }
    }
    private Reader() {}
}
