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
import lapin.lang.Data;
import lapin.lang.Env;
import lapin.lang.Lists;
import lapin.lang.Symbol;
import lapin.lang.Symbols;
import lapin.lang.TypeException;
import lapin.util.Logger;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.PushbackInputStream;
import java.io.PushbackReader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Reader;
import java.io.Writer;
import java.math.BigInteger;

/**
 * Operations for I/O.
 */
public final class IO {
    /** Uninterned symbol that denotes the end of file. */
    static public final Object EOF = Symbol.gensym("_EOF_");

    static private final Object DEFAULT_WRAP_OPTIONS_BIN_IN
        = Lists.list(Symbols.KW_USE_BUFFER);
    static private final Object DEFAULT_WRAP_OPTIONS_BIN_OUT
        = Lists.list(Symbols.KW_USE_BUFFER);
    static private final Object DEFAULT_WRAP_OPTIONS_CHAR_IN
        = Lists.list(Symbols.KW_USE_BUFFER,Symbols.KW_USE_UNREAD);
    static private final Object DEFAULT_WRAP_OPTIONS_CHAR_OUT
        = Lists.list(Symbols.KW_USE_BUFFER,Symbols.KW_USE_PRINT);

    static class StringPushbackReader extends PushbackReader {
        StringPushbackReader(String str) {
            super(new StringReader(str));
        }
        StringReader getStringReader() {
            return (StringReader) super.in;
        }
    }
    static class StringPrintWriter extends PrintWriter {
        StringPrintWriter() {
            super(new StringWriter());
        }
        StringWriter getStringWriter() {
            return (StringWriter) super.out;
        }
    }

    /**
     * Creates, opens, and returns a stream that is connected to
     * the file specified by <code>src</code>.
     * @param src {@link String} or {@link java.io.File}
     * @param direction Keyword, <code>:INPUT or :OUTPUT</code>.
     *        If <code>:INPUT</code> is specified,
     *        then the input stream is created.
     *        If <code>:OUTPUT</code> is specified,
     *        then the output stream is created.
     * @param elementType Keyword, <code>:CHARACTER or :SIGNED-BYTE</code>.
     *        If <code>:CHARACTER</code> is specified,
     *        then the character stream is created.
     *        If <code>:SIGNED-BYTE</code> is specified,
     *        then the binary stream is created.
     * @param ifExists Keyword which specifies the action to be taken
     *        if the :direction is :output and a file specified by
     *        <code>src</code> already exists. 
     *        If <code>:OVERWRITE</code> is specified,
     *        then bytes will be written from the beginning of the file.
     *        If <code>:APPEND</code> is specified,
     *        then bytes will be written to the end of the file.
     * @param ifDoesNotExist Keyword which specifies the action to be taken
     *        if the :direction is :output and a file specified by
     *        <code>src</code> does not exist.
     *        If <code>:ERROR</code> is specified,
     *        then {@link FileException} will be thrown.
     *        If <code>:CREATE</code> is specified,
     *        then an empty file will be created.
     * @param externalFormat NIL or {@link String} that denotes the
     *        character encoding. If NIL is specified, then the platform's
     *        default character encoding will be used.
     * @param wrapOptions T, NIL or List of keywords.
     *        If T, then the default wrapping is applied.
     *        Else if NIL, then no wrapping is applied.
     *        Else, wrapping specified by the list is applied.
     *        The default wrapping and vailable keywords depends on
     *        a method that does wrapping.
     *        If the binary input stream is created, wrapping is done by
     *        {@link #wrapInputStream wrapInputStream}.
     *        If the binary output stream is created, wrapping is done by
     *        {@link #wrapOutputStream wrapOutputStream}.
     *        If the character input stream is created, wrapping is done by
     *        {@link #wrapReader wrapReader}.
     *        If the character output stream is created, wrapping is done by
     *        {@link #wrapWriter wrapWriter}.
     * @return Stream
     * @throws StreamException
     * @throws FileException
     * @see #openInputStream openInputStream
     * @see #openOutputStream openOutputStream
     * @see #toReader toReader
     * @see #toWriter toWriter
     * @see #wrapInputStream wrapInputStream
     * @see #wrapOutputStream wrapOutputStream
     * @see #wrapReader wrapReader
     * @see #wrapWriter wrapWriter
     */
    static public Object open(Object src,
                              Object direction, Object elementType,
                              Object ifExists, Object ifDoesNotExist,
                              Object externalFormat, Object wrapOptions) {
        if (direction == Symbols.KW_INPUT) {
            if (elementType == Symbols.CHARACTER) {
                InputStream in = openInputStream(src);
                boolean success = false;
                try {
                    Reader r;
                    r = toReader(in, externalFormat);
                    r = wrapReader(r, wrapOptions);
                    success = true;
                    return r;
                } finally {
                    if (!success)
                        IO.close(in);
                }
            }
            else if (elementType == Symbols.SIGNED_BYTE) {
                InputStream in = openInputStream(src);
                boolean success = false;
                try {
                    in = wrapInputStream(in, wrapOptions);
                    success = true;
                    return in;
                } finally {
                    if (!success)
                        IO.close(in);
                }
            }
            else {
                throw new FileException
                    ("unsupported element-type: ~S.",
                     Lists.list(elementType));
            }
        }
        else if (direction == Symbols.KW_OUTPUT) {
            boolean append;
            if (ifExists == Symbols.KW_OVERWRITE) {
                append = false;
            }
            else if (ifExists == Symbols.KW_APPEND) {
                append = true;
            }
            else {
                throw new FileException
                    ("unsupported if-exists: ~S.",
                     Lists.list(ifExists));
            }
            boolean checkFileExists;
            if (ifDoesNotExist == Symbols.KW_ERROR) {
                checkFileExists = true;
            }
            else if (ifDoesNotExist == Symbols.KW_CREATE) {
                checkFileExists = false;
            }
            else {
                throw new FileException
                    ("unsupported if-does-not-exist: ~S.",
                     Lists.list(ifDoesNotExist));
            }
            if (elementType == Symbols.CHARACTER) {
                OutputStream out = openOutputStream(src, append,
                                                    checkFileExists);
                boolean success = false;
                try {
                    Writer w;
                    w = toWriter(out, externalFormat);
                    w = wrapWriter(w, wrapOptions);
                    success = true;
                    return w;
                }
                finally {
                    if (!success)
                        IO.close(out);
                }
            }
            else if (elementType == Symbols.SIGNED_BYTE) {
                OutputStream out = openOutputStream(src, append,
                                                    checkFileExists);
                boolean success = false;
                try {
                    out = wrapOutputStream(out, wrapOptions);
                    success = true;
                    return out;
                } finally {
                    if (!success)
                        IO.close(out);
                }
            }
            else {
                throw new FileException
                    ("unsupported elementType: ~S.",
                     Lists.list(elementType));
            }
        }
        else {
            throw new FileException
                ("unsupported direction: ~S.", Lists.list(direction));
        }
    }
    /**
     * Creates, opens, and returns a binary input stream that is
     * connected to the file specified by <code>src</code>.
     * @param src {@link String} or {@link java.io.File}
     * @return Binary input stream
     * @throws StreamException
     * @throws FileException
     */
    static public InputStream openInputStream(Object src) {
        try {
            if (Data.isString(src)) {
                File inFile = new File(Data.string(src));
                checkInFile(inFile);
                return new FileInputStream(inFile);
            }
            else if (Data.isFile(src)) {
                File inFile = Data.file(src);
                checkInFile(inFile);
                return new FileInputStream(inFile);
            }
            else
                throw new TypeException
                    ("src must be string or java.io.File: ~S.",
                     Lists.list(src));
        }
        catch (FileNotFoundException e) {
            throw new FileException
                ("file not found: ~S.", Lists.list(src), e);
        }
        catch (IOException e) {
            throw new StreamException
                ("failed to open stream: ~S.", Lists.list(src), e);
        }
    }
    /**
     * Creates, opens, and returns a binary output stream that is
     * connected to the file specified by <code>src</code>.
     * @param src {@link String} or {@link java.io.File}
     * @return Binary output stream
     * @throws StreamException
     * @throws FileException
     */
    static public OutputStream openOutputStream(Object src) {
        return openOutputStream(src, false, false);
    }
    /**
     * Creates, opens, and returns a binary output stream that is
     * connected to the file specified by <code>src</code>.
     * @param src {@link String} or {@link java.io.File}
     * @param append If true, then bytes will be written
     *        to the end of the file rather than the beginning.
     * @param checkFileExists If true, file specified by <code>src</code>
     *        must exist. Otherwise {@link FileException} will be thrown.
     * @return Binary output stream
     * @throws StreamException
     * @throws FileException
     */
    static public OutputStream openOutputStream(Object src,
                                                boolean append,
                                                boolean checkFileExists) {
        try {
            if (Data.isString(src)) {
                File outFile = new File(Data.string(src));
                checkOutFile(outFile, checkFileExists);
                return new FileOutputStream(outFile, append);
            }
            else if (Data.isFile(src)) {
                File outFile = Data.file(src);
                checkOutFile(outFile, checkFileExists);
                return new FileOutputStream(outFile, append);
            }
            else
                throw new TypeException
                    ("src must be string or java.io.File: ~S.",
                     Lists.list(src));
        }
        catch (FileNotFoundException e) {
            throw new FileException
                ("file not found: ~S.", Lists.list(src), e);
        }
        catch (IOException e) {
            throw new StreamException
                ("failed to open stream: ~S.", Lists.list(src), e);
        }
    }
    /**
     * Creates a character input stream from the specified binary
     * input stream.
     * @param in Binary input stream
     * @param enc NIL or {@link String} that denotes the character encoding.
     *        If NIL is specified, then the platform's default character
     *        encoding will be used.
     * @return Character input stream
     * @throws StreamException
     */
    static public Reader toReader(InputStream in, Object enc) {
        try {
            if (enc == Symbols.NIL)
                return new InputStreamReader(in);
            else if (Data.isString(enc))
                return new InputStreamReader(in, Data.string(enc));
            else
                throw new TypeException
                    ("enc must be nil or string: ~S.", Lists.list(enc));
        }
        catch (IOException e) {
            throw new StreamException
                ("failed to convert byte-stream to char-stream: ~S ~S.",
                 Lists.list(in, enc), e);
        }
    }
    /**
     * Creates a character output stream from the specified binary
     * output stream.
     * @param out Binary output stream
     * @param enc NIL or {@link String} that denotes the character encoding.
     *        If NIL is specified, then the platform's default character
     *        encoding will be used.
     * @return Character output stream
     * @throws StreamException
     */
    static public Writer toWriter(OutputStream out, Object enc) {
        try {
            if (enc == Symbols.NIL)
                return new OutputStreamWriter(out);
            else if (Data.isString(enc))
                return new OutputStreamWriter(out, Data.string(enc));
            else
                throw new TypeException
                    ("enc must be nil or string: ~S.", Lists.list(enc));
        }
        catch (IOException e) {
            throw new StreamException
                ("failed to convert byte-stream to char-stream: ~S ~S.",
                 Lists.list(out, enc), e);
        }
    }
    /**
     * Wraps the specified stream with wrappers specified by
     * <code>wrapOptions</code>.
     * @param in Binary input stream
     * @param wrapOptions T, NIL or List of keywords.
     *        If T, then the default wrapping is applied.
     *        Else if NIL, then no wrapping is applied.
     *        Else, for each keyword in the list (from left to right)
     *        wrapper specified by the keyword is applied to the stream.
     *        Available options are;
     *        <code>:USE-BUFFER</code> -&gt; {@link BufferedInputStream},
     *        <code>:USE-UNREAD</code> -&gt; {@link PushbackInputStream},
     *        <code>:USE-OBJECT</code> -&gt; {@link ObjectInputStream}.
     *        The default wrapping is <code>(:USE-BUFFER)</code>.
     * @return Wrapped binary input stream
     * @throws StreamException
     */
    static public InputStream wrapInputStream
        (InputStream in, Object wrapOptions) {
        if (wrapOptions == Symbols.T) {
            wrapOptions = DEFAULT_WRAP_OPTIONS_BIN_IN;
        }
        for (Object l = wrapOptions; !Lists.isEnd(l); l = Lists.cdr(l)) {
            Object key = Lists.car(l);
            //System.err.println(in+": wrap:"+key);
            if (key == Symbols.KW_USE_BUFFER) {
                in = new BufferedInputStream(in);
            }
            else if (key == Symbols.KW_USE_UNREAD) {
                in = new PushbackInputStream(in);
            }
            else if (key == Symbols.KW_USE_OBJECT) {
                try {
                    in = new ObjectInputStream(in);
                } catch (IOException e) {
                    throw new StreamException
                        ("failed to create object input stream.",
                         Symbols.NIL, e);
                }
            }
            else {
                throw new StreamException
                    ("unsupported option: ~S", Lists.list(key));
            }
        }
        return in;
    }
    /**
     * Wraps the specified stream with wrappers specified by
     * <code>wrapOptions</code>.
     * @param out Binary output stream
     * @param wrapOptions T, NIL or List of keywords.
     *        If T, then the default wrapping is applied.
     *        Else if NIL, then no wrapping is applied.
     *        Else, for each keyword in the list (from left to right)
     *        wrapper specified by the keyword is applied to the stream.
     *        Available options are;
     *        <code>:USE-BUFFER</code> -&gt; {@link BufferedOutputStream},
     *        <code>:USE-PRINT</code> -&gt; {@link PrintStream},
     *        <code>:USE-OBJECT</code> -&gt; {@link ObjectOutputStream}.
     *        The default wrapping is <code>(:USE-BUFFER)</code>.
     * @return Wrapped binary output stream
     * @throws StreamException
     */
    static public OutputStream wrapOutputStream
        (OutputStream out, Object wrapOptions) {
        if (wrapOptions == Symbols.T) {
            wrapOptions = DEFAULT_WRAP_OPTIONS_BIN_OUT;
        }
        for (Object l = wrapOptions; !Lists.isEnd(l); l = Lists.cdr(l)) {
            Object key = Lists.car(l);
            //System.err.println(out+": wrap:"+key);
            if (key == Symbols.KW_USE_BUFFER) {
                out = new BufferedOutputStream(out);
            }
            else if (key == Symbols.KW_USE_PRINT) {
                out = new PrintStream(out);
            }
            else if (key == Symbols.KW_USE_OBJECT) {
                try {
                    out = new ObjectOutputStream(out);
                } catch (IOException e) {
                    throw new StreamException
                        ("failed to create object output stream.",
                         Symbols.NIL, e);
                }
            }
            else {
                throw new StreamException
                    ("unsupported option: ~S", Lists.list(key));
            }
        }
        return out;
    }
    /**
     * Wraps the specified stream with wrappers specified by
     * <code>wrapOptions</code>.
     * @param r Character input stream
     * @param wrapOptions T, NIL or List of keywords.
     *        If T, then the default wrapping is applied.
     *        Else if NIL, then no wrapping is applied.
     *        Else, for each keyword in the list (from left to right)
     *        wrapper specified by the keyword is applied to the stream.
     *        Available options are;
     *        <code>:USE-BUFFER</code> -&gt; {@link BufferedReader},
     *        <code>:USE-UNREAD</code> -&gt; {@link PushbackReader},
     *        <code>:USE-LINENUMBER</code> -&gt; {@link LineNumberReader}.
     *        The default wrapping is <code>(:USE-BUFFER :USE-UNREAD)</code>.
     * @return Wrapped character input stream
     * @throws StreamException
     */
    static public Reader wrapReader(Reader r, Object wrapOptions) {
        if (wrapOptions == Symbols.T) {
            wrapOptions = DEFAULT_WRAP_OPTIONS_CHAR_IN;
        }
        for (Object l = wrapOptions; !Lists.isEnd(l); l = Lists.cdr(l)) {
            Object key = Lists.car(l);
            //System.err.println(r+": wrap:"+key);
            if (key == Symbols.KW_USE_BUFFER) {
                r = new BufferedReader(r);
            }
            else if (key == Symbols.KW_USE_UNREAD) {
                r = new PushbackReader(r);
            }
            else if (key == Symbols.KW_USE_LINENUMBER) {
                r = new LineNumberReader(r);
            }
            else {
                throw new StreamException
                    ("unsupported option: ~S", Lists.list(key));
            }
        }
        return r;
    }
    /**
     * Wraps the specified stream with wrappers specified by
     * <code>wrapOptions</code>.
     * @param w Character output stream
     * @param wrapOptions T, NIL or List of keywords.
     *        If T, then the default wrapping is applied.
     *        Else if NIL, then no wrapping is applied.
     *        Else, for each keyword in the list (from left to right)
     *        wrapper specified by the keyword is applied to the stream.
     *        Available options are;
     *        <code>:USE-BUFFER</code> -&gt; {@link BufferedWriter},
     *        <code>:USE-PRINT</code> -&gt; {@link PrintWriter}.
     *        The default wrapping is <code>(:USE-BUFFER :USE-PRINT)</code>.
     * @return Wrapped character output stream
     * @throws StreamException
     */
    static public Writer wrapWriter(Writer w, Object wrapOptions) {
        if (wrapOptions == Symbols.T) {
            wrapOptions = DEFAULT_WRAP_OPTIONS_CHAR_OUT;
        }
        for (Object l = wrapOptions; !Lists.isEnd(l); l = Lists.cdr(l)) {
            Object key = Lists.car(l);
            //System.err.println(w+": wrap:"+key);
            if (key == Symbols.KW_USE_BUFFER) {
                w = new BufferedWriter(w);
            }
            else if (key == Symbols.KW_USE_PRINT) {
                w = new PrintWriter(w);
            }
            else {
                throw new StreamException
                    ("unsupported option: ~S", Lists.list(key));
            }
        }
        return w;
    }
    /** Creates a character input stream
        whose source is the specified string. */
    static public Reader makeStringReader(String str) {
        return new StringPushbackReader(str);
    }
    /** Creates a character output stream
        that collects its output in a string buffer,
        which can then be used to construct a string. */
    static public Writer makeStringWriter() {
        return new StringPrintWriter();
    }
    /** Retrieves a {@link StringReader} from <code>stream</code>
        created by {@link #makeStringReader makeStringReader}. */
    static public StringReader getStringReader(Object stream) {
        if (stream instanceof StringPushbackReader) {
            StringPushbackReader spr = (StringPushbackReader) stream;
            return spr.getStringReader();
        }
        else if (stream instanceof StringReader) {
            return (StringReader) stream;
        }
        else {
            throw new TypeException
                ("cannot retrieve StringReader from stream: ~S.",
                 Lists.list(stream));
        }
    }
    /** Retrieves a {@link StringWriter} from <code>stream</code>
        created by {@link #makeStringWriter makeStringWriter}. */
    static public StringWriter getStringWriter(Object stream) {
        if (stream instanceof StringPrintWriter) {
            StringPrintWriter spw = (StringPrintWriter) stream;
            return spw.getStringWriter();
        }
        else if (stream instanceof StringWriter) {
            return (StringWriter) stream;
        }
        else {
            throw new TypeException
                ("cannot retrieve StringWriter from stream: ~S.",
                 Lists.list(stream));
        }
    }
    /**
     * Flushs a <code>stream</code>.
     * @param stream Stream
     */
    static public void flush(Object stream) {
        try {
            if (Data.isWriter(stream))
                Data.writer(stream).flush();
            else if (Data.isOutputStream(stream))
                Data.outputStream(stream).flush();
        }
        catch (IOException ignore) {
        }
    }
    /**
     * Closes a <code>stream</code>.
     * @param stream Stream
     */
    static public void close(Object stream) {
        close(stream, false);
    }
    /**
     * Closes a <code>stream</code>.
     * @param stream Stream
     * @param abort Currently, this parameter is ignored
     */
    static public void close(Object stream, boolean abort) {
        try {
            if (Data.isReader(stream))
                Data.reader(stream).close();
            else if (Data.isWriter(stream))
                Data.writer(stream).close();
            else if (Data.isInputStream(stream))
                Data.inputStream(stream).close();
            else if (Data.isOutputStream(stream))
                Data.outputStream(stream).close();
        }
        catch (IOException ignore) {
        }
    }
    /**
     * Ensures that <code>stream</code> is the binary input stream.
     * If <code>stream</code> is NIL, then the value of the special
     * variable *SYSTEM-BIN-IN* is returned.
     * Else, <code>stream</code> is returned.
     */
    static public InputStream inputStream(Object stream, Env env) {
        Object o;
        if (stream == Symbols.NIL)
            o = env.get(Symbols.SYSTEM_BIN_IN);
        else
            o = stream;
        return Data.inputStream(o);
    }
    /**
     * Ensures that <code>stream</code> is the binary output stream.
     * If <code>stream</code> is NIL, then the value of the special
     * variable *SYSTEM-BIN-OUT* is returned.
     * Else, <code>stream</code> is returned.
     */
    static public OutputStream outputStream(Object stream, Env env) {
        Object o;
        if (stream == Symbols.NIL)
            o = env.get(Symbols.SYSTEM_BIN_OUT);
        else
            o = stream;
        return Data.outputStream(o);
    }
    /**
     * Ensures that <code>stream</code> is the character input stream.
     * If <code>stream</code> is NIL, then the value of the special
     * variable *SYSTEM-IN* is returned.
     * Else if <code>stream</code> is T, then the value of the special
     * variable *TERMINAL-IN* is returned.
     * Else, <code>stream</code> is returned.
     */
    static public Reader reader(Object stream, Env env) {
        Object o;
        if (stream == Symbols.NIL)
            o = env.get(Symbols.SYSTEM_IN);
        else if (stream == Symbols.T)
            o = env.get(Symbols.TERMINAL_IN);
        else
            o = stream;
        return Data.reader(o);
    }
    /**
     * Ensures that <code>stream</code> is the character output stream.
     * If <code>stream</code> is NIL, then the value of the special
     * variable *SYSTEM-OUT* is returned.
     * Else if <code>stream</code> is T, then the value of the special
     * variable *TERMINAL-OUT* is returned.
     * Else, <code>stream</code> is returned.
     */
    static public Writer writer(Object stream, Env env) {
        Object o;
        if (stream == Symbols.NIL)
            o = env.get(Symbols.SYSTEM_OUT);
        else if (stream == Symbols.T)
            o = env.get(Symbols.TERMINAL_OUT);
        else
            o = stream;
        return Data.writer(o);
    }
    /** Retrieves a value of the special variable specified by
        <code>key</code> and then returns a {@link java.io.File}
        object which represents a directory specified by the value. */
    static public File dir(Symbol key, Env env) {
        File dir;
        if (env.isBound(key)) {
            dir = new File(Data.string(env.get(key)));
            if (!dir.exists()) {
                throw new FileException
                    ("dir for ~S not found: ~S.",
                     Lists.list(key, dir));
            }
            if (!dir.isDirectory()) {
                throw new FileException
                    ("dir for ~S is not directory: ~S.",
                     Lists.list(key, dir));
            }
        }
        else {
            dir = null;
        }
        if (Logger.debuglevelp(env))
            Logger.debug("[getdir] dir for ~S: ~S",
                         Lists.list(key, dir), env);
        return dir;
    }
    static private void checkInFile(File inFile) {
        if (!inFile.exists())
            throw new FileException
                ("inFile not exists: ~S.", Lists.list(inFile));
        if (!inFile.isFile())
            throw new FileException
                ("inFile is not file: ~S.", Lists.list(inFile));
    }
    static private void checkOutFile(File outFile, boolean checkExists) {
        if (checkExists && !outFile.exists())
            throw new FileException
                ("outFile not exists: ~S.", Lists.list(outFile));
        if (checkExists && !outFile.isFile())
            throw new FileException
                ("outFile is not file: ~S.", Lists.list(outFile));
        File parent = outFile.getParentFile();
        if (parent != null && !parent.exists() && !parent.mkdirs())
            throw new FileException
                ("mkdirs failed: ~S.", Lists.list(parent));
    }
    /**
     * Returns a substring of <code>string</code> bounded by
     * <code>start</code>(inclusive) and <code>end</code>(exclusive).
     * @param string String
     * @param start NIL (considered as 0) or fixnum
     * @param end NIL (considered as length of <code>string</code>) or fixnum
     * @return Substring bounded by start and end
     */
    static public String substring(String string,
                                   Object start, Object end) {
        if (start != Symbols.NIL && end != Symbols.NIL)
            string = string.substring(Data.fixnum(start).intValue(),
                                      Data.fixnum(end).intValue());
        else if (start != Symbols.NIL && end == Symbols.NIL)
            string = string.substring(Data.fixnum(start).intValue(),
                                      string.length());
        else if (start == Symbols.NIL && end != Symbols.NIL)
            string = string.substring(0,
                                      Data.fixnum(end).intValue());
        return string;
    }

    /*
     * package private utilities
     */

    // uninterned symbol: for package private use only
    static final Symbol DOT = Symbol.gensym("_DOT_");
    static final Symbol MULTIPLE_DOT = Symbol.gensym("_MULTIPLE-DOT_");

    static Readtable readtable(Env env) {
        return env.lisp().getReadtable();
    }
    static Object parseNumber(String s) {
        int len = s.length();
        // s is an empty string -> cannot be number
        if (len == 0)
            return Symbols.NIL;

        // first char of s is not contained in "0123456789+-."
        // -> cannot be number
        if ("0123456789+-.".indexOf(s.charAt(0)) == -1)
            return Symbols.NIL;

        // remove last dot
        if (len > 1 && s.endsWith("."))
            s = s.substring(0, len-1);

        // try to convert s to number
        Object n;
        n = parseFixnum(s);
        if (n != Symbols.NIL)
            return n;
        n = parseBignum(s);
        if (n != Symbols.NIL)
            return n;
        n = parseFlonum(s);
        //if (n != Symbols.NIL)
        //    return n;
        return n;
    }
    static Object parseFixnum(String s) {
        try {
            return Data.toFixnum(Integer.parseInt(s));
        } catch (NumberFormatException e) {
            return Symbols.NIL;
        }
    }
    static Object parseFlonum(String s) {
        try {
            return Data.toFlonum(Double.parseDouble(s));
        } catch (NumberFormatException e) {
            return Symbols.NIL;
        }
    }
    static Object parseBignum(String s) {
        try {
            return new BigInteger(s);
        } catch (NumberFormatException e) {
            return Symbols.NIL;
        }
    }
    static Object parseDot(String s) {
        int i;
        for (i = 0; i < s.length(); i++) {
            if (s.charAt(i) != '.')
                return Symbols.NIL; // non-dot
        }
        if (i <= 0)
            return Symbols.NIL; // empty pname
        else if (i == 1)
            return DOT; // single dot
        else
            return MULTIPLE_DOT; // multiple dots
    }
    static boolean isDigit(int ch) {
        //return Character.isDigit((char) ch);
        return '0' <= ch && ch <= '9';
    }
    static int toDigit(int ch) {
        //return Character.digit((char) ch, 10);
        return ch - '0';
    }
    static int toUpcase(int ch) {
        return Character.toUpperCase((char) ch);
    }
    static int nameToChar(String name) {
        int x;
        if ("NUL".equals(name))
            x = 0x00;
        else if ("SOH".equals(name))
            x = 0x01;
        else if ("STX".equals(name))
            x = 0x02;
        else if ("ETX".equals(name))
            x = 0x03;
        else if ("EOT".equals(name))
            x = 0x04;
        else if ("ENQ".equals(name))
            x = 0x05;
        else if ("ACK".equals(name))
            x = 0x06;
        else if ("BEL".equals(name))
            x = 0x07;
        else if ("BACKSPACE".equals(name) ||
                 "BS".equals(name))
            x = 0x08;
        else if ("TAB".equals(name) ||
                 "HT".equals(name))
            x = 0x09;
        else if ("NEWLINE".equals(name) ||
                 "LINEFEED".equals(name) ||
                 "LF".equals(name))
            x = 0x0a;
        else if ("VT".equals(name))
            x = 0x0b;
        else if ("PAGE".equals(name) ||
                 "FF".equals(name))
            x = 0x0c;
        else if ("RETURN".equals(name) ||
                 "CR".equals(name))
            x = 0x0d;
        else if ("SO".equals(name))
            x = 0x0e;
        else if ("SI".equals(name))
            x = 0x0f;
        else if ("DLE".equals(name))
            x = 0x10;
        else if ("DC1".equals(name))
            x = 0x11;
        else if ("DC2".equals(name))
            x = 0x12;
        else if ("DC3".equals(name))
            x = 0x13;
        else if ("DC4".equals(name))
            x = 0x14;
        else if ("NAK".equals(name))
            x = 0x15;
        else if ("SYN".equals(name))
            x = 0x16;
        else if ("ETB".equals(name))
            x = 0x17;
        else if ("CAN".equals(name))
            x = 0x18;
        else if ("EM".equals(name))
            x = 0x19;
        else if ("SUB".equals(name))
            x = 0x1a;
        else if ("ESC".equals(name))
            x = 0x1b;
        else if ("FS".equals(name))
            x = 0x1c;
        else if ("GS".equals(name))
            x = 0x1d;
        else if ("RS".equals(name))
            x = 0x1e;
        else if ("US".equals(name))
            x = 0x1f;
        else if ("SPACE".equals(name))
            x = 0x20;
        else if ("RUBOUT".equals(name) ||
                 "DEL".equals(name))
            x = 0x7f;
        else
            x = -1;
        return x;
    }
    static String charToName(int x) {
        String name;
        switch(x) {
        case 0x00: // NUL
            name = "Nul";
            break;
        case 0x01: // SOH
            name = "Soh";
            break;
        case 0x02: // STX
            name = "Stx";
            break;
        case 0x03: // ETX
            name = "Etx";
            break;
        case 0x04: // EOT
            name = "Eot";
            break;
        case 0x05: // ENQ
            name = "Enq";
            break;
        case 0x06: // ACK
            name = "Ack";
            break;
        case 0x07: // BEL
            name = "Bel";
            break;
        case 0x08: // BS
            name = "Backspace";
            break;
        case 0x09: // HT
            name = "Tab";
            break;
        case 0x0a: // LF
            name = "Newline";
            //name = "LINEFEED";
            break;
        case 0x0b: // VT
            name = "Vt";
            break;
        case 0x0c: // FF
            name = "Page";
            break;
        case 0x0d: // CR
            name = "Return";
            break;
        case 0x0e: // SO
            name = "So";
            break;
        case 0x0f: // SI
            name = "Si";
            break;
        case 0x10: // DLE
            name = "Dle";
            break;
        case 0x11: // DC1
            name = "Dc1";
            break;
        case 0x12: // DC2
            name = "Dc2";
            break;
        case 0x13: // DC3
            name = "Dc3";
            break;
        case 0x14: // DC4
            name = "Dc4";
            break;
        case 0x15: // NAK
            name = "Nak";
            break;
        case 0x16: // SYN
            name = "Syn";
            break;
        case 0x17: // ETB
            name = "Etb";
            break;
        case 0x18: // CAN
            name = "Can";
            break;
        case 0x19: // EM
            name = "Em";
            break;
        case 0x1a: // SUB
            name = "Sub";
            break;
        case 0x1b: // ESC
            name = "Esc";
            break;
        case 0x1c: // FS
            name = "Fs";
            break;
        case 0x1d: // GS
            name = "Gs";
            break;
        case 0x1e: // RS
            name = "Rs";
            break;
        case 0x1f: // US
            name = "Us";
            break;
        case 0x20: // ' '
            name = "Space";
            break;
        case 0x7f: // DEL
            name = "Rubout";
            break;
        default:
            name = null;
            break;
        }
        return name;
    }

    private IO() {}
}
