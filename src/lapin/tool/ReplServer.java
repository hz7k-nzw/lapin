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
package lapin.tool;
import lapin.io.IO;
import lapin.io.Printer;
import lapin.lang.Lisp;
import lapin.lang.Repl;
import lapin.lang.Symbols;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.net.ServerSocket;
import java.net.Socket;

public class ReplServer implements Runnable {
    /** Port number that the server listens. */
    private int port;
    /** Character encoding applied to the socket stream.
        Either {@link java.lang.String} or {@link Symbols#NIL NIL}
        is exptected. If NIL is specified, then the platform's default
        character encoding will be applied. */
    private Object enc;

    public ReplServer(int port, Object enc) {
        this.port = port;
        this.enc = enc;
    }
    public ReplServer(int port) {
        this(port, Symbols.NIL);
    }
    public void run() {
        System.out.println("[server] start: "+port);
        try {
            ServerSocket ss = new ServerSocket(port);
            while (true) {
                Socket sock = ss.accept();
                Thread clientHandler = makeClientHandler(sock);
                clientHandler.start();
            }
        }
        catch (IOException e) {
            throw new RuntimeException("I/O error occurred", e);
        }
        finally {
            System.out.println("[server] end: "+port);
        }
    }
    private Thread makeClientHandler(final Socket sock) {
        return new Thread() {
            public void run() {
                System.out.println("[client-handler] start: "+sock);
                InputStream in = null;
                OutputStream out = null;
                try {
                    in = sock.getInputStream();
                    out = sock.getOutputStream();
                    InputStream binIn = IO.wrapInputStream
                        (in, Symbols.T);
                    OutputStream binOut = IO.wrapOutputStream
                        (out, Symbols.T);
                    Reader charIn = IO.wrapReader
                        (IO.toReader(in, enc), Symbols.T);
                    Writer charOut = IO.wrapWriter
                        (IO.toWriter(out, enc), Symbols.T);
                    Lisp lisp = new Lisp();
                    lisp.set(Symbols.SYSTEM_BIN_IN, binIn);
                    lisp.set(Symbols.SYSTEM_BIN_OUT, binOut);
                    lisp.set(Symbols.SYSTEM_BIN_ERR, binOut);
                    lisp.set(Symbols.SYSTEM_IN, charIn);
                    lisp.set(Symbols.SYSTEM_OUT, charOut);
                    lisp.set(Symbols.SYSTEM_ERR, charOut);
                    lisp.set(Symbols.TERMINAL_IN, charIn);
                    lisp.set(Symbols.TERMINAL_OUT, charOut);
                    lisp.set(Symbols.LOG_OUT, charOut);
                    Printer.format("hello!~%", Symbols.NIL,
                                   Symbols.NIL, lisp.getEnv());
                    new Repl(lisp).run();
                    Printer.format("bye!~%", Symbols.NIL,
                                   Symbols.NIL, lisp.getEnv());
                    sock.shutdownInput();
                    sock.shutdownOutput();
                } catch (IOException e) {
                    throw new RuntimeException("I/O error occurred", e);
                } finally {
                    IO.close(in);
                    IO.close(out);
                    try {
                        sock.close();
                    } catch (IOException ignore) {
                    }
                    System.out.println("[client-handler] end: "+sock);
                }
            }};
    }
    static public void main(String[] args) {
        int port = Integer.parseInt(args[0]);
        new ReplServer(port).run();
    }
}
