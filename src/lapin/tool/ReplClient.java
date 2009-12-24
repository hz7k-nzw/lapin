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
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.Socket;

public class ReplClient implements Runnable {
    /** Host name. */
    private String host;
    /** Port number. */
    private int port;

    public ReplClient(String host, int port) {
        this.host = host;
        this.port = port;
    }
    public void run() {
        //System.out.println("[client] start: "+host+":"+port);
        try {
            Socket sock = new Socket(host, port);
            InputStream in = sock.getInputStream();
            OutputStream out = sock.getOutputStream();
            Thread sender = makeSender(sock, out);
            Thread receiver = makeReceiver(sock, in);
            sender.start();
            receiver.start();
            sender.join();
            receiver.join();
            in.close();
            out.close();
            sock.close();
        }
        catch (IOException e) {
            throw new RuntimeException("I/O error occurred", e);
        }
        catch (InterruptedException e) {
            throw new RuntimeException("Thread interrupted", e);
        }
        //finally {
        //    System.out.println("[client] end: "+host+":"+port);
        //}
    }
    private Thread makeSender(final Socket sock,
                              final OutputStream out) {
        return new Thread() {
            public void run() {
                try {
                    int b;
                    while ((b = System.in.read()) != -1) {
                        out.write(b);
                        out.flush();
                    }
                    //out.close();
                    sock.shutdownOutput();
                }
                catch (IOException e) {
                    throw new RuntimeException("I/O error occurred", e);
                }
            }};
    }
    private Thread makeReceiver(final Socket sock,
                                final InputStream in) {
        return new Thread() {
            public void run() {
                try {
                    int b;
                    while ((b = in.read()) != -1) {
                        System.out.write(b);
                        System.out.flush();
                    }
                    //in.close();
                    sock.shutdownInput();
                }
                catch (IOException e) {
                    throw new RuntimeException("I/O error occurred", e);
                }
            }};
    }
    static public void main(String[] args) {
        String host = args[0];
        int port = Integer.parseInt(args[1]);
        new ReplClient(host, port).run();
    }
}
