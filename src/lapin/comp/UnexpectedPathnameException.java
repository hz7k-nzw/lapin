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
import lapin.io.FileException;
import lapin.lang.Lists;
import java.io.File;
final class UnexpectedPathnameException extends FileException {
    UnexpectedPathnameException(String name, String ext, String pathname) {
        super("~S must end with ~S: ~S.",
              Lists.list(name, ext, pathname));
    }
    UnexpectedPathnameException(String javapkg) {
        super("failed to make javapkg from pathname. "+
              "pathname contains illegal char: ~S.",
              Lists.list(javapkg));
    }
    UnexpectedPathnameException(File inFile) {
        super("Unexpected inFile: ~S.",
              Lists.list(inFile));
    }
}
