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
package lapin.lang;

final class Constants {
    static final boolean DEBUG = false;
    static final Integer FIXNUM_CACHE_POW
        = Integer.getInteger("lapin.fixnum.cache.pow", 10);
    static final Integer MULTIPLE_VALUES_LIMIT
        = Integer.getInteger("lapin.multiple.values.limit", 20);
    static final Integer LOG_LEVEL
        = Integer.getInteger("lapin.log.level", 3);
    //static final String INIT_RESOURCE_NAME
    //    = System.getProperty("lapin.init.resource.name", "/init.fasl");
    static final String INIT_RESOURCE_NAME
        = System.getProperty("lapin.init.resource.name", "/init");
    static final String BYTE_CODE_GENERATOR_CLASSNAME
        = System.getProperty("lapin.byte_code_generator.classname",
                             "lapin.comp.bcel.BCELByteCodeGenerator");
    private Constants() {}
}
