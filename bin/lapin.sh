#!/bin/sh
#
# Copyright (C) 2009 Kenji Nozawa
# This file is part of LAPIN.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
#
dir=`dirname $0`
app_root=`cd $dir/.. && pwd`
#echo $app_root 1>&2

CLASSPATH=".:$app_root/classes"
for jar in $app_root/lib/*.jar
do
    CLASSPATH=$CLASSPATH:$jar
done
#echo $CLASSPATH 1>&2

#JAVA_OPTS="-Dlapin.init.resource.name=/init.lisp"
#echo $JAVA_OPTS 1>&2

rval=-1
case $1 in
    --server|-s)
	# execute REPL server
	shift
	java -cp $CLASSPATH $JAVA_OPTS lapin.tool.ReplServer $*
	rval=$?
	break
	;;
    --client|-c)
	# execute REPL client
	shift
	java -cp $CLASSPATH $JAVA_OPTS lapin.tool.ReplClient $*
	rval=$?
	break
	;;
    *)
	# execute REPL or file loader
	java -cp $CLASSPATH $JAVA_OPTS lapin.tool.Main $*
	rval=$?
	break
	;;
esac

exit $rval
