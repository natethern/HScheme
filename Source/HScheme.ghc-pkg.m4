dnl HBase -- general-purpose libraries for Haskell
dnl Copyright (C) 2002 Ashley Yakeley <ashley@semantic.org>
dnl
dnl This library is free software; you can redistribute it and/or
dnl modify it under the terms of the GNU Lesser General Public
dnl License as published by the Free Software Foundation; either
dnl version 2.1 of the License, or (at your option) any later version.
dnl
dnl This library is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl Lesser General Public License for more details.
dnl
dnl You should have received a copy of the GNU Lesser General Public
dnl License along with this library; if not, write to the Free Software
dnl Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
dnl
dnl
Package
	{
	name             = "PACKAGENAME",
	import_dirs      = ["importdir"],
	source_dirs      = [],
	library_dirs     = ["libdir"],
	hs_libraries     = [],
	extra_libraries  = ["LIBNAME"],
	include_dirs     = [],
	c_includes       = [],
	package_deps     = ["HBase"],
	extra_ghc_opts   = [],
	extra_cc_opts    = [],
	extra_frameworks = [],
	extra_ld_opts    = []
	}
