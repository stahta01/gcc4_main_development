// -*- C++ -*-

// Copyright (C) 2005, 2006 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
// MA 02111-1307, USA.

// As a special exception, you may use this file as part of a free
// software library without restriction.  Specifically, if other files
// instantiate templates or use macros or inline functions from this
// file, or you compile this file and link it with other files to
// produce an executable, this file does not by itself cause the
// resulting executable to be covered by the GNU General Public
// License.  This exception does not however invalidate any other
// reasons why the executable file might be covered by the GNU General
// Public License.

// Copyright (C) 2004 Ami Tavory and Vladimir Dreizin, IBM-HRL.

// Permission to use, copy, modify, sell, and distribute this software
// is hereby granted without fee, provided that the above copyright
// notice appears in all copies, and that both that copyright notice
// and this permission notice appear in supporting documentation. None
// of the above authors, nor IBM Haifa Research Laboratories, make any
// representation about the suitability of this software for any
// purpose. It is provided "as is" without express or implied
// warranty.

/**
 * @file exception.hpp
 * Contains exception classes.
 */

#ifndef PB_DS_EXCEPTION_HPP
#define PB_DS_EXCEPTION_HPP

#include <stdexcept>

namespace pb_ds
{
  // Base class for exceptions.
  struct container_error : public std::logic_error
  {
    container_error() : std::logic_error("data structure exception") { }
  };

  // An entry cannot be inserted into a container object for logical
  // reasons (not, e.g., if memory is unabvailable, in which case
  // the allocator's exception will be thrown).
  struct insert_error : public container_error { };

  // A join cannot be performed logical reasons (i.e., the ranges of
  // the two container objects being joined overlaps.
  struct join_error : public container_error { };

  // A container cannot be resized.
  struct resize_error : public container_error { };
} // namespace pb_ds

#endif
