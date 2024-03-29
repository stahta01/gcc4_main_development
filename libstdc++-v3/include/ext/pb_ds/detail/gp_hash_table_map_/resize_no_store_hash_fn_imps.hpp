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
 * @file resize_no_store_hash_fn_imps.hpp
 * Contains implementations of gp_ht_map_'s resize related functions, when the
 *    hash value is not stored.
 */

PB_DS_CLASS_T_DEC
inline void
PB_DS_CLASS_C_DEC::
resize_imp_reassign(entry_pointer p_e, entry_array a_entries_resized, 
		    false_type)
{
  const_key_reference r_key = PB_DS_V2F(p_e->m_value);
  size_type hash = ranged_probe_fn_base::operator()(r_key);
  size_type i;
  for (i = 0; i < m_num_e; ++i)
    {
      const size_type pos = ranged_probe_fn_base::operator()(r_key, hash, i);
      entry_pointer p_new_e = a_entries_resized + pos;
      switch(p_new_e->m_stat)
        {
        case empty_entry_status:
	  new (&p_new_e->m_value) value_type(p_e->m_value);
	  p_new_e->m_stat = valid_entry_status;
	  return;
        case erased_entry_status:
	  _GLIBCXX_DEBUG_ASSERT(0);
	  break;
        case valid_entry_status:
	  break;
        default:
	  _GLIBCXX_DEBUG_ASSERT(0);
        };
    }
  throw insert_error();
}

