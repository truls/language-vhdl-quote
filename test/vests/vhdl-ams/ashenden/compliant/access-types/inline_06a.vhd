
-- Copyright (C) 2002 Morgan Kaufmann Publishers, Inc

-- This file is part of VESTs (Vhdl tESTs).

-- VESTs is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2 of the License, or (at
-- your option) any later version. 

-- VESTs is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
-- for more details. 

-- You should have received a copy of the GNU General Public License
-- along with VESTs; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA 

entity inline_06a is

end entity inline_06a;


----------------------------------------------------------------


architecture test of inline_06a is
begin


  process is

    -- code from book:

    type value_cell is record
        value : real_vector(0 to 3);
        next_cell : value_ptr;
      end record value_cell;

    type value_ptr is access value_cell;

    -- end of code from book

  begin

    wait;
  end process;


end architecture test;
