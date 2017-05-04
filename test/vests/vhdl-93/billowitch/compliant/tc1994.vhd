
-- Copyright (C) 2001 Bill Billowitch.

-- Some of the work to develop this test suite was done with Air Force
-- support.  The Air Force and Bill Billowitch assume no
-- responsibilities for this software.

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

-- ---------------------------------------------------------------------
--
-- $Id: tc1994.vhd,v 1.2 2001-10-26 16:29:44 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b02x00p07n01i01994ent IS
END c07s02b02x00p07n01i01994ent;

ARCHITECTURE c07s02b02x00p07n01i01994arch OF c07s02b02x00p07n01i01994ent IS

BEGIN
  TESTING: PROCESS
    type CHAR_RECORD is record
                          C1, C2, C3 : CHARACTER;
                        end record;
    variable k : integer     := 0;
    variable m : CHAR_RECORD := ('a','b','c');
  BEGIN
    if (m /= ('a','b','b')) then
      k := 5;
    else
      k := 3;
    end if;
    assert NOT(k=5) 
      report "***PASSED TEST: c07s02b02x00p07n01i01994" 
      severity NOTE;
    assert (k=5) 
      report "***FAILED TEST: c07s02b02x00p07n01i01994 - Inequality operators are not defined for file types."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b02x00p07n01i01994arch;
