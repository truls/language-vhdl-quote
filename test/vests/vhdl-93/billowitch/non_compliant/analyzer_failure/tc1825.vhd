
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
-- $Id: tc1825.vhd,v 1.2 2001-10-26 16:30:13 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s01b00x00p08n01i01825ent IS
  type small_int is range 0 to 7;
END c07s01b00x00p08n01i01825ent;

ARCHITECTURE c07s01b00x00p08n01i01825arch OF c07s01b00x00p08n01i01825ent IS
  signal s_int : small_int := 0;
BEGIN
  TESTING : PROCESS
  BEGIN
    wait for 5 ns;
    assert s_int > ch0701_p00801_16_ent              -- entity name illegal here
      report "Entity name accepted as primary in an expression."
      severity note ;
    assert FALSE
      report "***FAILED TEST: c07s01b00x00p08n01i01825 - Entity name are not permitted as primaries in an assert condition."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s01b00x00p08n01i01825arch;
