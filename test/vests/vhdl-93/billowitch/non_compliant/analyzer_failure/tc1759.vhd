
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
-- $Id: tc1759.vhd,v 1.2 2001-10-26 16:30:12 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s05b01x00p03n01i01759ent IS
  generic (g1: integer := 12);
  port (
    input1: in  bit ;
    input2: in  bit ;
    clk   : in  boolean;
    output: out bit);
END c09s05b01x00p03n01i01759ent;

ARCHITECTURE c09s05b01x00p03n01i01759arch OF c09s05b01x00p03n01i01759ent IS
  signal local    : boolean;
  signal local1    : boolean;
BEGIN
  local1 <=    not (clk)    when local    = true    else
               not (local)    when clk    = false else
               clk       when local    = false else
               local       when clk    = true    else
               not (clk) or not (local) when clk = true; -- Failure_here
  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c09s05b01x00p03n01i01759 - A conditional signal assignment can not end with a condition."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c09s05b01x00p03n01i01759arch;
