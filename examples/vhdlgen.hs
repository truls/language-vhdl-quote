{-# LANGUAGE QuasiQuotes #-}
-- Example use cases for VHDL quasi-quote generation
module Main where

import           Language.VHDL.Pretty
import           Language.VHDL.Quote
import           Language.VHDL.Syntax

designFileTest :: DesignFile
designFileTest = let
  modName = "System_tb"
  sigNames = [ ("System_Control_data", "i32_t")
             , ("System_Control_readout", "i32_t")
             , ("System_Control_selector", "i32_t")
             , ("System_DataBus_data", "i32_t")
             , ("System_InDataBus_data", "i32_t")
             , ("System_InDataBus_valid", "i32_t")
             ]
  signal (x, y) = [blockdecl|signal $ident:x: $ident:y;|]
  topUnit = "System"
  ports = map fst sigNames
  portMap = map (\x -> [assocel|$ident:x => $ident:x|]) ports
  fieldAssert (x, _) = [seqstms|read_csv_field(L, tmp);
assert are_strings_equal(tmp, $slit:x) report "Field #" & integer'image(fieldno) & " is named: " & truncate(tmp) & $slit:(" but expected " ++ x) severity Failure;
fieldno := fieldno + 1;|]
  valAssert (x, _) = [seqstms|read_csv_field(L, tmp);
if not are_strings_equal(tmp, "U") then
  assert are_strings_equal(int_image($ident:x), tmp) report $slit:("Unexpected value of " ++ x ++ " in cycle ") & integer'image(clockcycle) & ". Actual value was: " & int_image($ident:x) & " but expected " & truncate(tmp) severity Error;
end if;
fieldno := fieldno + 1;|]
    in
    [designfile|library ieee;
               use ieee.std_logic_1164.all;
               use ieee.std_logic_unsigned.all;
               use ieee.numeric_std.all;
               use std.textio.all;
               use ieee.std_logic_textio.all;
               library sme_types;
               use work.sme_types.all;

use work.csv_util.all;
entity $ident:modName is
end $ident:modName;

architecture RTL of $ident:modName is

$blockdecls:(map signal sigNames)

signal clock: std_logic;
signal stop_clock: boolean;
signal reset: std_logic;

begin
uut: entity work.$ident:topUnit
  port map ($assocels:portMap,
--  port map (
            rst => reset,
            clk => clock);

  clk: process
begin
  while not stop_clock loop
    clock <= '1'; -- TODO: test clit antiquote
    wait for 5 ns;
    clock <= '0';
    wait for 5 ns;
  end loop;
  wait;
end process;


  TraceTester: process

file F: TEXT;
variable L: LINE;
variable Status: FILE_OPEN_STATUS;
constant filename : string := "trace.csv";
variable clockcycle : integer := 0;
variable tmp : CSV_LINE_T;
variable readOK : boolean;
variable fieldno : integer := 0;

    -- More decls here
  begin
    file_open(Status, F, filename, READ_MODE);
    if Status /= OPEN_OK then
      report "Unable to open trace file";
    else
      readline(F, L);
      fieldno := 0;
      $seqstms:(concatMap fieldAssert sigNames)

Reset <= '1';
wait for 5 ns;
reset <= '0';

      while not endfile(F) loop
        readline(F, L);
        wait until rising_edge(clock);
        fieldno := 0;
        $seqstms:(concatMap valAssert sigNames)

        clockcycle := clockcycle + 1;
      end loop;
      file_close(F);
    end if;
    report "Completed after " & integer'image(clockcycle) & " clockcycles";
    stop_clock <= true;
    wait;
  end process;


end architecture; |]


main :: IO ()
main =
  putStrLn (pprr designFileTest)
