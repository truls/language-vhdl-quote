{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Language.VHDL.Parser
import           Language.VHDL.Quote
import           Test.Tasty
import           Test.Tasty.HUnit

p f s = f ("", 0, 0) s

exprTest :: Assertion
exprTest = (Right [expr|2 + 2|]) @?= (p parseExpr "2 + 2")

exprTest2 :: Assertion
exprTest2 = let
  n = (2 :: Int)
  m = (4 :: Int)
  in
  [expr|$expr:n + $expr:m|] @?= [expr|2 + 4|]

exprTest3 :: Assertion
exprTest3 = let
  n = (2 :: Int)
  m = ("foo" :: String)
  in
  [expr|$expr:n + $expr:m|] @?= [expr|2 + foo|]

ifTest1 :: Assertion
ifTest1 =
  (let e = [expr|2+2|]
       s = [seqstm|foo := 2;|]
   in ([seqstm|if $expr:e then $seqstm:s end if;|])) @?=
  [seqstm|if 2 + 2 then foo := 2; end if;|]

ifTest2 :: Assertion
ifTest2 =
  (let e = [expr|2+2|]
       s = [seqstm|foo := 2;|]
   in ([seqstm|if $expr:e then $seqstm:s end if;|])) @?=
  [seqstm|if 2 + 2 then foo := 2; end if;|]

ifTest3 :: Assertion
ifTest3 =
  (let stms = [ (\x -> [seqstm|foo := $expr:x;|])
              , (\x -> [seqstm|bar := $expr:x;|])
              , (\x -> [seqstm|baz := $expr:x;|])
              ]
       stmsl = map (\(x,y) -> x y) (zip stms ([1..] :: [Int]))
   in
     ([seqstm|if foo = 3 then first := 0; $seqstms:stmsl end if;|])) @?=
  [seqstm|if foo = 3 then
         first := 0;
         foo := 1;
         bar := 2;
         baz := 3;
         end if;|]

ifTest4 :: Assertion
ifTest4 =
  (let names = map (\x -> [name|$ident:x|]) ["foo", "bar", "baz"]
       stmsl =
         map
           (\(x, y) -> [seqstm|$name:x := $expr:y;|])
           (zip names ([1 ..] :: [Int]))
   in ([seqstm|if foo = 3 then $seqstms:stmsl end if;|])) @?=
  [seqstm|if foo = 3 then
         foo := 1;
         bar := 2;
         baz := 3;
         end if;|]

seqAss :: Assertion
seqAss =
  let n = 2 :: Int
  in [seqstm|foo := $expr:n;|] @?= [seqstm|foo := 2;|]

seqAssHexpr :: Assertion
seqAssHexpr =
  [seqstm|foo := $expr:((2 + 2) :: Int);|] @?= [seqstm|foo := 4;|]

sigAss :: Assertion
sigAss =
  let n = [expr|2ns|]
      w = [wave|4 after $expr:n|]
  in [seqstm|foo <= $wave:w;|] @?= [seqstm|foo <= 4 after 2ns;|]

sigAssExpr :: Assertion
sigAssExpr =
  let n = (2 :: Int)
  in [seqstm|foo <= $expr:n after 4ns;|] @?= [seqstm|foo <= 2 after 4ns;|]

assocList :: Assertion
assocList = let
  unit = "System"
  portMap = map (\x -> [assocel|$ident:x => $ident:x|]) ["foo", "bar"]
  in [constm|uut: entity work.$ident:("System") port map ($assocels:portMap, baz => baz);|] @?=
     [constm|uut: entity work.System port map (foo => foo, bar => bar, baz => baz);|]



qqtests :: TestTree
qqtests =
  testGroup
  "Test statements"
    [ testCase "Parses simple expression correctly" exprTest
    , testCase "Expression with antiquotation" exprTest2
    , testCase "Expression with string var" exprTest3
    , testCase "Parses if antiquotation correctly" ifTest1
    , testCase "Parses second if antiquotation correctly" ifTest2
    , testCase "Can insert several statements at once method 1" ifTest3
    , testCase "Can insert several statements at once method 2" ifTest4
    , testCase "Expression antiquote in assignment" seqAss
    , testCase "Expression antiquote in assignment with haskell expression" seqAssHexpr
    , testCase "Signal assignment with anti waveform" sigAss
    , testCase "Signal assignment with anti expression" sigAssExpr
    , testCase "Association list antiquotation" assocList
    ]

tbAssertStm :: Assertion
tbAssertStm = (let
    x = "System_Control_data"
    in
      [seqstms|read_csv_field(L, tmp);
assert are_strings_equal(tmp, $slit:x) report "Field #" & integer'image(fieldno) & " is named: " & truncate(tmp) & $slit:(" but expected " ++ x) severity Failure;
fieldno := fieldno + 1;|]) @?=
       [seqstms|read_csv_field(L, tmp);
assert are_strings_equal(tmp, "System_Control_data") report "Field #" & integer'image(fieldno) & " is named: " & truncate(tmp) & " but expected System_Control_data" severity Failure;
fieldno := fieldno + 1;|]

tbCheckStm :: Assertion
tbCheckStm = (let
   x = "System_Control_data"
   in
     [seqstms|read_csv_field(L, tmp);
if not are_strings_equal(tmp, "U") then
  assert are_strings_equal(int_image($ident:x), tmp) report $slit:("Unexpected value of " ++ x ++ " in cycle ") & integer'image(clockcycle) & ". Actual value was: " & int_image($ident:x) & " but expected " & truncate(tmp) severity Error;
end if;
fieldno := fieldno + 1;|]) @?=
     [seqstms|read_csv_field(L, tmp);
if not are_strings_equal(tmp, "U") then
  assert are_strings_equal(int_image(System_Control_data), tmp) report "Unexpected value of System_Control_data in cycle " & integer'image(clockcycle) & ". Actual value was: " & int_image(System_Control_data) & " but expected " & truncate(tmp) severity Error;
end if;
fieldno := fieldno + 1;|]


designFileTest :: Assertion
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
    clock <= '1';
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


end architecture;
  |] @?=
      [designfile|library ieee;
                 use ieee.std_logic_1164.all;
                 use ieee.std_logic_unsigned.all;
                 use ieee.numeric_std.all;
                 use std.textio.all;
                 use ieee.std_logic_textio.all;

library sme_types;
use work.sme_types.all;

use work.csv_util.all;
entity System_tb is
end System_tb;

architecture RTL of System_tb is

signal System_Control_data: i32_t;
signal System_Control_readout: i32_t;
signal System_Control_selector: i32_t;
signal System_DataBus_data: i32_t;
signal System_InDataBus_data: i32_t;
signal System_InDataBus_valid: i32_t;
signal clock: std_logic;
signal stop_clock: boolean;
signal reset: std_logic;

begin
   uut: entity work.System
   --port map (
  port map (System_Control_data => System_Control_data,
            System_Control_readout => System_Control_readout,
            System_Control_selector => System_Control_selector,
            System_DataBus_data => System_DataBus_data,
            System_InDataBus_data => System_InDataBus_data,
            System_InDataBus_valid => System_InDataBus_valid,
            rst => reset,
clk => clock);

  clk: process
begin
  while not stop_clock loop
    clock <= '1';
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
      read_csv_field(L, tmp);
assert are_strings_equal(tmp, "System_Control_data") report "Field #" & integer'image(fieldno) & " is named: " & truncate(tmp) & " but expected System_Control_data" severity Failure;
fieldno := fieldno + 1;

      read_csv_field(L, tmp);
assert are_strings_equal(tmp, "System_Control_readout") report "Field #" & integer'image(fieldno) & " is named: " & truncate(tmp) & " but expected System_Control_readout" severity Failure;
fieldno := fieldno + 1;

      read_csv_field(L, tmp);
assert are_strings_equal(tmp, "System_Control_selector") report "Field #" & integer'image(fieldno) & " is named: " & truncate(tmp) & " but expected System_Control_selector" severity Failure;
fieldno := fieldno + 1;

      read_csv_field(L, tmp);
assert are_strings_equal(tmp, "System_DataBus_data") report "Field #" & integer'image(fieldno) & " is named: " & truncate(tmp) & " but expected System_DataBus_data" severity Failure;
fieldno := fieldno + 1;

      read_csv_field(L, tmp);
assert are_strings_equal(tmp, "System_InDataBus_data") report "Field #" & integer'image(fieldno) & " is named: " & truncate(tmp) & " but expected System_InDataBus_data" severity Failure;
fieldno := fieldno + 1;

      read_csv_field(L, tmp);
assert are_strings_equal(tmp, "System_InDataBus_valid") report "Field #" & integer'image(fieldno) & " is named: " & truncate(tmp) & " but expected System_InDataBus_valid" severity Failure;
fieldno := fieldno + 1;


Reset <= '1';
wait for 5 ns;
reset <= '0';

      while not endfile(F) loop
        readline(F, L);
        wait until rising_edge(clock);
        fieldno := 0;
        read_csv_field(L, tmp);
if not are_strings_equal(tmp, "U") then
  assert are_strings_equal(int_image(System_Control_data), tmp) report "Unexpected value of System_Control_data in cycle " & integer'image(clockcycle) & ". Actual value was: " & int_image(System_Control_data) & " but expected " & truncate(tmp) severity Error;
end if;
fieldno := fieldno + 1;

        read_csv_field(L, tmp);
if not are_strings_equal(tmp, "U") then
  assert are_strings_equal(int_image(System_Control_readout), tmp) report "Unexpected value of System_Control_readout in cycle " & integer'image(clockcycle) & ". Actual value was: " & int_image(System_Control_readout) & " but expected " & truncate(tmp) severity Error;
end if;
fieldno := fieldno + 1;

        read_csv_field(L, tmp);
if not are_strings_equal(tmp, "U") then
  assert are_strings_equal(int_image(System_Control_selector), tmp) report "Unexpected value of System_Control_selector in cycle " & integer'image(clockcycle) & ". Actual value was: " & int_image(System_Control_selector) & " but expected " & truncate(tmp) severity Error;
end if;
fieldno := fieldno + 1;

        read_csv_field(L, tmp);
if not are_strings_equal(tmp, "U") then
  assert are_strings_equal(int_image(System_DataBus_data), tmp) report "Unexpected value of System_DataBus_data in cycle " & integer'image(clockcycle) & ". Actual value was: " & int_image(System_DataBus_data) & " but expected " & truncate(tmp) severity Error;
end if;
fieldno := fieldno + 1;

        read_csv_field(L, tmp);
if not are_strings_equal(tmp, "U") then
  assert are_strings_equal(int_image(System_InDataBus_data), tmp) report "Unexpected value of System_InDataBus_data in cycle " & integer'image(clockcycle) & ". Actual value was: " & int_image(System_InDataBus_data) & " but expected " & truncate(tmp) severity Error;
end if;
fieldno := fieldno + 1;

        read_csv_field(L, tmp);
if not are_strings_equal(tmp, "U") then
  assert are_strings_equal(int_image(System_InDataBus_valid), tmp) report "Unexpected value of System_InDataBus_valid in cycle " & integer'image(clockcycle) & ". Actual value was: " & int_image(System_InDataBus_valid) & " but expected " & truncate(tmp) severity Error;
end if;
fieldno := fieldno + 1;

        clockcycle := clockcycle + 1;
      end loop;
      file_close(F);
    end if;
    report "Completed after " & integer'image(clockcycle) & " clockcycles";
    stop_clock <= true;
    wait;
  end process;

end architecture;
|]

-- designUnitTest :: Assertion
-- designUnitTest =

topLvlTests :: TestTree
topLvlTests =
  testGroup
  "Test toplevel Files"
  [ testCase "Full testbench generation example" designFileTest
  , testCase "TestBench assert block test" tbAssertStm
  , testCase "TestBench assert value test" tbCheckStm
  ]

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "QuasiQouter tests" [qqtests, topLvlTests]
