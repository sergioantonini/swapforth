-- This file is the translation of the stack2.v file written by James Bowman
-- for his J1 Forth CPU.
-- It has been translated to VHDL by Wojciech M. Zabolotny
-- The file is licensed under the original J1 license.
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity stack2 is
  generic (
    WIDTH : integer;
    DEPTH : integer
    );
  port (
    clk : in std_logic;
    delta : in std_logic_vector(1 downto 0);
    rd : out std_logic_vector(WIDTH-1 downto 0);
    wd : in std_logic_vector(WIDTH-1 downto 0);
    we : in std_logic
  );
end entity; 

architecture rtl of stack2 is

  constant BITS : integer := WIDTH * DEPTH - 1;
  signal move : std_logic;
  signal head, headN : std_logic_vector(15 downto 0) := (others => '0');
  signal tail, tailN : std_logic_vector(BITS downto 0) := (others => '0');
  
begin
  rd <= head;
  move <= delta(0);
  headN <= wd when (we = '1') else tail(15 downto 0);
  tailN <= x"55aa" & tail(BITS downto 16) when (delta(1) = '1') else tail(BITS-16 downto 0) & head;

  process (clk) is
  begin
    if (rising_edge(clk)) then
      if (we='1') or (move = '1') then
        head <= headN;
      end if;
      if move = '1' then
        tail <= tailN;
      end if;
    end if;
  end process;
  
end architecture rtl;



