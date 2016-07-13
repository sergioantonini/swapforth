-------------------------------------------------------------------------------
-- Title      : Testbench for design "j1" and the GHDL simulator
-- Project    : 
-------------------------------------------------------------------------------
-- File       : j1_tb.vhd
-- Author     : Wojciech M. Zabolotny  <wzab01@gmail.com>
-- Company    :
-- License    : BSD License
-- Created    : 2016-07-07
-- Last update: 2016-07-13
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-07-07  1.0      wzab    Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
-------------------------------------------------------------------------------

entity j1_tb is

end entity j1_tb;

-------------------------------------------------------------------------------

architecture test of j1_tb is

  component j1 is
    generic (
      WIDTH : integer);
    port (
      clk       : in  std_logic;
      resetq    : in  std_logic;
      io_rd     : out std_logic;
      io_wr     : out std_logic;
      mem_addr  : out unsigned(15 downto 0);
      mem_wr    : out std_logic;
      dout      : out std_logic_vector(WIDTH-1 downto 0);
      mem_din   : in  std_logic_vector(WIDTH-1 downto 0);
      io_din    : in  std_logic_vector(WIDTH-1 downto 0);
      code_addr : out unsigned(12 downto 0);
      insn      : in  std_logic_vector(15 downto 0));
  end component j1;

  component ghdl_uart is
    port (
      data_out : out std_logic_vector(7 downto 0);
      data_in  : in  std_logic_vector(7 downto 0);
      dav      : out std_logic;
      ready    : out std_logic;
      empty    : out std_logic;
      rd       : in  std_logic;
      wr       : in  std_logic);
  end component ghdl_uart;


-- component ports
  signal uart_rd, uart_wr                 : std_logic;
  signal uart_din, uart_dout              : std_logic_vector(7 downto 0);
  signal uart_dav, uart_ready, uart_empty : std_logic;
  signal code_addr                        : unsigned(12 downto 0);
  signal dout, dout_d                     : std_logic_vector(31 downto 0);
  signal insn                             : std_logic_vector(15 downto 0);
  signal io_din, mem_din                  : std_logic_vector(31 downto 0);
  signal io_rd, io_rd_d                   : std_logic;
  signal io_wr, io_wr_d                   : std_logic;
  signal mem_addr, mem_addr_d             : unsigned(15 downto 0);
  signal io_addr, io_addr_d               : unsigned(15 downto 0);
  signal mem_wr                           : std_logic;
  signal resetq                           : std_logic := '0';

  -- clock
  signal Clk : std_logic := '1';
  type T_RAM_PROG is array(0 to 8191) of std_logic_vector(31 downto 0);
  -- Initialization of the memory

  procedure read_hex_stlv (
    variable fline : inout line;
    constant nbits :       integer;
    variable res   : out   std_logic_vector) is

    variable tmp          : std_logic_vector((nbits+3) downto 0) := (others => '0');
    variable c            : character;
    variable npos, nchars : integer;
  begin  -- readhex
    nchars := (nbits+3)/4;              -- number of hex chars to read
    for i in nchars-1 downto 0 loop
      npos := i*4+3;
      read (fline, c);
      case c is
        when '0' =>
          tmp(npos downto npos-3) := "0000";
        when '1' =>
          tmp(npos downto npos-3) := "0001";
        when '2' =>
          tmp(npos downto npos-3) := "0010";
        when '3' =>
          tmp(npos downto npos-3) := "0011";
        when '4' =>
          tmp(npos downto npos-3) := "0100";
        when '5' =>
          tmp(npos downto npos-3) := "0101";
        when '6' =>
          tmp(npos downto npos-3) := "0110";
        when '7' =>
          tmp(npos downto npos-3) := "0111";
        when '8' =>
          tmp(npos downto npos-3) := "1000";
        when '9' =>
          tmp(npos downto npos-3) := "1001";
        when 'a' =>
          tmp(npos downto npos-3) := "1010";
        when 'A' =>
          tmp(npos downto npos-3) := "1010";
        when 'b' =>
          tmp(npos downto npos-3) := "1011";
        when 'B' =>
          tmp(npos downto npos-3) := "1011";
        when 'c' =>
          tmp(npos downto npos-3) := "1100";
        when 'C' =>
          tmp(npos downto npos-3) := "1100";
        when 'd' =>
          tmp(npos downto npos-3) := "1101";
        when 'D' =>
          tmp(npos downto npos-3) := "1101";
        when 'e' =>
          tmp(npos downto npos-3) := "1110";
        when 'E' =>
          tmp(npos downto npos-3) := "1110";
        when 'f' =>
          tmp(npos downto npos-3) := "1111";
        when 'F' =>
          tmp(npos downto npos-3) := "1111";
        when others =>
          assert(false)
            report "Error: wrong separator in the write command" severity error;
      end case;
    end loop;  -- i
    res := tmp((nbits-1) downto 0);
  end read_hex_stlv;

  impure function read_ram
    return T_RAM_PROG is
    file ramini      : text;
    variable line_in : line;
    variable vram    : T_RAM_PROG := (others => (others => '0'));
    variable i, vali : integer    := 0;
    variable valv    : std_logic_vector(31 downto 0);
  begin
    file_open(ramini, "nuc.hex", read_mode);
    while true loop
      if endfile(ramini) then
        exit;
      end if;
      readline(ramini, line_in);
      read_hex_stlv(line_in, 32, valv);
      --vali := to_integer(unsigned(valv));
      --report integer'image(vali) severity note;
      vram(i) := valv;
      i       := i+1;
    end loop;
    report "finished RAM initialization" severity note;
    return vram;
  end function;

  -- Program and data memory
  signal ram      : T_RAM_PROG := read_ram;
  signal codeaddr : unsigned(12 downto 0);

begin  -- architecture test
  codeaddr <= '0' & code_addr(12 downto 1);
  -- Program and data memory
  P2 : process (clk) is
    variable ram_data : std_logic_vector(31 downto 0);
  begin  -- process
    if clk'event and clk = '1' then     -- rising clock edge
      ram_data := ram(to_integer(unsigned(codeaddr)));
      if code_addr(0) = '1' then
        insn <= ram_data(31 downto 16);
      else
        insn <= ram_data(15 downto 0);
      end if;
      if mem_wr = '1' then
        ram(to_integer(unsigned(mem_addr(14 downto 2)))) <= dout;
      end if;
      mem_din <= ram(to_integer(unsigned(mem_addr(14 downto 2))));
    end if;
  end process;

  -- I/O service
  P3 : process(clk) is
  begin
    if clk'event and clk = '1' then     -- rising clock edge
      io_rd_d <= io_rd;
      io_wr_d <= io_wr;
      dout_d  <= dout;
      if io_wr = '1' or io_rd = '1' then
        io_addr_d <= mem_addr;
      end if;
    end if;
  end process;

  uart_wr  <= io_wr_d and io_addr_d(12);
  uart_rd  <= io_rd_d and io_addr_d(12);
  uart_din <= dout_d(7 downto 0);

  io_din <= x"000000" & uart_dout when io_addr_d(12) = '1' else
            (0      => uart_ready, 1 => uart_dav, others => '0') when io_addr_d(13) = '1' else
            (others => '0');

  -- component instantiation
  DUT : j1
    generic map (
      WIDTH => 32)
    port map (
      clk       => clk,
      resetq    => resetq,
      io_rd     => io_rd,
      io_wr     => io_wr,
      mem_wr    => mem_wr,
      dout      => dout,
      mem_din   => mem_din,
      io_din    => io_din,
      mem_addr  => mem_addr,
      code_addr => code_addr,
      insn      => insn
      );

  ghdl_uart_1 : ghdl_uart
    port map (
      data_out => uart_dout,
      data_in  => uart_din,
      dav      => uart_dav,
      ready    => uart_ready,
      empty    => uart_empty,
      rd       => uart_rd,
      wr       => uart_wr);
  -- clock generation

  Clk <= not Clk after 10 ns;

  -- waveform generation
  WaveGen_Proc : process
  begin
    -- insert signal assignments here
    wait until Clk = '1';
    wait for 5 ns;
    resetq <= '1';
    wait;
  end process WaveGen_Proc;



end architecture test;


