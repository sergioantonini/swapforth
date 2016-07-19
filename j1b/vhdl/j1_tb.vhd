-------------------------------------------------------------------------------
-- Title      : Testbench for design "j1" and the GHDL simulator
-- Project    : 
-------------------------------------------------------------------------------
-- File       : j1_tb.vhd
-- Author     : Wojciech M. Zabolotny  <wzab01@gmail.com>
-- Company    :
-- License    : BSD License
-- Created    : 2016-07-07
-- Last update: 2016-07-19
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
      io_ready  : in  std_logic;
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
  
  component j1b2wb is
    generic (
      ADRWIDTH  : integer;
      DATAWIDTH : integer);
    port (
      J1B_CLK      : in  std_logic;
      J1B_ARESETN  : in  std_logic;
      J1B_IO_RD    : in  std_logic;
      J1B_IO_WR    : in  std_logic;
      J1B_IO_READY : out std_logic;
      J1B_IO_ADDR  : in  std_logic_vector(15 downto 0);
      J1B_DOUT     : in  std_logic_vector(31 downto 0);
      J1B_DIN      : out std_logic_vector(31 downto 0);
      J1B_WB_DATA  : in  std_logic;
      J1B_WB_REGS  : in  std_logic;
      wb_clk_o     : out std_logic;
      wb_rst_o     : out std_logic;
      wb_addr_o    : out std_logic_vector(31 downto 0);
      wb_dat_o     : out std_logic_vector(31 downto 0);
      wb_we_o      : out std_logic;
      wb_sel_o     : out std_logic_vector(3 downto 0);
      wb_stb_o     : out std_logic;
      wb_cyc_o     : out std_logic;
      wb_dat_i     : in  std_logic_vector(31 downto 0);
      wb_err_i     : in  std_logic;
      wb_ack_i     : in  std_logic);
  end component j1b2wb;

  component wb_test_slave is
    port (
      rst_n_i    : in  std_logic;
      clk_sys_i  : in  std_logic;
      wb_adr_i   : in  std_logic_vector(2 downto 0);
      wb_dat_i   : in  std_logic_vector(31 downto 0);
      wb_dat_o   : out std_logic_vector(31 downto 0);
      wb_cyc_i   : in  std_logic;
      wb_sel_i   : in  std_logic_vector(3 downto 0);
      wb_stb_i   : in  std_logic;
      wb_we_i    : in  std_logic;
      wb_ack_o   : out std_logic;
      wb_stall_o : out std_logic;
      wbt_led_o  : out std_logic_vector(7 downto 0);
      wbt_key_i  : in  std_logic_vector(4 downto 0);
      wbt_sw_i   : in  std_logic_vector(3 downto 0);
      wbt_do1_o  : out std_logic_vector(31 downto 0);
      wbt_do2_o  : out std_logic_vector(31 downto 0);
      wbt_di1_i  : in  std_logic_vector(31 downto 0));
  end component wb_test_slave;
  
-- component ports
  signal uart_rd, uart_wr                 : std_logic;
  signal uart_din, uart_dout              : std_logic_vector(7 downto 0);
  signal uart_dav, uart_ready, uart_empty : std_logic;
  signal code_addr                        : unsigned(12 downto 0);
  signal dout, dout_d                     : std_logic_vector(31 downto 0);
  signal insn                             : std_logic_vector(15 downto 0);
  signal io_din, mem_din                  : std_logic_vector(31 downto 0);
  signal io_ready                         : std_logic;
  signal io_rd, io_rd_d                   : std_logic;
  signal io_wr, io_wr_d                   : std_logic;
  signal mem_addr, mem_addr_d             : unsigned(15 downto 0);
  signal io_addr_d                        : unsigned(15 downto 0);
  signal mem_wr                           : std_logic;
  signal resetq                           : std_logic := '0';

  signal wb_test_dout      : std_logic_vector(31 downto 0);
  signal wb_addr, wb_addr_d, wb_ready : std_logic;

  signal jwb_data, jwb_regs, jwb_ready : std_logic;
  signal jwb_dout : std_logic_vector(31 downto 0);
  
  signal wb_addr_o   : std_logic_vector(31 downto 0);
  signal wb_dat_i   : std_logic_vector(31 downto 0);
  signal wb_dat_o   : std_logic_vector(31 downto 0);
  signal wb_rst_o   : std_logic;
  signal wb_clk_o   : std_logic;
  signal wb_cyc_o   : std_logic;
  signal wb_sel_o   : std_logic_vector(3 downto 0);
  signal wb_stb_o   : std_logic;
  signal wb_we_o   : std_logic;
  signal wb_ack_i   : std_logic;
  signal wb_err_i   : std_logic := '0';
  signal wb_stall_i : std_logic;

  signal wbt_led_o  : std_logic_vector(7 downto 0);
  signal wbt_key_i  : std_logic_vector(4 downto 0) := "11010";
  signal wbt_sw_i   : std_logic_vector(3 downto 0) := "0101";
  signal wbt_do1_o  : std_logic_vector(31 downto 0);
  signal wbt_do2_o  : std_logic_vector(31 downto 0);
  signal wbt_di1_i  : std_logic_vector(31 downto 0);
  
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
    file_close(ramini);
    return vram;
  end function;

  -- Program and data memory
  shared variable ram : T_RAM_PROG := read_ram;
  signal codeaddr     : unsigned(12 downto 0);
  signal ram_data     : std_logic_vector(31 downto 0);
  signal code_sel     : std_logic;
begin  -- architecture test
  codeaddr <= '0' & code_addr(12 downto 1);
  -- Program and data memory
  P2a : process (clk) is
  begin  -- process
    if clk'event and clk = '1' then     -- rising clock edge
      ram_data <= ram(to_integer(unsigned(codeaddr)));
    end if;
  end process;

  P2 : process (clk) is
  begin
    if clk'event and clk = '1' then     -- rising clock edge
      code_sel <= code_addr(0);
    end if;
  end process;

  insn <= ram_data(31 downto 16) when code_sel = '1' else ram_data(15 downto 0);

  P2b : process (clk) is
    variable ram_data : std_logic_vector(31 downto 0);
  begin  -- process
    if clk'event and clk = '1' then     -- rising clock edge
      if mem_wr = '1' then
        ram(to_integer(unsigned(mem_addr(14 downto 2)))) := dout;
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
      wb_addr_d <= wb_addr;
      dout_d  <= dout;
      if io_wr = '1' or io_rd = '1' then
        io_addr_d <= mem_addr;
      end if;
    end if;
  end process;

  uart_wr  <= '1' when io_wr_d='1' and io_addr_d=x"1000" else '0';
  uart_rd  <= '1' when io_rd_d='1' and io_addr_d=x"1000" else '0';
  uart_din <= dout_d(7 downto 0);


  -- Process simulating the slow peripheral
  wb1 : block is
    signal wb_rd, wb_rd_d : std_logic;
    signal del_cnt        : integer;
  begin  -- block wb1
    wb_rd <= '1' when wb_addr = '1' and io_rd = '1' else '0';
    process (clk, resetq) is
      variable v_wb_rdy,v_io_rdy,v_io_rd, v_io_wr : character;
    begin  -- process
      if resetq = '0' then                -- asynchronous reset (active low)
        wb_test_dout <= x"01233210";
        wb_rd_d      <= '0';
        del_cnt      <= 0;
        wb_ready     <= '0';
      elsif clk'event and clk = '1' then  -- rising clock edge
        if wb_addr='1' then
          if io_rd='1' then
            v_io_rd := '1';
          else
            v_io_rd := '0';
          end if;
          if io_wr='1' then
            v_io_wr := '1';
          else
            v_io_wr := '0';
          end if;
          if wb_ready='1' then
            v_wb_rdy := '1';
          else
            v_wb_rdy := '0';
          end if;
          if  io_ready='1' then
            v_io_rdy := '1';
          else
            v_io_rdy := '0';
          end if;
          report "pc=" & integer'image(to_integer(codeaddr)) & " insn=" & integer'image(to_integer(unsigned(insn))) & " wb_ready=" & v_wb_rdy & " io_ready=" & v_io_rdy & " io_wr=" & v_io_wr & " io_rd=" & v_io_rd severity note; 
        end if;
        if wb_rd = '0' then
          del_cnt      <= 0;
          wb_test_dout <= x"23452345";
          wb_ready     <= '0';
        else
          if wb_rd = '1' then
            wb_test_dout <= x"77711777";
            report "wait state: " & integer'image(del_cnt) severity note;
            del_cnt <= del_cnt+1;
            wb_ready     <= '0';
          end if;
          if del_cnt >= 3 then
            report "wait state finished" severity note;
            wb_ready     <= '1';
            wb_test_dout <= x"55667788";
          end if;
        end if;
      end if;
    end process;

  end block wb1;

  jwb_regs <= '1' when mem_addr(15 downto 8)=x"60" else '0';
  jwb_data <= '1' when mem_addr(15 downto 8)=x"61" else '0';
  
  
  j1b2wb_1: j1b2wb
    generic map (
      ADRWIDTH  => 8,
      DATAWIDTH => 32)
    port map (
      J1B_CLK      => clk,
      J1B_ARESETN  => resetq,
      J1B_IO_RD    => io_rd,
      J1B_IO_WR    => io_wr,
      J1B_IO_READY => jwb_ready,
      J1B_IO_ADDR  => std_logic_vector(mem_addr),
      J1B_DOUT     => dout,
      J1B_DIN      => jwb_dout,
      J1B_WB_DATA  => jwb_data,
      J1B_WB_REGS  => jwb_regs,
      wb_clk_o     => wb_clk_o,
      wb_rst_o     => wb_rst_o,
      wb_addr_o    => wb_addr_o,
      wb_dat_o     => wb_dat_o,
      wb_we_o      => wb_we_o,
      wb_sel_o     => wb_sel_o,
      wb_stb_o     => wb_stb_o,
      wb_cyc_o     => wb_cyc_o,
      wb_dat_i     => wb_dat_i,
      wb_err_i     => wb_err_i,
      wb_ack_i     => wb_ack_i);
  
  wb_test_slave_1: wb_test_slave
    port map (
      rst_n_i    => wb_rst_o,
      clk_sys_i  => wb_clk_o,
      wb_adr_i   => wb_addr_o(2 downto 0),
      wb_dat_i   => wb_dat_o,
      wb_dat_o   => wb_dat_i,
      wb_cyc_i   => wb_cyc_o,
      wb_sel_i   => wb_sel_o,
      wb_stb_i   => wb_stb_o,
      wb_we_i    => wb_we_o,
      wb_ack_o   => wb_ack_i,
      wb_stall_o => wb_stall_i,
      wbt_led_o  => wbt_led_o,
      wbt_key_i  => wbt_key_i,
      wbt_sw_i   => wbt_sw_i,
      wbt_do1_o  => wbt_do1_o,
      wbt_do2_o  => wbt_do2_o,
      wbt_di1_i  => wbt_di1_i);

  wbt_di1_i <= std_logic_vector(unsigned(wbt_do1_o)+unsigned(wbt_do2_o));

  
  io_din <= x"000000" & uart_dout when io_addr_d = x"1000" else
            (0      => uart_ready, 1 => uart_dav, others => '0') when io_addr_d = x"2000" else
            wb_test_dout                                         when wb_addr_d = '1' else
            jwb_dout when (jwb_regs='1' or jwb_data='1') else
            (others => '0');
  wb_addr <= '1' when mem_addr(15 downto 12) = x"8" else '0';
  io_ready <= wb_ready when wb_addr = '1' else
              jwb_ready when (jwb_regs = '1' or jwb_data='1') else
              '1';

  -- component instantiation
  DUT : j1
    generic map (
      WIDTH => 32)
    port map (
      clk       => clk,
      resetq    => resetq,
      io_rd     => io_rd,
      io_wr     => io_wr,
      io_ready  => io_ready,
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

  -- Special process used to dump the memory image
  -- It may be used to dump the memory with the compiled words and reuse it
  process (clk) is
    file ram_image  : text;
    variable wrline : line;
    variable mw     : std_logic_vector(31 downto 0);
    variable vc     : character;
  begin  -- process
    if clk'event and clk = '1' then
      if io_rd_d = '1' and io_addr_d = x"2345" then
        file_open(ram_image, "mem_dump.hex", write_mode);
        for i in 0 to 8191 loop
          mw := ram(i);
          for j in 0 to 7 loop
            case mw(31 downto 28) is
              when "0000" => vc := '0';
              when "0001" => vc := '1';
              when "0010" => vc := '2';
              when "0011" => vc := '3';
              when "0100" => vc := '4';
              when "0101" => vc := '5';
              when "0110" => vc := '6';
              when "0111" => vc := '7';
              when "1000" => vc := '8';
              when "1001" => vc := '9';
              when "1010" => vc := 'A';
              when "1011" => vc := 'B';
              when "1100" => vc := 'C';
              when "1101" => vc := 'D';
              when "1110" => vc := 'E';
              when "1111" => vc := 'F';
              when others => vc := 'X';
            end case;
            write(wrline, vc);
            mw := mw(27 downto 0) & "0000";
          end loop;  -- j
          writeline(ram_image, wrline);
        end loop;  -- i
        file_close(ram_image);
      end if;
    end if;
  end process;

end architecture test;


