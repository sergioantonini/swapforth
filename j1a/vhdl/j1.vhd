-- This file implements the orginal J1 Forth CPU written by James Bowman
-- but translated to VHDL by Wojciech M. Zabolotny
-- This file is licensed uder the original J1 license.
-- I have retained the original Verilog vode in comments, to allow
-- verification of the translation
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity j1 is
  generic (
    WIDTH : integer := 16);
  port (
    clk       : in  std_logic;
    code_addr : out unsigned(12 downto 0);
    dout      : out std_logic_vector(WIDTH-1 downto 0);
    insn      : in  std_logic_vector(15 downto 0);
    io_din    : in  std_logic_vector(WIDTH-1 downto 0);
    io_rd     : out std_logic;
    io_wr     : out std_logic;
    mem_addr  : out unsigned(15 downto 0);
    mem_wr    : out std_logic;
    resetq    : in  std_logic
    );
end entity;

architecture beh of j1 is

  function or_reduce (
    constant vec : std_logic_vector)
    return std_logic is
    variable res : std_logic;
  begin  -- function reduce_or
    res := '0';
    for i in vec'low to vec'high loop
      if vec(i) = '1' then
        res := '1';
      end if;
    end loop;  -- i
    return res;
  end function or_reduce;

--  reg [3:0] dsp, dspN;          // data stack pointer
  signal dsp, dspN : unsigned(3 downto 0);
--  reg [`WIDTH-1:0] st0, st0N;   // top of data stack
  signal st0, st0N : std_logic_vector(WIDTH-1 downto 0);
--  reg dstkW;                    // data stack write
  signal dstkW     : std_logic;
--  reg [12:0] pc /* verilator public_flat */, pcN;           // program counter
  signal pc, pcN   : unsigned(12 downto 0);
--  wire [12:0] pc_plus_1 = pc + 13'd1;
  signal pc_plus_1 : unsigned(12 downto 0);
--  reg rstkW;                    // return stack write
  signal rstkW     : std_logic;
--  wire [`WIDTH-1:0] rstkD;      // return stack write value
  signal rstkD     : std_logic_vector(WIDTH-1 downto 0);
--  reg reboot = 1;
  signal reboot    : std_logic := '1';

--  // The D and R stacks
--  wire [`WIDTH-1:0] st1, rst0;
  signal st1, rst0  : std_logic_vector(WIDTH-1 downto 0);
--  reg [1:0] dspI, rspI;
  signal dspI, rspI : std_logic_vector(1 downto 0);



  component stack2 is
    generic (
      WIDTH : integer;
      DEPTH : integer);
    port (
      clk   : in  std_logic;
      delta : in  std_logic_vector(1 downto 0);
      rd    : out std_logic_vector(15 downto 0);
      wd    : in  std_logic_vector(15 downto 0);
      we    : in  std_logic);
  end component stack2;


--  wire [16:0] minus = {1'b1, ~st0} + st1 + 1;
  signal minus      : unsigned(16 downto 0);
--  wire signedless = st0[15] ^ st1[15] ? st1[15] : minus[16];
  signal signedless : std_logic;

--  wire func_T_N =   (insn[6:4] == 1);
  signal func_T_N   : std_logic;
--  wire func_T_R =   (insn[6:4] == 2);
  signal func_T_R   : std_logic;
--  wire func_write = (insn[6:4] == 3);
  signal func_write : std_logic;
--  wire func_iow =   (insn[6:4] == 4);
  signal func_iow   : std_logic;
--  wire func_ior =   (insn[6:4] == 5);
  signal func_ior   : std_logic;

--  wire is_alu = !pc[12] & (insn[15:13] == 3'b011);
  signal is_alu : std_logic;

--endmodule
begin  -- architecture beh

  pc_plus_1  <= pc + 1;
  mem_addr   <= unsigned(st0(15 downto 0));
  code_addr  <= pcN;
  minus      <= unsigned('1' & not st0) + unsigned(st1) + 1;
--  wire signedless = st0[15] ^ st1[15] ? st1[15] : minus[16];
  signedless <= st1(15) when st0(15) /= st1(15) else minus(16);

--  stack2 #(.DEPTH(15)) dstack(.clk(clk), .rd(st1),  .we(dstkW), .wd(st0),   .delta(dspI));
  dstack : stack2
    generic map (
      WIDTH => 16,
      DEPTH => 15)
    port map (
      clk   => clk,
      delta => dspI,
      rd    => st1,
      wd    => st0,
      we    => dstkW);

--  stack2 #(.DEPTH(17)) rstack(.clk(clk), .rd(rst0), .we(rstkW), .wd(rstkD), .delta(rspI));
  rstack : stack2
    generic map (
      WIDTH => 16,
      DEPTH => 17)
    port map (
      clk   => clk,
      delta => rspI,
      rd    => rst0,
      wd    => rstkD,
      we    => rstkW);

  process(dsp, insn, io_din, minus, pc(12), rst0, signedless, st0, st1)
    variable case_sel : std_logic_vector(8 downto 0);
  begin
    --  always @*
    --  begin
    --    // Compute the new value of st0
    --    casez ({pc[12], insn[15:8]})
    case_sel := pc(12) & insn(15 downto 8);
    -- 9'b1_???_?????: st0N = insn;                    // literal
    if std_match(case_sel, "1--------") then
      st0N <= insn;
    --  9'b0_1??_?????: st0N = { {(`WIDTH - 15){1'b0}}, insn[14:0] };    // literal
    elsif std_match(case_sel, "01-------") then
      st0N              <= (others => '0');
      st0N(14 downto 0) <= insn(14 downto 0);
    --  9'b0_000_?????: st0N = st0;                     // jump
    elsif std_match(case_sel, "0000-----") then
      st0N <= st0;
    -- 9'b0_010_?????: st0N = st0;                     // call
    elsif std_match(case_sel, "0010-----") then
      st0N <= st0;
    --      9'b0_001_?????: st0N = st1;                     // conditional jump
    elsif std_match(case_sel, "0001-----") then
      st0N <= st1;
    --      9'b0_011_?0000: st0N = st0;                     // ALU operations...
    elsif std_match(case_sel, "0011-0000") then
      st0N <= st0;
    --      9'b0_011_?0001: st0N = st1;
    elsif std_match(case_sel, "0011-0001") then
      st0N <= st1;
    --      9'b0_011_?0010: st0N = st0 + st1;
    elsif std_match(case_sel, "0011-0010") then
      st0N <= std_logic_vector(unsigned(st0) + unsigned(st1));
    --      9'b0_011_?0011: st0N = st0 & st1;
    elsif std_match(case_sel, "0011-0011") then
      st0N <= st0 and st1;
    --      9'b0_011_?0100: st0N = st0 | st1;
    elsif std_match(case_sel, "0011-0100") then
      st0N <= st0 or st1;
    --      9'b0_011_?0101: st0N = st0 ^ st1;
    elsif std_match(case_sel, "0011-0101") then
      st0N <= st0 xor st1;
    --      9'b0_011_?0110: st0N = ~st0;
    elsif std_match(case_sel, "0011-0110") then
      st0N <= not st0;
    --      9'b0_011_?0111: st0N = {`WIDTH{(minus == 0)}};                //  =
    elsif std_match(case_sel, "0011-0111") then
      if to_integer(minus) = 0 then
        st0N <= (others => '1');
      else
        st0N <= (others => '0');
      end if;
    --      9'b0_011_?1000: st0N = {`WIDTH{(signedless)}};                //  <
    elsif std_match(case_sel, "0011-1000") then
      st0N <= (others => signedless);
    --      9'b0_011_?1001: st0N = {st0[`WIDTH - 1], st0[`WIDTH - 1:1]};
    elsif std_match(case_sel, "0011-1001") then
      st0N <= st0(WIDTH-1) & st0(WIDTH-1 downto 1);
    --      9'b0_011_?1010: st0N = {st0[`WIDTH - 2:0], 1'b0};
    elsif std_match(case_sel, "0011-1010") then
      st0N <= st0(WIDTH-2 downto 0) & '0';
    --      9'b0_011_?1011: st0N = rst0;
    elsif std_match(case_sel, "0011-1011") then
      st0N <= rst0;
    --      9'b0_011_?1100: st0N = minus[15:0];
    elsif std_match(case_sel, "0011-1100") then
      st0N <= std_logic_vector(minus(15 downto 0));
    --      9'b0_011_?1101: st0N = io_din;
    elsif std_match(case_sel, "0011-1101") then
      st0N <= io_din;
    --      9'b0_011_?1110: st0N = {{(`WIDTH - 4){1'b0}}, dsp};
    elsif std_match(case_sel, "0011-1110") then
      st0N             <= (others => '0');
      st0N(3 downto 0) <= std_logic_vector(dsp);
    --      9'b0_011_?1111: st0N = {`WIDTH{(minus[16])}};                 // u<
    elsif std_match(case_sel, "0011-1111") then
      st0N <= (others => minus(16));
    --      default: st0N = {`WIDTH{1'bx}};
    else
      st0N <= (others => '1');
    end if;
  end process;

--  wire func_T_N =   (insn[6:4] == 1);
  func_T_N   <= '1' when insn(6 downto 4) = "001" else '0';
--  wire func_T_R =   (insn[6:4] == 2);
  func_T_R   <= '1' when insn(6 downto 4) = "010" else '0';
--  wire func_write = (insn[6:4] == 3);
  func_write <= '1' when insn(6 downto 4) = "011" else '0';
--  wire func_iow =   (insn[6:4] == 4);
  func_iow   <= '1' when insn(6 downto 4) = "100" else '0';
--  wire func_ior =   (insn[6:4] == 5);
  func_ior   <= '1' when insn(6 downto 4) = "101" else '0';

--  wire is_alu = !pc[12] & (insn[15:13] == 3'b011);
  is_alu <= '1' when (pc(12) = '0') and (insn(15 downto 13) = "011") else '0';
--  assign mem_wr = !reboot & is_alu & func_write;
  mem_wr <= (not reboot) and is_alu and func_write;
  dout   <= st1;
  io_wr  <= (not reboot) and is_alu and func_iow;
  io_rd  <= (not reboot) and is_alu and func_ior;

--  assign rstkD = (insn[13] == 1'b0) ? {{(`WIDTH - 14){1'b0}}, pc_plus_1, 1'b0} : st0;
  process (insn(13), pc_plus_1, st0) is
  begin  -- process
    if insn(13) = '0' then
      rstkD              <= (others => '0');
      rstkD(13 downto 0) <= std_logic_vector(pc_plus_1) & '0';
    else
      rstkD <= st0;
    end if;
  end process;

  process (dsp, dspI, func_T_N, func_T_R, insn, pc(12), pc_plus_1,
           reboot, rst0, st0) is
    variable psel1 : std_logic_vector(3 downto 0);
    variable psel2 : std_logic_vector(3 downto 0);
    variable psel3 : std_logic_vector(6 downto 0);
  begin  -- process
    --  always @*
    --  begin
    --    casez ({pc[12], insn[15:13]})
    psel1 := pc(12) & insn(15 downto 13);
    --    4'b1_???,
    --    4'b0_1??:   {dstkW, dspI} = {1'b1,      2'b01};
    if std_match(psel1, "1---") or std_match(psel1, "01--") then
      dstkW <= '1'; dspI <= "01";
    --    4'b0_001:   {dstkW, dspI} = {1'b0,      2'b11};
    elsif std_match(psel1, "0001") then
      dstkW <= '0'; dspI <= "11";
    --    4'b0_011:   {dstkW, dspI} = {func_T_N,  {insn[1:0]}};
    elsif std_match(psel1, "0011") then
      dstkW <= func_T_N; dspI <= insn(1 downto 0);
    --    default:    {dstkW, dspI} = {1'b0,      2'b00};
    else
      dstkW <= '0'; dspI <= "00";
--    endcase
    end if;

--    dspN = dsp + {dspI[1], dspI[1], dspI};
    dspN <= dsp + unsigned(dspI(1) & dspI(1) & dspI);


--    casez ({pc[12], insn[15:13]})

    psel2 := pc(12) & insn(15 downto 13);
    --    4'b1_???:   {rstkW, rspI} = {1'b0,      2'b11};
    if std_match(psel2, "1---") then
      rstkW <= '0'; rspI <= "11";
    --    4'b0_010:   {rstkW, rspI} = {1'b1,      2'b01};
    elsif std_match(psel2, "0010") then
      rstkW <= '1'; rspI <= "01";
    --    4'b0_011:   {rstkW, rspI} = {func_T_R,  insn[3:2]};
    elsif std_match(psel2, "0011") then
      rstkW <= func_T_R; rspI <= insn(3 downto 2);
    --    default:    {rstkW, rspI} = {1'b0,      2'b00};
    else
      rstkW <= '0'; rspI <= "00";
--    endcase
    end if;


    --    casez ({reboot, pc[12], insn[15:13], insn[7], |st0})
    psel3 := reboot & pc(12) & insn(15 downto 13) & insn(7) & or_reduce(st0);
    --    7'b1_0_???_?_?:   pcN = 0;
    if std_match(psel3, "10-----") then
      --report "PC reset " & integer'image(to_integer(unsigned(insn))) & " pc:" & integer'image(to_integer(pc)) severity note;
      pcN <= (others => '0');
    --    7'b0_0_000_?_?,
    --    7'b0_0_010_?_?,
    --    7'b0_0_001_?_0:   pcN = insn[12:0];
    elsif std_match(psel3, "00000--") or std_match(psel3, "00010--") or std_match(psel3, "00001-0") then
      --report "jump " & integer'image(to_integer(unsigned(insn))) & " pc:" & integer'image(to_integer(pc)) severity note;
      pcN <= unsigned(insn(12 downto 0));
    --    7'b0_1_???_?_?,
    --    7'b0_0_011_1_?:   pcN = rst0[13:1];
    elsif std_match(psel3, "01-----") or std_match(psel3, "000111-") then
      pcN <= unsigned(rst0(13 downto 1));
      --report "PC from rst0 " & integer'image(to_integer(unsigned(insn))) & " pc:" & integer'image(to_integer(pc)) & " psel:" & integer'image(to_integer(unsigned(psel))) severity note;

    --    default:          pcN = pc_plus_1;
    else
      pcN <= pc_plus_1;
--    endcase
    end if;
  end process;
--  end

--  always @(negedge resetq or posedge clk)
--  begin
--    if (!resetq) begin
--      reboot <= 1'b1;
--      { pc, dsp, st0} <= 0;
--    end else begin
--      reboot <= 0;
--      { pc, dsp, st0} <= { pcN, dspN, st0N };
--    end
--  end
  process (clk, resetq) is
  begin  -- process
    if resetq = '0' then                -- asynchronous reset (active low)
      reboot <= '1';
      pc     <= (others => '0');
      dsp    <= (others => '0');
      st0    <= (others => '0');
    elsif clk'event and clk = '1' then  -- rising clock edge
      reboot <= '0';
      pc     <= pcN;
      dsp    <= dspN;
      st0    <= st0N;
    end if;
  end process;

end architecture beh;
