VHDLS = \
  txt_util.vhd \
  stack_mem.vhd \
  j1.vhd \
  uart/ghdl_uart_pkg.vhd \
  uart/ghdl_uart.vhd \
  wb/j1b2wb.vhd \
  i2c_hw/I2C_slave.vhd \
  i2c_hw/i2c_master_bit_ctrl.vhd \
  i2c_hw/i2c_master_byte_ctrl.vhd \
  i2c_hw/i2c_master_top.vhd \
  j1_tb.vhd \

  #wb_switch/wb.vhd \
  #wb_slave/my_regs.vhd \
  
STD=standard
#STD=synopsys

all: simulate

ghdl_pty.o : uart/ghdl_pty.c
	gcc -c uart/ghdl_pty.c
	
simulate: j1_tb
	./j1_tb #--assert-level=warning
j1_tb: ${VHDLS} ghdl_pty.o
	ghdl -a -g  --std=93c  --ieee=${STD} ${VHDLS} 
	ghdl -e -g  -Wl,ghdl_pty.o --std=93c  --ieee=${STD} j1_tb 
clean:
	rm *.o *.vcd *.ghw *.cf j1_tb
	
