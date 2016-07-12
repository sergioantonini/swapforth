VHDLS = \
  stack2.vhd \
  j1.vhd \
  uart/ghdl_uart_pkg.vhd \
  uart/ghdl_uart.vhd \
  j1_tb.vhd \
  
STD=standard
#STD=synopsys

all: simulate

ghdl_pty.o : uart/ghdl_pty.c
	gcc -c uart/ghdl_pty.c
	
simulate: j1_tb
	./j1_tb 
j1_tb: ${VHDLS} ghdl_pty.o
	ghdl -a -O3 --std=02  --ieee=${STD} ${VHDLS} 
	ghdl -e -O3 -Wl,ghdl_pty.o --std=02  --ieee=${STD} j1_tb 
clean:
	rm *.o *.vcd *.ghw *.cf j1_tb
	
