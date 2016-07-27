16 base !
6100 constant I2C_REGS 
a base !

: err_halt 
  dup .
  throw
;
: i2c_init
  200 I2C_REGS io!
  0 I2C_REGS 1 + io!
  128 I2C_REGS 2 + io!
;

\ i2c_wr1 writes a single byte 
: i2c_wr1 ( dta addr -- )
  2* I2C_REGS 3 + io! \ set address
  128 16 or I2C_REGS 4 + io! \ CMD: STA+WR
  \ Wait for ACK
  begin 
    I2C_REGS 4 + io@
    dup 2 and while
    drop 
  repeat
  128 and if
     \ NACK in address
     133 err_halt then
  I2C_REGS 3 + io!
  64 16 or
  I2C_REGS 4 + io!
  begin 
    I2C_REGS 4 + io@
    dup 2 and while
    drop 
  repeat
  128 and if
     \ NACK in data
     134 err_halt then
;    

: i2c_rd1 ( addr -- data )
  2* 1+ I2C_REGS 3 + io! \ set address
  128 16 or I2C_REGS 4 + io! \ CMD: STA+WR
  begin 
    I2C_REGS 4 + io@
    dup 2 and while
    drop
  repeat
  128 and if
     \ NACK in address
     133 err_halt then
  64 32 or 8 or I2C_REGS 4 + io!
  begin 
    I2C_REGS 4 + io@
    dup 2 and while
    drop
  repeat
  drop
  I2C_REGS 3 + io@  
;
    
\ i2c_wr writes multiple bytes stored in a byte array.
\ The first byte contains the length of the array.
\ Next bytes contain the data to be sent

: i2c_wr ( dtaptr addr -- )
  2* I2C_REGS 3 + io! \ set address
  128 16 or I2C_REGS 4 + io! \ CMD: STA+WR
  \ Wait for ACK
  begin 
    I2C_REGS 4 + io@
    dup 2 and while
    drop 
  repeat
  128 and if
     \ NACK in address
      133 err_halt then
  \ Read the length of the data
  dup c@ \ dtaptr len --
  \ Now we transfer data in the loop
  begin
    swap 1+ swap \ increase dtaptr
    dup while
    over c@	  
    I2C_REGS 3 + io!
    1- dup if 16 else 64 16 or then 
    I2C_REGS 4 + io!
    begin 
      I2C_REGS 4 + io@
      dup 2 and while
      drop 
    repeat
    128 and if
      \ NACK in data
       134 err_halt then
  repeat
  2drop  
;  

     
