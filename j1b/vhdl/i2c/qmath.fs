\ The code below is written by Wojciech M. Zabolotny
\ ( wzab01<at>gmail.com or wzab<at>ise.pw.edu.pl )
\ It is available as PUBLIC DOMAIN or under Creative Commons CC0 License
\
\ In routines below I mark double cell opperands as x0 (LSW) x1 (MSW)
\ The quadruple cell values are marked as x0 x1 x2 x3
\ In case of calculated double cell results, they are described as (e.g. for a0*b1):
\ a0*b1 (MSW) . (LSW)
\ All values are unsigned

4 cells buffer: UDres

\ Find the value of the most significant bit in the cell
-1 dup 1 rshift xor

constant cell_msb
\ Unfortunately we have only d< operator, which cant be used in the multiplication
\ Below is the definition of the unsigned double compare
: ud< ( a0 a1 b0 b1 -- flag )
    \ If MSW are equal, check the LSW
    rot ( a0 b0 b1 a1 )
    over over = if
        \ MSWs are equal, so compare LSWs
	drop drop u<
    else
        \ MSWs are not equal, so their comparison produces the result
	u> >r
	drop drop r>
    then
;

\ Procedure below compares multiple precision buffers
: un< ( adr1 adr2 n )
    0 swap ( adr1 adr2 flag n ) \ Added 0 on stack to improve loop organization
    0 do
	( adr1 adr2 flag)
	drop ( adr1 adr2 )
	over @ over @ u< dup
	if
	    leave
	then
	( adr1 adr2 flag)
	rot 1 cells +
	rot 1 cells +
	rot
	\ Addresses are updated
    loop
    -rot 2drop \ Drop addresses
;
\ Procedure below subtracts multiple precision buffer pointed by adr2 from buffer pointed by adr1
\ To avoid problems with lack of borrow bit, we use double precision operations
: un- ( adr1 adr2 n )
    \ Adjust buffers to point to LSW
    .s cr
    swap over 1- cells + ( adr1 n adr2[LSW])
    -rot swap over 1- cells + ( adr2[LSW] n adr1[LSW])
    -rot ( adr1[LSW] adr2[LSW] n)
    .s cr
    1 swap ( adr1[LSW] adr2[LSW] borrow_flag n)
    0 do
	( adr1 adr2 borrow_flag)
	.s cr
	over @ -1 xor s>d ( adr1 adr2 borrow_flag -a1 .)
	rot 0 ( adr1 adr2 a1 . borrow_flag .)
	.s cr
	D+ ( adr1 adr2 res_a . )
	.s cr
	3 pick @ 0 D+ ( adr1 adr2 res_b \ which will be treated as ( adr1 adr2 res borrow_flag )
	.s cr
	2dup u. u. cr
	.s cr
	swap ( adr1 adr2 borrow_flag res )
	3 pick !
	\ Adjust addresses
	rot 1 cells - rot 1 cells - rot
	1+ \ adjust borrow flag
    loop
    drop 2drop
;
\ The word below shifts right by 1 bit the multiple precision buffer pointed by addr
: n2/ ( addr n )
    0 ( addr n 0) \ Simulate MSB from the previous word for first cell 
    -rot ( 0 addr n)
    0 do
	( prev addr )
	dup @ dup ( prev addr cur cur)
	>r ( prev addr cur) ( R: cur)
	1 rshift ( prev addr cur/2 ) ( R: cur)
	rot or ( addr cur/2|prev ) ( R: cur) \ Or with MSB from the LSB of the previous word
	over ! ( addr ) ( R: cur)
	cell+  ( addr ) ( R: cur) \ Adjust the address
	r> ( addr cur )
	\ Now transfer the LSB from the previous word to the MSB of the current word
	1 and negate dup 1 rshift xor
	swap
    loop
;

create x1 10 , 32 , 5 ,
create x2 10 , 33 , 5 ,
create x3 10 , 32 , 6 ,
create x4 11 , 32 , 5 ,



: ud* ( a0 a1 b0 b1 -- ) \ Result is placed in UDres in order q3 q2 q1 q0
    ( a0 a1 b0 b1 )
    0. 2dup UDres 2! UDres 2 cells + 2!
    >r >r ( a0 a1) ( R: b1 b0)
    swap ( a1 a0 ) ( R: b1 b0)
    dup ( a1 a0 a0 ) ( R: b1 b0)
    r@  ( a1 a0 a0 b0) ( R: b1 b0)
    um* ( a1 a0 a0*b0 .) ( R: b1 b0)
    UDres 2 cells + 2! ( a1 a0) ( R: b1 b0)
    over ( a1 a0 a1) ( R: b1 b0)
    r>   ( a1 a0 a1 b0) ( R: b1)
    um* ( a1 a0 a1*b0 . ) ( R: b1)
    UDres 1 cells + 2@ ( a1 a0 a1*b0 . q1 q2) ( R: b1)
    D+
    UDres 1 cells + 2! ( a1 a0) ( R: b1)
    r@ ( a1 a0 b1) ( R: b1)
    um* ( a1 a0*b1 .) ( R: b1)
    \ To check for overflow, we have to store the addend
    2>r 2r@ ( a1 a0*b1 .) ( R: b1 a0*b1 .)
    UDres 1 cells + 2@ ( a1 a1*b0 . q1 q2) ( R: b1 a0*b1 .)
    D+ ( a1 q1 q2) ( R: b1 a0*b1 .)
    2dup   ( a1 q1 q2 q1 q2) ( R: b1 a0*b1 .)
    UDres 1 cells + 2! ( a1 q1 q2 ) ( R: b1 a0*b1 .)
    \ Now we can check if the overflow occured. It happened if the result is smaller then the addend
    2r> ( Da1 uq1 uq2 Da0*Db1 .) ( R: Db1 ) 
    ud< if
	\ We have to add one to the q2 q3
	UDres @ 1+ UDres !
    then
    r>
    um*
    UDres 2@ D+ UDres 2!
;    

4 cells buffer: UDsub \ This buffer will be used to store the value subtracted from UDres

: ud/ ( a0 a1 -- b0 b1 ) \ b=UDres(q3 q2 q1 q0)/a 
    \ We can perform division only if q3 q2 is less than a0 a1,
    \ otherwise result won't fit in double precision result
    2dup UDres 2@ ud< if
	UDsub 2! ( -- )
	0. UDsub 1 cells + 2!
	\ Now we can start the main division loop
	\ We compare UDres with UDsub if UDres>UDsub, we set 1 in result and subtract UDsub from UDres
	32 0 do
	loop
    else
	." Overflow"
	136 throw
    then
    
;

: ud@ UDres 2@ swap UDres 2 cells + 2@ swap ;
hex
: test 2000000030000000. 5000000070000000. ud* ;

: test2 ffffffffffffffff. 2dup ud* ;
\ test
\  2drop 2drop
\ test2
\ decimal

\ Test the un- procedure
\ 0123456789abcdef  0123456789abcdef
  2000000030000000. 5400023303533333. UDres 2 cells + 2! UDres 2!
\ 0123456789abcdef  0123456789abcdef
  1000000031000000. 5400023303353333. UDsub 2 cells + 2! UDsub 2!

UDres UDsub 4 un-
UDres dup @ u. cell+ dup @ u. cell+ dup @ u. cell+ @ u.
cr
UDsub dup @ u. cell+ dup @ u. cell+ dup @ u. cell+ @ u.

: .UDsub ." UDsub: " UDsub dup @ u. cell+ dup @ u. cell+ dup @ u. cell+ @ u. ;


