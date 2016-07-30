create UDres 4 cells allot
\ Unfortunately we have only d< operator, which cant be used in the multiplication
\ Below is the definition of the unsigned double compare
: ud< ( ua0 ua1 ub0 ub1 )
    \ If MSW are equal, check the LSW
    rot ( ua0 ub0 ub1 ua1 )
    over over = if
	drop drop u<
    else
	u> >r
	drop drop r>
    then
;

: ud* ( Da0 Da1 Db0 Db1 -- uq0 uq1 uq2 ug3 ) \ Mutliplication
    ( Da0 Da1 Db0 Db1 )
    0. 2dup UDres 2! UDres 2 cells + 2!
    >r >r ( Da0 Da1) ( R: Db1 Db0)
    swap ( Da1 Da0 ) ( R: Db1 Db0)
    dup ( Da1 Da0 Da0 ) ( R: Db1 Db0)
    r@  ( Da1 Da0 Da0 Db0) ( R: Db1 Db0)
    um* ( Da1 Da0 Da0*Db0 .) ( R: Db1 Db0)
    UDres 2 cells + 2! ( Da1 Da0) ( R: Db1 Db0)
    over ( Da1 Da0 Da1) ( R: Db1 Db0)
    r>   ( Da1 Da0 Da1 Db0) ( R: Db1)
    um* ( Da1 Da0 Da1*Db0 . ) ( R: Db1)
    UDres 1 cells + 2@ ( Da1 Da0 Da1*Db0 . uq1 uq2) ( R: Db1)
    D+
    UDres 1 cells + 2! ( Da1 Da0) ( R: Db1)
    r@ ( Da1 Da0 Db1) ( R: Db1)
    um* ( Da1 Da0*Db1 .) ( R: Db1)
    \ To check for overflow, we have to store the added value
    2>r 2r@ ( Da1 Da0*Db1 .) ( R: Db1 Da0*Db1 .)
    UDres 1 cells + 2@ ( Da1 Da1*Db0 . uq1 uq2) ( R: Db1 Da0*Db1 .)
    D+ ( Da1 uq1 uq2)
    2dup   
    UDres 1 cells + 2! ( Da1 uq1 uq2 ) ( R: Db1 Da0*Db1 .)
    \ Now we can check if the overflow occured. It happened if the result is smaller then one of components
    2r> ( Da1 uq1 uq2 Da0*Db1 .) ( R: Db1 ) 
    ud< if
	\ We have to add one to the uq2 uq3
	UDres @ 1+ UDres !
    then
    r>
    um*
    UDres 2@ D+ UDres 2!
;    

: ud@ UDres 2@ swap UDres 2 cells + 2@ swap ;
hex
: test 2000000030000000. 5000000070000000. ud* ;

: test2 ffffffffffffffff. 2dup ud* ;
\ test
\  2drop 2drop
\ test2
\ decimal
