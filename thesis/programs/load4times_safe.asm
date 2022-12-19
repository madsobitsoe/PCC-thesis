        mov   r0, 0             ; initialize r0
        jlt   r2, 32, +8        ; if packet size is less than 32 bytes, jump to exit
        ldxdw r3, [r1+0]        ; load first 8 bytes
        ldxdw r4, [r1+8]        ; load next 8 bytes
        ldxdw r5, [r1+16]       ; load next 8 bytes
        ldxdw r6, [r1+24]       ; load next 8 bytes
        add   r0, r3            ; sum
        add   r0, r4            ; sum
        add   r0, r5            ; sum
        add   r0, r6            ; sum
        exit
