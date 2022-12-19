        mov r3, r2              ; copy size of ctx
        xor r3, r2              
        jeq r3, 0, +1           ; will always be taken
        div r3, 0               ; direct division by 0
        exit                    ; r0 is not initialized
        mov r0, 0
        exit
