        mov   r0, 10  ; initialize r0
        div   r0, r2  ; divide by the ctx-size, ctx-size >= 8
        div   r2, r0  ; divide ctx-size by the result, might be 0
        exit
