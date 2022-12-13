	mov r0, 0
	jle r2, 3, +8
	ldxdw r3, [r1+0]
	ldxdw r4, [r1+1]
	ldxdw r5, [r1+2]
	ldxdw r6, [r1+3]
	add r0, r3
	add r0, r4
	add r0, r5
	add r0, r6
	exit
