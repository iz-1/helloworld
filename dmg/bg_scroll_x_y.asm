; -----------------------------------------------------------------------------
; Example: Background scroll (clockwise)
; -----------------------------------------------------------------------------
; Optical Illusion pixeled by Phexion @ http://pixeljoint.com/pixelart/81815.htm
; More examples by tmk @ https://github.com/gitendo/helloworld
; -----------------------------------------------------------------------------

	INCLUDE "hardware.inc"			; system defines

        SECTION "V-Blank",ROM0[$0040]		; vblank interrupt handler
	jp	vblank

	SECTION	"Start",ROM0[$100]		; start vector, followed by header data applied by rgbfix.exe
	nop
	jp	start

        SECTION "Example",ROM0[$150]		; code starts here

start:
	di					; disable interrupts
	ld	sp,$E000			; setup stack

.wait_vbl					; wait for vblank to properly disable lcd
	ld	a,[rLY]	
	cp	$90
	jr	nz,.wait_vbl

	xor	a				; reset important registers
	ld	[rIF],a
	ld	[rLCDC],a
	ld	[rSTAT],a
	ld	[rSCX],a
	ld	[rSCY],a
	ld	[rLYC],a
	ld	[rIE],a

	ld	hl,_RAM                         ; clear ram (fill with a which is 0 here)
	ld	bc,$2000-2			; watch out for stack ;)
	call	fill

	ld	hl,_HRAM			; clear hram
	ld	c,$80				; a = 0, b = 0 here, so let's save a byte and 4 cycles (ld c,$80 - 2/8 vs ld bc,$80 - 3/12)
	call	fill
						; no point in clearing vram, we'll overwrite it with picture data anyway
						; lcdc is already disabled so we have 'easy' access to vram

	ld	hl,picture_chr			; picture data
	ld	de,_VRAM			; place it between $8000-8FFF (tiles are numbered here from 0 to 255)
	ld	bc,3872				; opti.chr file size
	call 	copy

	ld	hl,picture_map			; picture map (256x256px = 32*32) takes whole _SCRN0
	ld	de,_SCRN0			; place it at $9800
	ld	bc,3872				; opti.map file size
	call	copy

	ld	a,%00011011			; bits: 7-6 = 1st color, 5-4 = 2nd, 3-2 = 3rd and 1-0 = 4th color
						; color values: 00 - light, 01 - gray, 10 - dark gray, 11 - dark
	ld	[rBGP],a			; bg palette
	ld	[rOBP0],a			; obj palettes (not used in this example)
	ld	[rOBP1],a

	ld	a,IEF_VBLANK			; vblank interrupt
	ld	[rIE],a				; setup
	
	ld	a,LCDCF_ON | LCDCF_BG8000 | LCDCF_BG9800 | LCDCF_OBJ8 | LCDCF_OBJOFF | LCDCF_WINOFF | LCDCF_BGON
						; lcd setup: tiles at $8000, map at $9800, 8x8 sprites (disabled), no window, etc.
	ld	[rLCDC],a			; enable lcd

	ei					; enable interrupts

.the_end
	call	parse_input			; read joypad and update inputs array that holds individual keys status
	halt					; save battery
;	nop					; nop after halt is mandatory but rgbasm takes care of it :)
	jr	.the_end			; endless loop


vblank:
	ldh	a,[delay]			; fetch delay value, it's 0 after hram initialization
	xor	1				; (0 xor 1 = 1) then (1 xor 1 = 0) - this makes code bellow to be called every second frame
	ldh	[delay],a			; store delay value
	and	a				; check if a = 0
	jr	z,.scroll			; execute scroll part if so
	reti

.scroll
	;ldh	a,[direction]			; first load direction value
.chkRight
	ldh	a,[btn_rt]
	cp	"+"				; move right if it's 0
	jr	nz,.chkDown			; not 'right', check another direction
	ldh	a,[rSCX]			; increase scroll x
	inc	a
	ldh	[rSCX],a
	cp	96				; boundary (256 - 160)
	jr	nz,.r_done			; we haven't reached it yet
	ld	a,1				; boundary reached, change direction to 'down'
	ldh	[direction],a
.r_done
	reti

.chkDown
	ldh	a,[btn_dn]
	cp	"+"				; move down if it's 1
	jr	nz,.chkLeft			; not 'down', check another direction
	ldh	a,[rSCY]			; increase scroll y
	inc	a
	ldh	[rSCY],a
	cp	112				; boundary (256 - 144)
	jr	nz,.d_done			; we haven't reached it yet
	ld	a,2				; boundary reached, change direction to 'left'
	ldh	[direction],a
.d_done
	reti

.chkLeft
	ldh	a,[btn_lt]
	cp	"+"				; move left if it's 2
	jr	nz,.chkUp				; not 'left', check another direction
	ldh	a,[rSCX]			; decrease scroll x
	dec	a
	ldh	[rSCX],a
	and	a				; let's see if we reached starting point = 0
	jr	nz,.l_done                      ; nope
	ld	a,3				; true, change direction to 'up'
	ldh	[direction],a
.l_done

	reti

.chkUp						; no point in checking direction here sinc it's last possibility
	ldh	a,[btn_up]
	cp	"+"
	jr	nz,.u_done
	ldh	a,[rSCY]			; decrease scroll y
	dec	a
	ldh	[rSCY],a
	and	a				; let's see if we reached starting point = 0
	jr	nz,.u_done                      ; nope
	xor	a				; true, change direction to 'right'
	ldh	[direction],a
.u_done
	reti

;-------------------------------------------------------------------------------	
parse_input:
;-------------------------------------------------------------------------------
	
		ld	a,"-"				; button not pressed, you could write $2D instead
		ld	hl,inputs			; 8 byte array that holds individual keys status
		ld	c,8
	.clear
		ld	[hl+],a				; mark all keys as not pressed
		dec	c
		jr	nz,.clear
	
		call	read_keys			; read joypad
	
		ld	a,"+"				; button pressed, you could write $2B instead
		dec	l				; hl points here to next byte after inputs array, move it back to point on btn_a
	.btn_a
		bit	0,b				; is button a pressed ? (bit must be 1)
		jr	z,.btn_b			; no, check other key (apparently it's 0)
		ldh	[btn_a],a			; it is, mark it as +
	.btn_b
		bit	1,b				; ...
		jr	z,.select
		ldh	[btn_b],a
	.select
		bit	2,b
		jr	z,.start
		ldh	[btn_sl],a
	.start
		bit	3,b
		jr	z,.right
		ldh	[btn_st],a
	.right
		bit	4,b
		jr	z,.left
		ldh	[btn_rt],a
	.left
		bit	5,b
		jr	z,.up
		ldh	[btn_lt],a
	.up
		bit	6,b
		jr	z,.down
		ldh	[btn_up],a
	.down
		bit	7,b
		ret	z
		ldh	[btn_dn],a
	
		ret

;-------------------------------------------------------------------------------
read_keys:
;-------------------------------------------------------------------------------
; this function returns two different values in b and c registers:
; b - returns raw state (pressing key triggers given action continuously as long as it's pressed - it does not prevent bouncing)
; c - returns debounced state (pressing key triggers given action only once - key must be released and pressed again)

		ld      a,$20				; read P15 - returns a, b, select, start
		ldh     [rP1],a        
		ldh     a,[rP1]				; mandatory
		ldh     a,[rP1]
	cpl					; rP1 returns not pressed keys as 1 and pressed as 0, invert it to make result more readable
		and     $0f				; lower nibble has a, b, select, start state
	swap	a				
	ld	b,a

		ld      a,$10				; read P14 - returns up, down, left, right
		ldh     [rP1],a
		ldh     a,[rP1]				; mandatory
		ldh     a,[rP1]
		ldh     a,[rP1]
		ldh     a,[rP1]
		ldh     a,[rP1]
		ldh     a,[rP1]
	cpl					; rP1 returns not pressed keys as 1 and pressed as 0, invert it to make result more readable
		and     $0f				; lower nibble has up, down, left, right state
	or	b				; combine P15 and P14 states in one byte
		ld      b,a				; store it

	ldh	a,[previous]			; this is when important part begins, load previous P15 & P14 state
	xor	b				; result will be 0 if it's the same as current read
	and	b				; keep buttons that were pressed during this read only
	ldh	[current],a			; store final result in variable and register
	ld	c,a
	ld	a,b				; current P15 & P14 state will be previous in next read
	ldh	[previous],a

	ld	a,$30				; reset rP1
		ldh     [rP1],a

	ret

;-------------------------------------------------------------------------------	
copy:
;-------------------------------------------------------------------------------
; hl - source address
; de - destination
; bc - size

	inc	b
	inc	c
	jr	.skip
.copy
	ld	a,[hl+]
	ld	[de],a
	inc	de
.skip
	dec	c
	jr	nz,.copy
	dec	b
	jr	nz,.copy
	ret


;-------------------------------------------------------------------------------
fill:
;-------------------------------------------------------------------------------
; a - byte to fill with
; hl - destination address
; bc - size of area to fill

	inc	b
	inc	c
	jr	.skip
.fill
	ld	[hl+],a
.skip
	dec	c
	jr	nz,.fill
	dec	b
	jr	nz,.fill
	ret

;-------------------------------------------------------------------------------

picture_chr:					; bmp2cgb -x -y -z opti.bmp
        INCBIN	"opti.chr"
picture_map:
	INCBIN	"opti.map"


	SECTION	"Variables",HRAM

delay:		DS	1
direction:	DS	1

current:	DS	1			; usually you read keys state and store it into variable for further processing
previous:	DS	1			; this is previous keys state used by debouncing part of read_keys function
inputs:						; array of buttons
btn_dn:		DS	1
btn_up:		DS	1
btn_lt:		DS	1
btn_rt:		DS	1
btn_st:		DS	1
btn_sl:		DS	1
btn_b:		DS	1
btn_a:		DS	1