[map all lites.map]

%include "WonderSwan.inc"

MAP_FG equ WSC_TILE_BANK1 - MAP_SIZE
MAP_BG equ MAP_FG - MAP_SIZE
SPRITE_TABLE equ MAP_BG - SPR_TABLE_SIZE

NUMBER_TILES equ 64

LITEW equ 3
LITEH equ 3

STATE_GAME equ 1

; ram
section .bss

	interrupts: resb 0x400
	
	vblankdone: resb 1
	tic: resb 1
	
	lites: resb 5 * 5
	
	movecount: resw 1
	
	px: resb 1
	py: resb 1

	last_dpad: resw 1
	last_buttons: resw 1
	
	flip_x: resb 1
	flip_y: resb 1
	flip_w: resb 1
	flip_h: resb 1
	
	timertics: resb 1
	timer: resw 1
	
	state: resw 1

; code	
section .code

	org 0x0000
	cpu 186 
	bits 16

initialize:

	; disable interrupts on the cpu
	cli
	
	; disable wonderswan interrupts
	mov al, 0
	out IO_INT_ENABLE, al
	
	; direction flag forward
	cld

	mov ax, 0x0000
	; data segment 0
	mov ds, ax
	; extra segment 0
	mov es, ax
	
	; setup stack
	mov ax, 0x0000
	mov bp, ax
	mov ss, ax
	mov sp, WSC_STACK

	; clear Ram
	mov ax, 0x0000
	mov di, 0x0100
	mov cx, 0x4000 - 2
	rep stosw

	mov al, 0
	out IO_SRAM_BANK, al 

	; set video mode
	in al, IO_VIDEO_MODE
	or al, VMODE_16C_CHK | VMODE_CLEANINIT
	out IO_VIDEO_MODE, al
	
	; set sprite table location
	mov al, SPR_TABLE(SPRITE_TABLE)
	out IO_SPR_TABLE, al
	
	; map data locations
	mov ax, BG_MAP(MAP_BG) | FG_MAP(MAP_FG)
	out IO_FGBG_MAP, ax
	
	; display modes
	mov al, BG_ON | SPR_ON
	out IO_DISPLAY_CTRL, al
	
	; switch on display
	in al, IO_LCD_CTRL
	or al, LCD_ON
	out IO_LCD_CTRL, al
	
	; data segment = rom
	mov ax, 0xf000
	mov ds, ax
	
	; set tile palettes
	mov si, tile_palette
	mov di, WSC_PALETTES
	mov cx, 16
	rep movsw
	
	; set sprite palette
	mov si, sprite_palette
	mov di, WSC_PALETTES + (32 * 8)
	mov cx, 16
	rep movsw

	; copy tile graphics into memory
	mov si, tiles
	mov di, WSC_TILE_BANK1
	mov cx, (16 * $100)
	rep movsw
	
	; clear sprites
	mov ax, 0xffff
	mov cx, 128 * 2
	mov di, SPRITE_TABLE
	rep stosw

	; enable sprites
	mov al, 0
	out IO_SPR_START, al
	
	; 4 for cursor + 25 for avatar's lites
	mov al, 0x7f
	out IO_SPR_STOP, al

	; cursor pointer
	; hardcoded as sprites 0-3 for now
	mov word [es:SPRITE_TABLE], 0x0002
	mov word [es:SPRITE_TABLE + 2], 0x0000
	
	mov word [es:SPRITE_TABLE + 4], 0x0003
	mov word [es:SPRITE_TABLE + 6], 0x0800
	
	mov word [es:SPRITE_TABLE + 8], 0x0004
	mov word [es:SPRITE_TABLE + 10], 0x0008
	
	mov word [es:SPRITE_TABLE + 12], 0x0005
	mov word [es:SPRITE_TABLE + 14], 0x0808
	
	; data segment = ram
	mov ax, 0x0000
	mov ds, ax
		
	; interrupts are 4 bytes (2 words) long and are stored in the first 0x100 bytes of ram
	; the first word is the address of the interrupt code
	; the second word is the segment the interrupt code is in
	
	; which interrupt we start counting from for the wonderswan interrupts
	; e.g. if INT_BASE is 32 then all of the wonderswan interrupts are at interrupts 32 to 39
	mov ax, INT_BASE
	out IO_INT_BASE, ax
	
	; set up vblank interrupt in interrupt vector table
	; get the interrupt number
	add ax, INTVEC_VBLANK_START
	; multiply it by 4 to get the address of the interrupt in the table
	shl ax, 2
	mov di, ax
	; put the interrupt location and segment in the table
	mov word [es:di], vblank_interrupt
	add di, 2
	mov word [es:di], 0xf000
	
	; acknowledge all interrupts
	mov al, 0xff
	out IO_INT_ACK, al
	
	; clear HBL & Timer
	mov ax, 0
	out IOw_HBLANK_FREQ, ax
	out IO_TIMER_CTRL, al
	
	; init values
	mov al, 0
	mov [es:vblankdone], al
	
	mov ax, 0
	mov [es:last_dpad], ax
	mov [es:last_buttons], ax
	
	mov [es:timertics], al
	mov [es:timer], ax
	
	mov [es:movecount], ax	
	
	mov ax, STATE_GAME
	mov [es:state], ax
	
	; draw screen
	call init_map
	call draw_movecount
	call draw_timer
	
	call init_avatar_lites

	; enable vblank interrupt
	mov al, INT_VBLANK_START 
	out IO_INT_ENABLE, al

	; enable interrupts on the cpu
	sti

main_loop:

	hlt
	
	; wait for vblankdone == 1
	mov al, [es:vblankdone]
	or al, al
	jz main_loop
	
	; clear vblankdone flag
	mov al, 0
	mov [es:vblankdone], al
	
	; update tic counter
	inc byte [es:tic]
	
	; update timer tic counter
	inc byte [es:timertics]
	mov al, [es:timertics]
	
	; check if a second has elapsed
	cmp al, 75
	jnz timertic_done
	
	; reset timer
	mov al, 0
	mov [es:timertics], al
	
	; get timer
	mov bx, [es:timer]
	
	; increment seconds
	mov al, bl
	inc al
	daa	
	mov bl, al
	
	; check if a minute has elapsed
	cmp al, 0x60
	jnz timer_minute_not_elapsed
	
	; increment minutes
	mov bl, 0
	mov al, bh
	inc al
	daa
	mov bh, al	
	
timer_minute_not_elapsed:

	;update value in memory
	mov [es:timer], bx
	
	call draw_timer
	
timertic_done:

	; poll dpad
	mov al, KEYPAD_READ_ARROWS_H
	out IO_KEYPAD, al
	nop
	nop
	nop
	
	; get dpad
	in ax, IO_KEYPAD
	
	; get last frame's dpad state
	mov bx, [es:last_dpad]
	
	; update last dpad state
	mov [es:last_dpad], ax
	
	; this makes ax contain dpad buttons which 
	; were 0 last frame and 1 this frame
	xor ax, bx,
	and ax, [es:last_dpad]
	
joy_up:
	test ax, PAD_UP
	jz joy_down
	
	dec byte [es:py]

joy_down:
	test ax, PAD_DOWN
	jz joy_left
	
	inc byte [es:py]
	
joy_left:
	test ax, PAD_LEFT
	jz joy_right
	
	dec byte [es:px]
	
joy_right:
	test ax, PAD_RIGHT
	jz joy_dir_done
	
	inc byte [es:px]

joy_dir_done:

	; keep x coords in range 0-4
	mov al, [es:px]
	
joy_xtest1:
	cmp al, 0x5
	jnz joy_xtest2
	
	mov al, 0x0
	
joy_xtest2:
	cmp al, 0xff
	jnz joy_xtest_done
	mov al, 0x4

joy_xtest_done:
	
	; update px 
	mov [es:px], al

	; keep y coords in range 0-4
	mov al, [es:py]
	
joy_ytest1:
	cmp al, 0x5
	jnz joy_ytest2
	
	mov al, 0x0
	
joy_ytest2:
	cmp al, 0xff
	jnz joy_ytest_done
	
	mov al, 0x4
	
joy_ytest_done:	

	; update py
	mov [es:py], al

	; update cursor position
	call update_cursor

	; poll buttons
	mov al, KEYPAD_READ_BUTTONS
	out IO_KEYPAD, al
	nop
	nop
	nop
	
	; get dpad
	in ax, IO_KEYPAD
	
	mov bx, [es:last_buttons]
	mov [es:last_buttons], ax
	
	; this makes ax contain buttons which 
	; were 0 last frame and 1 this frame
	xor ax, bx
	and ax, [es:last_buttons]

joy_a:

	test ax, PAD_A
	jz joy_done

	; increase move counter, stored as bcd
	mov ax, [es:movecount]
	
	; increment and bcd correct first two digits, store in bl
	inc al
	daa
	mov bl, al
	
	; increment and bcd correct second two digits, store in bh
	mov al, ah
	adc al, 0
	daa
	mov bh, al
	
	; update value in ram
	mov [es:movecount], bx
	
	call draw_movecount
	
	; flip tiles under cursor
	; get width and start x of area to flip
	mov al, [es:px]
	
	; cursor in first column
flip_check_col_0:
	cmp al, 0
	jnz flip_check_col_4
	
	mov byte [es:flip_x], 0
	mov byte [es:flip_w], 2	
	jmp flip_check_col_done
	
	; cursor in last column
flip_check_col_4:
	cmp al, 4
	jnz flip_check_col
	
	mov byte [es:flip_x], 3
	mov byte [es:flip_w], 2
	jmp flip_check_col_done
	
	; cursor in middle columns
flip_check_col:

	dec al
	mov byte [es:flip_x], al
	mov byte [es:flip_w], 3
	jmp flip_check_col_done

flip_check_col_done:
	
	; get height and start y of area to flip
	mov al, [es:py]

	; cursor in first column
flip_check_row_0:

	cmp al, 0
	jnz flip_check_row_4
	
	mov byte [es:flip_y], 0
	mov byte [es:flip_h], 2
	jmp flip_check_row_done
	
	; cursor in last column
flip_check_row_4:

	cmp al, 4
	jnz flip_check_row
	
	mov byte [es:flip_y], 3
	mov byte [es:flip_h], 2
	jmp flip_check_row_done
	
	; cursor in middle columns
flip_check_row:

	dec al
	mov byte [es:flip_y], al
	mov byte [es:flip_h], 3
	jmp flip_check_row_done

flip_check_row_done:

	; put number of lites we'll be flipping in cx
	mov ax, 0
	mov cx, [es:flip_h]
	
	; multiply flip_w by flip_h
flip_lite_count:

	add al, [es:flip_w]
	dec cx
	jnz flip_lite_count
	
	mov cx, ax

	; put start address of lites to flip in di
	mov di, lites
	
	mov ax, 0
	mov al, [es:flip_x]
	add di, ax
	
	mov al, [es:flip_y]
	shl al, 2
	add al, [es:flip_y]
	add di, ax
	
	; offset of lite we're flipping
	mov bx, 0
	
flip_lite_y:

	; how far along in the row we are
	mov dx, 0
	
flip_lite_x:

	mov al, [es:bx+di]
	not al
	and al, 0x1
	mov [es:bx+di], al
	
	; flipped all lites to flip, we're done
	dec cx
	jz flip_lite_done
	
	; move to next cell
	inc bx

	; check if we've reached the end of the row
	inc dx
	cmp dl, [es:flip_w]
	jnz flip_lite_x
	
	; not done, so move to next row
	add bx, 5
	mov ax, 0
	mov al, [es:flip_w]
	sub bx, ax
	jmp flip_lite_y

	
flip_lite_done:

	call draw_lites
	call update_avatar_lites

joy_done:

	jmp main_loop
	
vblank_interrupt:
	push ax
	push di
	
	mov al, 1
	mov [es:vblankdone], al
	
	;acknowledge vblank
	in al, IO_INT_ACK
	or al, INT_VBLANK_START
	out IO_INT_ACK, al
	
	pop di
	pop ax
	iret
	
init_map:

	mov ax, 0
	mov byte [es:px], al
	mov byte [es:py], al

	; set lites to off
	mov ax, 0
	mov cx, 5 * 5
	mov di, lites
	rep stosb
	
	mov ax, 0xf000
	mov ds, ax
	
	; draw background
	mov si, litemap
	mov di, MAP_BG
	mov cx, (32 * 18)
	rep movsw
	
	mov ax, 0x0000
	mov ds, ax
	
	call draw_lites
	
	ret

draw_lites:
	
	mov cx, 5
draw_lites_y:
		
	mov dx, 5
draw_lites_x:

	; get address of lite in di (lites + (((dx-1) + ((cx-1) * 5)))
	mov di, lites
	
	mov ax, dx
	dec ax
	add di, ax
	
	mov bx, cx
	dec bx
	mov ax, bx
	shl ax, 2
	add ax, bx
	add di, ax
	
	; get state of lite at that address
	mov bl, [es:di]
	; get start of tile data for this lite state
	shl bl, 4
	add bl, 16 * 5
	
	; get x coordinate of lite tile (((dx-1) * 3) + 1)
	mov di, dx
	dec di
	add di, dx
	add di, dx
	inc di
	
	; get y coordinate of lite tile ((((cx-1) * 3) + 1) * 32) 
	mov si, cx
	dec si
	add si, cx
	add si, cx
	inc si
	shl si, 5
	
	; combine x and y coordinates and move pointer to start of tile
	add di, si
	sub di, 2 + (32 * 2)
	
	; each tile is 2 bytes long
	shl di, 1
	
	; get pointer into bg for this tile
	add di, MAP_BG 
	
	; copy tiles
	; top row
	mov [es:di], bl
	add di, 2
	inc bl
	mov [es:di], bl
	add di, 2
	inc bl
	mov [es:di], bl
	
	; middle row
	add di, (32 - 2) * 2
	inc bl
	mov [es:di], bl
	add di, 2
	inc bl
	mov [es:di], bl
	add di, 2
	inc bl
	mov [es:di], bl
	
	; bottom row
	add di, (32 - 2) * 2
	inc bl
	mov [es:di], bl
	add di, 2
	inc bl
	mov [es:di], bl
	add di, 2
	inc bl
	mov [es:di], bl
	
	dec dx
	jnz draw_lites_x
	
	dec cx
	jnz draw_lites_y
	
	ret

draw_movecount:

	; draw count
	; point di at 24,4 in tilemap
	mov di, MAP_BG + ((24 + (32 * 4)) * 2)
	
	mov bx, [es:movecount]

	; 1s digit
	mov al, bl
	and al, 0xf
	add al, NUMBER_TILES 
	mov [es:di], al
	
	; 10s digit
	sub di, 2
	mov al, bl
	shr al, 4
	and al, 0xf
	add al, NUMBER_TILES 
	mov [es:di], al
	
	; 100s digit
	sub di, 2
	mov al, bh
	and al, 0xf
	add al, NUMBER_TILES 
	mov [es:di], al
	
	; 10s digit
	sub di, 2
	mov al, bh
	shr al, 4
	and al, 0xf
	add al, NUMBER_TILES 
	mov [es:di], al

	ret

draw_timer:

	; draw count
	; point di at 24,2 in tilemap
	mov di, MAP_BG + ((24 + (32 * 2)) * 2)
	
	mov bx, [es:timer]

	; 1s digit
	mov al, bl
	and al, 0xf
	add al, NUMBER_TILES
	mov [es:di], al
	
	; 10s digit
	sub di, 2
	mov al, bl
	shr al, 4
	and al, 0xf
	add al, NUMBER_TILES 
	mov [es:di], al
	
	; 100s digit
	sub di, 4
	mov al, bh
	and al, 0xf
	add al, NUMBER_TILES 
	mov [es:di], al
	
	; 10s digit
	sub di, 2
	mov al, bh
	shr al, 4
	and al, 0xf
	add al, NUMBER_TILES 
	mov [es:di], al
	
	ret
	
update_cursor:

	mov di, SPRITE_TABLE + 2
	mov si, cursor_offsets
	
	mov ah, [es:px]
	mov al, [es:py]	
	
	; we want to do x = (px * 24) + 20 and y = (py * 24) + 16
	shl ax, 3
	mov bx, ax
	add ax, bx
	add ax, bx
	add ax, 0x1410
	
	; keep initial coords for later
	mov bx, ax

	mov cx, 4
update_cursor_loop:
	
	mov ax, bx
	add ax, [cs:si]
	mov [es:di], ax
	
	add di, 4
	add si, 2
	
	dec cx
	jnz update_cursor_loop

	ret

	; init avatar sprite lights 
init_avatar_lites:
	mov di, SPRITE_TABLE + (4 * 4)
	mov si, avatar_lite_offsets
	mov cx, 25
init_avatar_lites_loop:
	; point at dark lite mini sprite
	mov word [es:di], 0x0006
	mov word bx, [cs:si]
	mov al, bh
	mov ah, bl
	mov word [es:di+2], ax
	
	add si, 2
	add di, 4
	dec cx
	jnz init_avatar_lites_loop
	
	ret

update_avatar_lites:
	
	mov di, SPRITE_TABLE + (4 * 4)
	mov si, lites
	mov cx, 25
update_avatar_lites_loop:

	mov byte al, [es:si]
	add al, 6
	mov [es:di], al
	
	add di, 4
	inc si

	dec cx
	jnz update_avatar_lites_loop

	ret

;
; Graphics data
;
	align 2

	cursor_offsets:
	dw 0x0000, 0x0800, 0x0008, 0x0808
	
	avatar_lite_offsets:
	db 160, 133,	157, 131,	154, 129,	151, 127,	148, 125
	db 163, 131,	160, 129,	157, 127,	154, 125,	151, 123
	db 166, 129,	163, 127,	160, 125,	157, 123,	154, 121
	db 169, 127,	166, 125,	163, 123,	160, 121,	157, 119
	db 172, 125,	169, 123,	166, 121,	163, 119,	160, 117
	
	tile_palette:
	incbin "tiles.palette"
	
	sprite_palette:
	incbin "sprites.palette"
	
	tiles:
	incbin "tiles.raw"
	
	litemap:
	incbin "litemap.bin"
	
	ROM_HEADER initialize, 0xf000, RH_WS_COLOR, RH_ROM_8MBITS, RH_NO_SRAM, RH_HORIZONTAL
	