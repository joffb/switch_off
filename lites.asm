[map all lites.map]

%include "WonderSwan.inc"

MAP_FG equ WSC_TILE_BANK1 - MAP_SIZE
MAP_BG equ MAP_FG - MAP_SIZE
SPRITE_TABLE equ MAP_BG - SPR_TABLE_SIZE

PALETTE_SIZE equ 16 * 2

NUMBER_TILES equ 64
GFX_TILE_SIZE equ 32

LITEW equ 3
LITEH equ 3

STATE_GAME equ 0
STATE_WON equ 1
STATE_TITLE equ 2

BLINK_OPEN equ 1
BLINK_CLOSED equ 2

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
	
	blink_state: resw 1
	blink_timer: resw 1
	
	rnd_val:
	rnd_val_low: resw 1
	rnd_val_high: resw 1
	
	%include "music_bss.asm"

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

	; extra segment 0
	mov ax, 0x0000	
	mov es, ax
	
	; data segment = rom
	mov ax, 0xf000
	mov ds, ax
	
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
	or al, VMODE_16C_PLN | VMODE_CLEANINIT
	out IO_VIDEO_MODE, al
	
	; set sprite table location
	mov al, SPR_TABLE(SPRITE_TABLE)
	out IO_SPR_TABLE, al
	
	; map data locations
	mov ax, BG_MAP(MAP_BG) | FG_MAP(MAP_FG)
	out IO_FGBG_MAP, ax
	
	; display modes
	mov al, BG_ON | FG_ON | SPR_ON
	out IO_DISPLAY_CTRL, al
	
	; switch on display
	in al, IO_LCD_CTRL
	or al, LCD_ON
	out IO_LCD_CTRL, al
	
	; main palette
	mov si, tile_palette
	mov di, WSC_PALETTES
	mov cx, 16
	rep movsw
	
	; title screen palette
	mov si, title_palette
	mov di, WSC_PALETTES + PALETTE_SIZE
	mov cx, 16
	rep movsw
	
	; game over screen palette
	mov si, tile_palette
	mov di, WSC_PALETTES + PALETTE_SIZE * 4
	mov cx, 16
	rep movsw
	
	; sprite palette
	mov si, sprite_palette
	mov di, WSC_PALETTES + PALETTE_SIZE * 8
	mov cx, 16
	rep movsw

	; copy tile graphics into memory
	mov si, tiles
	mov di, WSC_TILE_BANK1
	mov cx, (16 * $100)
	rep movsw
	
	; copy title screen graphics into memory
	mov si, title_tiles
	mov di, WSC_TILE_BANK1 + (32 * 256)
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
	
	; 4 for cursor + 25 for avatar's lites + 4 for eyes
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
	
	; set pcm waveform location
	mov dx, IO_AUDIO_SAMPLE
	mov ax, pcm_waveforms
	shr ax, 6
	out dx, al
	
	; init values
	mov al, 0
	mov [es:vblankdone], al
	
	; random number seed
	mov ax, 0xac87
	mov [es:rnd_val_low], ax
	mov [es:rnd_val_high], ax
	
	; play music
	mov ax, song2
	call music_init
	
	; start at title screen
	mov ax, STATE_TITLE
	mov word [es:state], ax
	call draw_title

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
	
	; jump to code for this game state
	mov word bx, [es:state]
	shl bx, 1
	add bx, main_loop_jump_table
	mov word bx, [ds:bx]
	jmp bx
	
main_loop_game:
	
	; update and redraw the game timer
	call update_timer
	call draw_timer

	call update_avatar_blink
	
	call music_update

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
	
	; get buttons
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
	
	call flip_lites
	call draw_lites
	call update_avatar_lites
	
	call check_win

joy_done:
	
	jmp main_loop
	
main_loop_won:

	call music_update
	
	; poll buttons
	mov al, KEYPAD_READ_BUTTONS
	out IO_KEYPAD, al
	nop
	nop
	nop
	
	; get buttons
	in ax, IO_KEYPAD
	
	; start button restarts
	and ax, PAD_START
	jz main_loop
	
	; restart game
	
	; hide foreground
	mov al, BG_ON | SPR_ON
	out IO_DISPLAY_CTRL, al

	call init_game
	
	call draw_game_bg
	call draw_lites
	call update_avatar_lites
	call draw_movecount
	call draw_timer	
	
	mov ax, music_test
	call music_init
	
	jmp main_loop
	
main_loop_title:

	; poll buttons
	mov al, KEYPAD_READ_BUTTONS
	out IO_KEYPAD, al
	nop
	nop
	nop
	
	; get buttons
	in ax, IO_KEYPAD
	
	; start button restarts
	and ax, PAD_START
	jz title_continue
	
	call init_game
	call init_avatar
	call draw_game_bg
	call draw_lites
	call update_avatar_lites
	call draw_movecount
	call draw_timer
	
	; set display mode
	mov al, BG_ON | SPR_ON
	out IO_DISPLAY_CTRL, al
	
	mov ax, STATE_GAME
	mov [es:state], ax
	
	jmp main_loop

title_continue:

	call music_update

	mov ax, [es:tic]
	and ax, 0x3
	jnz main_loop

	; PLANE 1
	;
	; scroll blue diamond/plane 1 right for top two tiles
	mov cx, 8
	mov di, WSC_TILE_BANK1 + (256 * GFX_TILE_SIZE) + GFX_TILE_SIZE
	
	title_scroll_1:
		; get a row of pixels from the top two tiles
		mov al, [es:di]
		mov ah, [es:di + (GFX_TILE_SIZE * 2)]
		; rotate it around
		ror ax, 1
		; write it back
		mov [es:di], al
		mov [es:di + (GFX_TILE_SIZE * 2)], ah
		add di, 4
		loopnz title_scroll_1

	; PLANE 2
	;
	; scroll cyan circle/plane 2 downwards for leftmost tiles
	; start at the bottom byte of the graphics data and work our way up
	mov cx, 15
	mov di, WSC_TILE_BANK1 + (256 * GFX_TILE_SIZE) + GFX_TILE_SIZE + (GFX_TILE_SIZE * 2) + 1 - 4
	mov si, di
	
	; keep the bottom row byte around
	mov bl, [es:di]
	
	title_scroll_2:
		; replace current byte with the one below it
		mov al, [es:di-4]
		mov [es:di], al

		sub di, 4
		loopnz title_scroll_2
		
	; update top row with old bottom row byte
	mov [es:di], bl

	mov ax, [es:tic]
	and ax, 0x7
	jnz main_loop

	; PLANE 3
	;
	; scroll green triangle/plane 3 upwards for rightmost tiles	
	; start at the top byte of the graphics data and work our way down
	mov cx, 15
	mov di, WSC_TILE_BANK1 + (256 * GFX_TILE_SIZE) + GFX_TILE_SIZE + (GFX_TILE_SIZE * 2) + 2
	mov si, di
	
	; keep the top row byte around
	mov bl, [es:di]
	
	title_scroll_3:
		; replace current byte with the one above it
		mov al, [es:di+4]
		mov [es:di], al

		add di, 4
		loopnz title_scroll_3
	
	; update bottom row with old top row byte
	mov [es:di], bl

	mov ax, [es:tic]
	and ax, 0xf
	jnz main_loop

	; PLANE 4
	;
	; scroll yellow square/plane 4 left for bottom two tiles
	mov cx, 8
	mov di, WSC_TILE_BANK1 + (256 * GFX_TILE_SIZE) + GFX_TILE_SIZE + GFX_TILE_SIZE + 3
	
	title_scroll_4:
		; get a row of pixels from the top two tiles
		mov al, [es:di]
		mov ah, [es:di + (GFX_TILE_SIZE * 2)]
		; rotate it around
		rol ax, 1
		; write it back
		mov [es:di], al
		mov [es:di + (GFX_TILE_SIZE * 2)], ah
		add di, 4
		loopnz title_scroll_4
	
	jmp main_loop

vblank_interrupt:
	push ax
	push cx
	push di
	
	mov al, 1
	mov [es:vblankdone], al
	
	;acknowledge vblank
	in al, IO_INT_ACK
	or al, INT_VBLANK_START
	out IO_INT_ACK, al
	
	pop di
	pop cx
	pop ax
	iret
	
init_game:

	; reset cursor position
	mov ax, 0
	mov byte [es:px], al
	mov byte [es:py], al
	
	; reset buttons
	mov [es:last_dpad], ax
	mov [es:last_buttons], ax
	
	; reset timer
	mov [es:timertics], al
	mov [es:timer], ax
	
	; reset movecount
	mov [es:movecount], ax	
	
	; set game state
	mov ax, STATE_GAME
	mov [es:state], ax

	; set lites to off
	mov ax, 0
	mov cx, 5 * 5
	mov di, lites
	rep stosb
	
	; number of points to flip at the start (4..19)
	call rnd32
	and ax, 0xf
	add ax, 4
	mov cx, ax
	mov di, lites
	
init_game_lites_loop:

	; flip random point
	call rnd32
	; reduce 32 bit number to 16 bit so the division doesn't overflow
	xor ax, dx
	mov dx, 0
	div word [cs:rnd_lites_divisor]
	; use modulus as offset
	mov bx, dx
	mov al, [es:di+bx]
	not al
	and al, 0x1
	mov [es:di+bx], al

	loopnz init_game_lites_loop

init_game_check_solvability:

	; if the board isn't solvable, ax will contain 1
	call check_solvability
	and ax, 1
	
	jz init_game_done
	
	; flip random point
	mov di, lites
	call rnd32
	; reduce 32 bit number to 16 bit so the division doesn't overflow
	xor ax, dx
	mov dx, 0
	div word [cs:rnd_lites_divisor]
	; use modulus as offset
	mov bx, dx
	mov al, [es:di+bx]
	not al
	and al, 0x1
	mov [es:di+bx], al
	
	jmp init_game_check_solvability
	
init_game_done:

	ret

draw_game_bg:

	; set bg color for when there's no pixel data
	mov al, BG_COLOR(0) | BG_PAL(0)
	out IO_BG_PAL, al
	
	; draw background
	mov si, litemap
	mov di, MAP_BG
	mov cx, (32 * 18)
	rep movsw

	ret
	
draw_title:

	; set bg color for when there's no pixel data
	mov al, BG_COLOR(0) | BG_PAL(1)
	out IO_BG_PAL, al
	
	; draw background
	mov si, titlemap_bg
	mov di, MAP_BG
	mov cx, (32 * 32)
	rep movsw
	
	; draw background
	mov si, titlemap_fg
	mov di, MAP_FG
	mov cx, (32 * 18)
	rep movsw

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

	; flip lites
flip_lites:

	; flip lite under cursor
	; calculate offset of currently highlighted lite
	mov byte bl, [es:py]
	mov bh, bl
	shl bl, 2
	add bl, bh
	mov byte bh, [es:px]
	add bl, bh
	mov bh, 0
	
	; keep the offset for later
	mov dx, bx
	
	; flip lite
	mov di, lites
	mov byte al, [es:bx+di]
	not al
	and al, 0x1
	mov byte [es:bx+di], al

	; flip light to the left if we're not on the leftmost column
flip_lite_left:
	mov byte al, [es:px]
	cmp al, 0
	jz flip_lite_right
	
	mov bx, dx
	dec bx
	
	; flip lite
	mov byte al, [es:bx+di]
	not al
	and al, 0x1
	mov byte [es:bx+di], al

	; flip light to the right if we're not on the rightmost column
flip_lite_right:	
	mov byte al, [es:px]
	cmp al, 4
	jz flip_lite_top
	
	mov bx, dx
	inc bx
	
	; flip lite
	mov byte al, [es:bx+di]
	not al
	and al, 0x1
	mov byte [es:bx+di], al
	
	; flip light to the top if we're not on the topmost column
flip_lite_top:	
	mov byte al, [es:py]
	cmp al, 0
	jz flip_lite_bottom
	
	mov bx, dx
	sub bx, 5
	
	; flip lite
	mov byte al, [es:bx+di]
	not al
	and al, 0x1
	mov byte [es:bx+di], al
	
	; flip light to the bottom if we're not on the bottommost column
flip_lite_bottom:	
	mov byte al, [es:py]
	cmp al, 4
	jz flip_lite_done
	
	mov bx, dx
	add bx, 5
	
	; flip lite
	mov byte al, [es:bx+di]
	not al
	and al, 0x1
	mov byte [es:bx+di], al
	
flip_lite_done:
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
	
	; 1000s digit
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

;
; update cursor position
;
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
	
	loopnz update_cursor_loop

	ret

;
; init avatar 
init_avatar:
	
	; init blink state and timer
	mov ax, BLINK_OPEN
	mov [es:blink_state], ax
	
	mov ax, 250
	mov [es:blink_timer], ax

	; init sprites
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
	loopnz init_avatar_lites_loop
	
	; eyes
	mov di, SPRITE_TABLE + (4 * 4) + (25 * 4)
	
	; left left
	mov ax, (173 << 8) | (92)
	mov bx, 0x0008
	mov [es:di], bx
	mov [es:di + 2], ax	
	
	; left right
	add di, 4
	mov ax, (181 << 8) | (92)
	mov bx, 0x0009
	mov [es:di], bx
	mov [es:di + 2], ax	
	
	; right left
	add di, 4
	mov ax, (185 << 8) | (96)
	mov bx, 0x0008
	mov [es:di], bx
	mov [es:di + 2], ax	
	
	; right right
	add di, 4
	mov ax, (193 << 8) | (96)
	mov bx, 0x0009
	mov [es:di], bx
	mov [es:di + 2], ax	
	
	ret

;
; update avatar sprite lights
;
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

	loopnz update_avatar_lites_loop

	ret


update_avatar_blink:

	; decrease blink timer
	dec word [es:blink_timer]
	jnz update_avatar_blink_done
	
	; timer hits 0, update
	mov ax, [es:blink_state]

	; eyes are closed
	cmp ax, BLINK_CLOSED
	jnz update_avatar_blink_open_check
	
	call rnd
	and ax, 0xff
	add ax, 250
	mov [es:blink_timer], ax
	
	mov ax, BLINK_OPEN
	mov [es:blink_state], ax
	
	jmp update_avatar_blink_update_sprite
	
update_avatar_blink_open_check:

	;eyes are open
	cmp ax, BLINK_OPEN
	jnz update_avatar_blink_done

	mov ax, 15
	mov [es:blink_timer], ax
	
	mov ax, BLINK_CLOSED
	mov [es:blink_state], ax
	
	jmp update_avatar_blink_update_sprite

update_avatar_blink_update_sprite:

	; eyes
	mov di, SPRITE_TABLE + (4 * 4) + (25 * 4)
	mov ax, [es:blink_state]
	dec ax
	shl ax, 1
	add ax, 8
	
	mov [es:di], ax
	inc ax
	mov [es:di+4], ax	
	dec ax
	mov [es:di+8], ax
	inc ax
	mov [es:di+12], ax

update_avatar_blink_done:
	ret


;
; check whether the game has been won
;
check_win:

	mov di, lites
	mov ax, 0
	mov cx, 25
	
	; add up all values of lights
check_win_loop:
	add al, [es:di]
	inc di
	dec cx
	jnz check_win_loop
	
	; check whether the sum of all the lights is 0
	and al, al
	jnz check_win_not_won
	
	; if it is 0, we've won
	mov ax, STATE_WON
	mov [es:state], ax

	; copy win screen tiles to fg map
	mov si, winmap
	mov di, MAP_FG
	mov cx, (32 * 18)
	rep movsw
	
	; show foreground
	mov al, BG_ON | FG_ON | SPR_ON
	out IO_DISPLAY_CTRL, al
	
	mov ax, winsong
	call music_init
	
check_win_not_won:

	ret

;
; update game timer
; timer stored as a word of 4 bcd numbers
;
update_timer:

	; update timer tic counter
	inc byte [es:timertics]
	mov al, [es:timertics]
	
	; check if a second has elapsed
	cmp al, 75
	jnz update_timer_done
	
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
	jnz update_timer_minute_not_elapsed
	
	; increment minutes
	mov bl, 0
	mov al, bh
	inc al
	daa
	mov bh, al
		
update_timer_minute_not_elapsed:

	;update value in memory
	mov [es:timer], bx
	
update_timer_done:

	ret

; check solvability
; checks the "quiet patterns" of the board to see whether then
; total of lites lit within those patterns is odd (unsolvable) or even (solvable)
; http://www.cs.cmu.edu/~tcortina/15110m14/pa11/index.html
check_solvability:

check_solvability_qp1:

	mov di, lites
	mov si, qp1_offsets
	mov ax, 0
	mov cx, 12
	
check_solvability_qp1_loop:

	; get offset of lite to check
	mov bx, [cs:si]
	
	; add lite status at offset to al
	add byte al, [es:di+bx]
	
	add si, 2
	loopnz check_solvability_qp1_loop
	
	; check whether the number is odd
	; if it's not, check qp2
	; if it's odd, return with 1 in ax to indicate it's odd
	and ax, 1
	jnz check_solvability_done
	
check_solvability_qp2:
	
	; check qp1 
	mov si, qp2_offsets
	mov ax, 0
	mov cx, 12
	
check_solvability_qp2_loop:

	; get offset of lite to check
	mov bx, [cs:si]
	
	; add lite status at offset to al
	add byte al, [es:di+bx]
	
	add si, 2
	loopnz check_solvability_qp2_loop
	
	; check whether the number is odd
	and ax, 1

check_solvability_done:

	; if it is, return with 1 in ax to indicate it's odd
	ret

;
; put a random value in ax
;
rnd:
	push bx
	
	mov word ax, [es:rnd_val]
	
	; rnd_val ^= rnd_val << 7;
	mov bx, ax
	shl bx, 7
	xor ax, bx
    
	; rnd_val ^= rnd_val >> 9;
	mov bx, ax
	shr bx, 9
	xor ax, bx
	
    ; rnd_val ^= rnd_val << 8;
	mov bx, ax
	shl bx, 8
	xor ax, bx
	
	; update rnd val in memory
	mov word [es:rnd_val], ax
	
	pop bx

    ret

; put 32 bit random value in memory at rnd_val_low and rnd_val_high
; returns low word in ax, high word in dx
rnd32:
	push bx
	push cx
	
	; x = x ^ (x << 13)
	mov ax, [es:rnd_val_low]
	
	; low word shifted by 13 in cx
	; (actually just clearing the bits we don't care about and rotating by 3)
	mov cx, ax
	and cx, 0b111
	ror cx, 3
	; xor by the original value
	; calculated lower word is now in cx
	xor cx, ax

	; get upper word and shift it left by 13
	mov bx, [es:rnd_val_high]
	mov dx, bx
	and dx, 0b111
	ror dx, 3
	; get the shifted msb from the original lower word and combine them with the upper word
	shr ax, 3
	or dx, ax
	; xor the upper word by the shifted version and put in dx
	xor dx, bx
	
	; low word now in cx, high word now in dx
	
	; x = x ^ (x >> 17)
	; upper word gets shifted away so we can just xor with 0
	mov bx, dx
	xor bx, 0
	
	; lower word gets xor'ed with the upper word shifted right by 1
	shr dx, 1
	mov ax, cx
	xor ax, dx
	
	; low word now in ax, high word now in bx
	
	; x = x ^ (x << 5)
	; shift and xor low word
	mov cx, ax
	shl cx, 5
	xor cx, ax
	
	; isolate 5 msb in original low word
	rol ax, 5
	and ax, 0b11111
	; combine with shifted upper word
	mov dx, bx
	shl dx, 5
	or dx, ax
	; xor with original upper word
	xor dx, bx
	
	; store new values in memory
	mov ax, cx
	mov [es:rnd_val_low], cx
	mov [es:rnd_val_high], dx
	
	pop cx
	pop bx
	
	ret

;
; Graphics data
;
	align 2

	main_loop_jump_table:
	dw main_loop_game
	dw main_loop_won
	dw main_loop_title

	rnd_lites_divisor:
	dw 25

	cursor_offsets:
	dw 0x0000, 0x0800, 0x0008, 0x0808
	
	avatar_lite_offsets:
	db 160, 133,	157, 131,	154, 129,	151, 127,	148, 125
	db 163, 131,	160, 129,	157, 127,	154, 125,	151, 123
	db 166, 129,	163, 127,	160, 125,	157, 123,	154, 121
	db 169, 127,	166, 125,	163, 123,	160, 121,	157, 119
	db 172, 125,	169, 123,	166, 121,	163, 119,	160, 117
	
	qp1_offsets:
		dw 0, 2, 4
		dw 5, 7, 9
		dw 15, 17, 19
		dw 20, 22, 24
		
	qp2_offsets:
		dw  0,  1,  3,  4
		dw 10, 11, 13, 14
		dw 20, 21, 23, 24
	
	winsong:
	incbin "winsong.song"
	
	music_test:
	incbin "song1.song"
	
	song2:
	incbin "song2.song"
	
	tile_palette:
	incbin "tiles.palette"
	
	sprite_palette:
	incbin "sprites.palette"
	
	tiles:
	incbin "tiles.raw"
	
	title_palette:
	incbin "title_scroller.palette"
	
	title_tiles:
	incbin "title_scroller.raw"
	incbin "title.raw"
	
	litemap:
	incbin "litemap.bin"
	
	titlemap_bg:
	incbin "titlemap_bg.bin"
	
	titlemap_fg:
	incbin "titlemap_fg.bin"
	
	winmap:
	incbin "winmap.bin"
	
	%include "music_code.asm"
	
	ROM_HEADER initialize, 0xf000, RH_WS_COLOR, RH_ROM_4MBITS, RH_NO_SRAM, RH_HORIZONTAL