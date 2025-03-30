	
MUSIC_DATA_WAIT equ 0x0 << 2
MUSIC_DATA_NOTEON equ 0x1 << 2
MUSIC_DATA_NOTEOFF equ 0x2 << 2
MUSIC_DATA_RESTART equ 0x3 << 2
MUSIC_DATA_VOLUME equ 0x4 << 2
MUSIC_DATA_INSTRUMENT equ 0x5 << 2
MUSIC_DATA_ENVELOPE equ 0x6 << 2
MUSIC_DATA_NOISE equ 0x7 << 2

; ax - address of music data
music_init:

	; point at music data
	mov [es:music_data], ax
	
	mov di, ax
	
	; get pointer to instructions
	; by doing music_data + offset specified in music header
	mov bx, [ds:di]
	lea ax, [di+bx]
	mov [es:music_ptr], ax
	mov [es:music_instructions], ax
	
	; get pointer to instruments
	; by doing music_data + offset specified in music header
	mov bx, [ds:di+2]
	lea ax, [di+bx]
	mov [es:music_instruments], ax	
	
	; init tics to 0
	mov ax, 0
	mov [es:music_tic], ax
	
	mov dx, IO_AUDIO_CTRL
	mov al, AUDIO_1_ON | AUDIO_2_ON | AUDIO_3_ON | AUDIO_4_ON
	out dx, al
	
	; set audio output
	mov dx, IO_AUDIO_OUTPUT
	mov al, AUDIO_OUT_MONO | AUDIO_OUT_STEREO | 0b0110
	out dx, al
	
	; set default channel volumes
	mov al, 0xff
	mov di, music_vols
	mov [es:di], al
	mov [es:di+1], al
	mov [es:di+2], al
	mov [es:di+3], al
	
	; set channel volume
	mov al, 0x00
	mov dx, IO_AUDIO1_VOL
	out dx, al
	mov dx, IO_AUDIO2_VOL
	out dx, al
	mov dx, IO_AUDIO3_VOL
	out dx, al
	mov dx, IO_AUDIO4_VOL
	out dx, al
	
	; clear envelope pointers
	mov ax, 0
	mov cx, (music_env_size * 4) / 2
	rep stosw
	
	; set waiting
	mov ax, 0
	mov [es:music_wait_tic], ax
	mov [es:music_waiting], ax
	
	ret
	
music_update:
	
	; point di to next instruction
	mov di, [es:music_ptr]
	
	; see if we're waiting for a tic
	mov ax, [es:music_waiting]
	and ax, ax
	
	; we aren't
	jz music_update_loop

	; we are waiting for a tic
	mov ax, [es:music_wait_tic]
	mov bx, [es:music_tic]
	
	; not reached the tic yet
	xor ax, bx
	jnz music_update_done
	
	; reached the target tic
	mov ax, 0
	mov [es:music_waiting], ax
	
music_update_loop:

	; get next instruction
	mov byte al, [ds:di]
	
	; jump to address in jump table for this instruction
	mov bh, 0
	mov bl, al
	and bl, 0xfc
	shr bl, 1
	add bx, music_event_jump_table
	jmp [ds:bx]
	
music_event_wait:

	; get tic to wait for
	inc di
	mov word ax, [ds:di]
	mov [es:music_wait_tic], ax
	add di, 2
	
	; check if we're at the tic it's waiting for already
	mov bx, [es:music_tic]
	cmp ax, bx
	jz music_update_loop
	
	; set waiting flag
	mov ax, 1
	mov [es:music_waiting], ax
	
	; point to next event after the wait
	jmp music_update_done
	
music_event_note_on:
	
	; put channel number in bl
	mov bl, al
	and bl, 0x3
	mov bh, 0
	
	; get channel volume from wram
	mov si, music_vols	
	mov byte al, [es:si+bx]
	
	; set channel volume
	mov dx, IO_AUDIO1_VOL
	add dx, bx
	out dx, al
	
	; point at frequency
	inc di

	; set channel frequency
	mov dx, IOw_AUDIO1_FREQ
	add dl, bl
	add dl, bl
	mov ax, [ds:di]
	out dx, ax
	
music_event_note_on_envelope:

	; restart envelope
	; get envelope address ( * 8)
	mov ax, bx
	shl bx, 3
	add bx, music_envelopes
	
	; point envelope pointer to start of envelope data
	mov ax, [es:bx + music_env.start]
	mov [es:bx + music_env.ptr], ax
	
	add di, 2
	jmp music_update_loop
	
music_event_note_off:
	
	; put channel number in bl
	mov bl, al
	and bl, 0x3
	
	; set channel volume
	mov dx, IO_AUDIO1_VOL
	add dl, bl
	mov al, 0x00
	out dx, al
	
	inc di
	jmp music_update_loop

music_event_restart:
	
	; loop song
	mov ax, 0xffff
	mov [es:music_tic], ax
	
	mov di, [es:music_instructions]
	
	jmp music_update_loop

music_event_volume:

	; get new volume level and point at next instruction
	inc di
	mov byte cl, [ds:di]
	inc di
	
	; put channel number in bl
	mov bl, al
	and bl, 0x3
	
	; update channel volume in wram
	mov si, music_vols
	mov bh, 0
	mov byte [es:si+bx], cl
	
	jmp music_update_loop
	
music_event_instrument:
	
	; get new instrument and point at next instruction
	inc di
	mov byte cl, [ds:di]	
	inc di
	
	; point si at the start of the instrument data
	mov si, [es:music_instruments]
	
	; get the offset of the instrument data (* 16) and add it to si
	mov ch, 0
	shl cx, 4
	add si, cx
	
	push di
	
	; point di at waveform destination
	mov di, pcm_waveforms
	mov ah, 0
	and al, 0x3
	shl al, 4
	add di, ax
	
	; copy instrument data
	mov cx, 8
	rep movsw	
	
	pop di
	
	jmp music_update_loop

music_event_envelope:

	; put channel number in bl
	mov bl, al
	and bl, 0x3
	; turn into offset for envelope data in ram (* 8)
	shl bl, 3
	mov bh, 0
	mov si, music_envelopes
	; si now points at music envelope data for this channel
	add si, bx

	; di now points at envelope number
	inc di
	
	; put envelope number in bx
	mov bh, 0
	mov byte bl, [ds:di]

	; make bx the pointer for this envelope in the envelope pointer table
	mov dx, [es:music_data]
	
	shl bx, 1
	add bx, 4
	add bx, dx
	
	; put pointer to envelope data in bx
	mov bx, [ds:bx]
	add bx, dx
	
	; put envelope length in cx
	mov cx, [ds:bx]
	
	; store pointer to start of envelope data in ram (music_env_x_ptr)
	mov [es:si + music_env.start], bx
	
	; move pointer to end of envelope data so it doesn't play
	add bx, cx
	
	; store current pointer to envelope data in ram (music_env_x_ptr)
	mov [es:si + music_env.ptr], bx
	
	; store pointer to end of envelope data in ram (music_env_x_end)
	mov [es:si + music_env.end], bx
	
	inc di
	
	jmp music_update_loop

music_event_noise:
	
	; keep the instruction around
	mov bl, al
	
	; put the tap position in al
	inc di	
	mov byte al, [ds:di]
	
	; set the tap position and enable noise	
	or al, NOISE_ENABLE | NOISE_RESET
	out IO_AUDIO_NOISE_CTRL, al
	
	; move the on/off bit to the correct position
	and bl, 0x1
	ror bl, 1
	
	mov bh, 0
	mov [es:music_noise], bx
	
	; get the current audio control status
	; update it with the noise on/off bit
	in al, IO_AUDIO_CTRL
	and al, 0b01111111
	or al, bl
	
	; write the status
	out IO_AUDIO_CTRL, al
	
	inc di
	
	jmp music_update_loop

music_event_default:
	inc di
	jmp music_update_done

music_update_done:

	inc word [es:music_tic]
	mov [es:music_ptr], di

	; update envelopes
	mov dx, IO_AUDIO1_VOL
	mov si, music_vols
	
	mov cx, 4
	mov di, music_envelopes
	
music_update_envelopes:

	; envelope ptr
	mov bx, [es:di + music_env.ptr]
	; envelope end
	mov ax, [es:di + music_env.end]
	
	; check if we've reached the end of the envelope
	cmp ax, bx
	jz music_update_envelopes_next

	; envelope still active so update the track volume
	; get envelope volume in ah
	mov byte ah, [ds:bx]
	
	; move the pointer along and update it in ram, free up bx
	inc bx
	mov [es:di + music_env.ptr], bx
	
	; get current track volume in al
	mov byte al, [es:si]
	
	; left channel
	; scale envelope level by channel volume using lookup table
	mov bx, music_envelope_lookup
	mov bl, al
	and bl, 0xf
	shl bl, 4
	or bl, ah
	
	; store scaled value in ch temporarily
	mov ch, [ds:bx]
	
	; right value
	mov bl, al
	and bl, 0xf0
	or bl, ah
	mov byte al, [ds:bx]
	shl al, 4
	
	; combine the values
	or al, ch
	
	; update levels
	out dx, al
	
	; clear ch
	mov ch, 0
	
music_update_envelopes_next:

	inc dx
	inc si
	add di, music_env_size
	loopnz music_update_envelopes

	ret

debug_waveforms:

	; debug audio waveform
	mov si, pcm_waveform_1
	mov di, MAP_BG
	mov cx, 16
debug_waveform_1:
	mov al, [es:si]
	mov bl, al
	and bl, 0xf
	add bl, 64
	mov [es:di], bl
	add di, 2
	
	mov bl, al
	shr bl, 4
	add bl, 64
	mov [es:di], bl
	add di, 2
	
	inc si
	
	loopnz debug_waveform_1
	
	ret
	
	align 2
	music_event_jump_table:
	dw music_event_wait
	dw music_event_note_on
	dw music_event_note_off
	dw music_event_restart
	dw music_event_volume
	dw music_event_instrument
	dw music_event_envelope
	dw music_event_noise
	dw music_event_default
	dw music_event_default
	dw music_event_default
	dw music_event_default
	dw music_event_default
	dw music_event_default
	dw music_event_default
	dw music_event_default
	
	align 256
	music_envelope_lookup:
	%assign j 0
	%rep 16
		%assign i 0
		%rep 16
			db (((i * j) / 15) & 0xf)
			%assign i i+1
		%endrep
		%assign j j+1
	%endrep
	
