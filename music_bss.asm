
	; pcm waveform memory
	alignb 64
	pcm_waveforms:
	pcm_waveform_1: resb 16
	pcm_waveform_2: resb 16
	pcm_waveform_3: resb 16
	pcm_waveform_4: resb 16
	
	; start of the song data
	music_data: resw 1
	; current instruction pointer
	music_ptr: resw 1
	; instruction data pointer
	music_instructions: resw 1
	; instrument data pointer
	music_instruments: resw 1
	; noise enabled?
	music_noise: resw 1
	
	; current tic
	music_tic: resw 1
	; we're waiting for a certain tic
	music_waiting: resw 1
	; tic we're waiting for
	music_wait_tic: resw 1
	
	; channel volumes
	music_vols:
	music_vol_0: resb 1
	music_vol_1: resb 1
	music_vol_2: resb 1
	music_vol_3: resb 1
	
	struc music_env
		.start resw 1
		.ptr resw 1
		.end resw 1
		.pad resw 1
	endstruc 
	
	; chennel envelopes
	music_envelopes:
	resb music_env_size
	resb music_env_size
	resb music_env_size
	resb music_env_size