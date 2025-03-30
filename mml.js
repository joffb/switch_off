
const DATA_WAIT = 0x0 << 2;
const DATA_NOTEON = 0x1 << 2;
const DATA_NOTEOFF = 0x2 << 2;
const DATA_RESTART = 0x3 << 2;
const DATA_VOLUME = 0x4 << 2;
const DATA_INSTRUMENT = 0x5 << 2;
const DATA_ENVELOPE = 0x6 << 2;
const DATA_NOISE = 0x7 << 2;

console.log(process.argv);

const fs = require('fs');
const readline = require('readline');

function make_track(id)
{
	return {
		id: id,
		tokens: [],
		octave: 3,
		tic: 0,
		note_len: 4,
		frames_per_quarter_note: 32
	}
}

var tracks = [
	make_track(0),
	make_track(1),
	make_track(2),
	make_track(3),
]

var note_numbers = {
	"c": 0,
	"c+": 1,
	"d-": 1,
	"d": 2,
	"d+": 3,
	"e-": 3,
	"e": 4,
	"f": 5,
	"f+": 6,
	"g-": 6,
	"g": 7,
	"g+": 8,
	"a-": 8,
	"a": 9,
	"a+": 10,
	"b-": 10,
	"b": 11,
}

var instruments = [];
var envelopes = [];
var events = [];
var variables = [];

const comment_regex = /^\s*;/g;
const note_track_regex = /^\s*([A-D])\s*/g;
const variable_regex = /^\s*(\![A-Za-z])\s+(.+)/;
const instrument_regex = /^\s*@([0-9]+)\s*{\s*([0-9\s]+)\s*\}/;
const envelope_regex = /^\s*@v([0-9]+)\s*{\s*([0-9\s]+)\s*\}/;
const tempo_regex = /^\s*T([0-9]+)\s*/;

const notes_regex  = /N([0-9]+)\,*([0-9]*)|(@[0-9]+|v[0-9]+\,*[0-9]*|@v[0-9]+|l[0-9]+|o[0-9]+|\^[0-9]+|r[0-9]*|[a-g][\+|\-]*[0-9]*|\<|\>|\?|\&)/g;

const octave_regex = /^o([0-9]+)$/;
const note_regex = /^([a-g][+|-]*)([0-9]*)$/;
const rest_regex = /^r([0-9]*)$/;
const note_len_regex = /^l([0-9]+)$/;
const caret_tie_regex = /^\^([0-9]+)$/;
const volume_regex = /^v([0-9]+)\,*([0-9]*)$/;
const instrument_select_regex = /^@([0-9]+)$/;
const envelope_select_regex = /^@v([0-9]+)$/;
const noise_enable_regex = /^N([0-9]+)\,*([0-9]*)$/;

async function process_mml() {
	
	const file = fs.createReadStream(process.argv[2]);

	const rl = readline.createInterface({
		input: file,
		crlfDelay: Infinity
	});

	for await (const line of rl)
	{
		// check if the line is a comment
		if (comment_regex.test(line))
		{
			continue;
		}
		
		// check if the line is an instrument definition
		else if (instrument_regex.test(line))
		{
			test_match = line.match(instrument_regex);
			
			instrument = { 
				id: parseInt(test_match[1]), 
				samples: []
			};
			
			// split the string to get the list of sample values
			samples = test_match[2].split(" ")
			
			for (i = 0; i < 32; i++)
			{
				if (i < samples.length)
				{
					instrument.samples[i] = parseInt(samples[i], 10) & 0xf;
				}
				else
				{
					instrument.samples[i] = 0;
				}
			}
			
			instruments.push(instrument);
			
			continue;
		}
		
		// check if the line is an envelope definition
		else if (envelope_regex.test(line))
		{
			test_match = line.match(envelope_regex);
			
			envelope = { 
				id: parseInt(test_match[1]), 
				levels: test_match[2].split(" ")
			};
			
			for (i = 0; i < envelope.levels.length; i++)
			{
				envelope.levels[i] = parseInt(envelope.levels[i], 10) & 0xf;
			}
			
			envelopes.push(envelope);
			
			continue;
		}
		
		// check if the line is a variable definition
		else if (variable_regex.test(line))
		{
			test_match = line.match(variable_regex);
			
			console.log(test_match);
			
			variables.push({name: test_match[1], content: test_match[2]});
			
			continue;
		}
		
		// check if the line is a variable definition
		else if (tempo_regex.test(line))
		{
			test_match = line.match(tempo_regex);
			
			console.log(test_match);
			
			for (i = 0; i < 4; i++)
			{
				tracks[i].tokens.push(test_match[0].trim());
			}
			
			continue;
		}
		
		// check if the line is a track
		notematch = line.match(note_track_regex);

		if (notematch && notematch.length == 1)
		{
			track = notematch[0].trim();
			
			switch (track)
			{
				case "A":
					track = 0;
					break;
				case "B":
					track = 1;
					break;
				case "C":
					track = 2;
					break;
				case "D":
					track = 3;
					break;
				default:
					track = false;
			}
			
			if (track === false)
			{
				continue;
			}
			
			//console.log ("track: " + track);
			
			content = line.replace(notematch[0], "");
			
			// expand variables
			while (content.indexOf("!") != -1)
			{
				for (var i = 0; i < variables.length; i++)
				{
					content = content.replace(variables[i].name, variables[i].content);
				}
			}
			
			tokens = content.match(notes_regex);
			
			tracks[track].tokens = tracks[track].tokens.concat(tokens);
			
			continue;
		}
	}

	//console.log(tracks);
	
	var i, j, track, token, matches, note_event;
	var last_note_event = null;
	
	
	for (i = 0; i < 4; i++)
	{
		track = tracks[i];
		
		for (j = 0; j < track.tokens.length; j++)
		{
			token = track.tokens[j];
		
			// create event array for this tic if one does not exist
			if (events[track.tic] === undefined)
			{
				events[track.tic] = [];
			}
		
			// octave down
			if (token == "<")
			{
				track.octave--;
			}
			// octave up
			else if (token == ">")
			{
				track.octave++;
			}
			// octave change
			else if (octave_regex.test(token))
			{
				matches = token.match(octave_regex)
				
				track.octave = parseInt(matches[1]);
			}
			// note length change
			else if (note_len_regex.test(token))
			{
				matches = token.match(note_len_regex)
				
				track.note_len = parseInt(matches[1], 10);
			}
			// note on
			else if (note_regex.test(token))
			{
				matches = token.match(note_regex);
				note_len = (matches[2] != "") ? parseInt(matches[2], 10) : track.note_len;
				note_len_tics = Math.floor((4.0 / note_len) * track.frames_per_quarter_note);
				
				note_event = {
					type: "note_on",
					track: track.id,
					note: matches[1],
					midi_note: note_numbers[matches[1]] + ((track.octave + 1) * 12),
					octave: track.octave,
					note_len: note_len,
					note_len_tics: note_len_tics,
				}
				
				events[track.tic].push(note_event);
				
				track.tic += note_len_tics;
				
				last_note_event = note_event;
			}
			// rest
			else if (rest_regex.test(token))
			{
				matches = token.match(rest_regex);
				note_len = (matches[1] != "") ? parseInt(matches[1], 10) : track.note_len;
				note_len_tics = Math.floor((4.0 / note_len) * track.frames_per_quarter_note);
				
				note_event = {
					type: "rest",
					track: track.id,					
					octave: track.octave,
					note_len: note_len,
					note_len_tics: note_len_tics,
				}
				
				events[track.tic].push(note_event);
				
				track.tic += note_len_tics;
				
				last_note_event = note_event;
			}
			// caret tie 
			else if (caret_tie_regex.test(token))
			{
				matches = token.match(caret_tie_regex);
				note_len = (matches[1] != "") ? parseInt(matches[1], 10) : track.note_len;
				note_len_tics = Math.floor((4.0 / note_len) * track.frames_per_quarter_note);
				
				if (last_note_event)
				{
					last_note_event.note_len += note_len;
					last_note_event.note_len_tics += note_len_tics;
					
					track.tic += note_len_tics;
				}
			}
			// volume
			else if (volume_regex.test(token))
			{
				matches = token.match(volume_regex);
				
				v1 = parseInt(matches[1], 10) & 0xf;
				v2 = (matches[2] != "") ? parseInt(matches[2], 10) & 0xf : ""
				
				if (matches[2] == "")
				{
					v1 = v1 | (v1 << 4)
				}
				else
				{
					v1 = v1 | (v2 << 4);
				}
				
				events[track.tic].push({type: "volume", volume: v1, track: track.id});
			}
			// instrument change
			else if (instrument_select_regex.test(token))
			{
				matches = token.match(instrument_select_regex)
				
				events[track.tic].push({
					type: "instrument", track: track.id, 
					instrument: parseInt(matches[1])
				});
			}
			// envelope change
			else if (envelope_select_regex.test(token))
			{
				matches = token.match(envelope_select_regex)
				
				events[track.tic].push({
					type: "envelope", track: track.id, 
					envelope: parseInt(matches[1])
				});
			}
			// noise enable
			else if (noise_enable_regex.test(token))
			{
				matches = token.match(noise_enable_regex);
				
				events[track.tic].push({
					type: "noise", 
					state: parseInt(matches[1]),
					tap: (matches[2] != "" ? parseInt(matches[2]) : 0)
				});
			}
			// tempo
			else if (tempo_regex.test(token))
			{
				matches = token.match(tempo_regex);
				
				track.frames_per_quarter_note = Math.floor((75.0 * 60.0) / parseFloat(matches[1]));
			}
		}
	}
	
	// add note off events
	event_keys = Object.keys(events);
	
	for (i = 0; i < event_keys.length; i++)
	{
		event_tic = parseInt(event_keys[i]);
		event_array = events[event_tic];
		
		for (j = 0; j < event_array.length; j++)
		{
			note_event = event_array[j];
			
			if (note_event.type == "note_on")
			{
				tic = event_tic + note_event.note_len_tics - 1
				
				if (events[tic] === undefined)
				{
					events[tic] = [];
				}
				
				events[tic].push({
					type: "note_off",
					track: note_event.track
				});
			}
		}
	}
	
	//console.log(tracks[2].tokens);
	//console.log(tracks[3].tokens);
	console.log(events);
	//console.log(variables);
	//console.log(instruments);
	//console.log(envelopes);
	
	// calculate size of instruments
	var instruments_size = (instruments.length * 16);
	
	// calculate size of envelope data
	// start with 2 bytes of length info
	var envelopes_size = envelopes.length * 2;
	
	// then get the how many steps are in each envelope
	for (i = 0; i < envelopes.length; i++)
	{
		envelopes_size += envelopes[i].levels.length;
	}
	
	// size of header
	var header_size = 2 + 2 + (envelopes.length * 2);
	
	// the file data which will be output
	var output = [];
	
	// format data
	//
	// header: 
	// 	instruction data pointer : word
	// 	instrument data pointer : word	
	// 	envelope data pointers
	// 		word per pointer
	//
	// data:
	//  instrument data
	// 		16 bytes per instrument, 2 samples of 4 bits per byte
	//  envelope data
	//  instruction data
	
	// start with pointer to instruction data
	var data_pos = header_size + instruments_size + envelopes_size;
	output.push(data_pos & 0xff);
	output.push((data_pos & 0xffff) >> 8);
	
	// then pointer to instrument data
	// this data comes after the header
	data_pos = header_size;
	output.push(data_pos & 0xff);
	output.push((data_pos & 0xffff) >> 8);
	
	// then the pointers to the envelope data (if any)
	if (envelopes.length > 0)
	{
		// first envelope is directly after the instrument_data
		data_pos = header_size + instruments_size;
		output.push(data_pos & 0xff);
		output.push((data_pos & 0xffff) >> 8);
		
		// subsequent envelopes follow
		for (i = 0; i < envelopes.length - 1; i++)
		{
			data_pos += envelopes[i].levels.length + 2;
			output.push(data_pos & 0xff);
			output.push((data_pos & 0xffff) >> 8);
		}
	}
		
	// include instrument data
	for (i = 0; i < instruments.length; i++)
	{
		ins = instruments[i];
		
		for (j = 0; j < 32; j += 2)
		{
			output.push(ins.samples[j] | ins.samples[j+1] << 4);
		}
	}
	
	// include envelope data
	for (i = 0; i < envelopes.length; i++)
	{
		env = envelopes[i];
	
		output.push(env.levels.length & 0xff);
		output.push((env.levels.length & 0xffff) >> 8);
		
		for (j = 0; j < env.levels.length; j++)
		{
			output.push(env.levels[j]);
		}
	}
	
	event_keys = Object.keys(events);
	
	// create output
	for (i = 0; i < event_keys.length; i++)
	{
		event_array = events[event_keys[i]];
		
		// wait for this tic to arrive before doing anything
		output.push(DATA_WAIT)
		output.push(event_keys[i] & 0xff);
		output.push((event_keys[i] & 0xffff) >> 8);
		
		for (j = 0; j < event_array.length; j++)
		{
			ev = event_array[j];
			
			// note on instruction
			if (ev.type == "note_on")
			{				
				// calculate the frequency of the note in hz
				frequency = Math.pow(2, (ev.midi_note - 69) / 12.0) * 440.0;
				
				// calculate the value for the wonderswan registers which will result in this frequency
				wswan_frequency = Math.floor((2048.0 * frequency - 96000.0) / frequency);
				wswan_frequency &= 0x7ff;
			
				output.push (DATA_NOTEON | ev.track);
				output.push(wswan_frequency & 0xff);
				output.push(wswan_frequency >> 8);
			}
			
			// note off instruction
			else if (ev.type == "note_off")
			{
				output.push (DATA_NOTEOFF | ev.track);
			}
			
			// volume change instruction
			else if (ev.type == "volume")
			{
				output.push (DATA_VOLUME | ev.track);
				output.push (ev.volume);
			}
			
			// instrument change instructions
			else if (ev.type == "instrument")
			{
				
				// find the index of this instrument in the array
				instrument_index = null;
				
				for (inst = 0; inst < instruments.length; inst++)
				{
					if (instruments[inst].id == ev.instrument)
					{
						instrument_index = inst;
						break;
					}
				}
				
				// write the instructions if we've found that instrument
				if (instrument_index !== null)
				{
					output.push(DATA_INSTRUMENT | ev.track);				
					output.push(instrument_index);
				}
			}
			
			// envelope change instructions
			else if (ev.type == "envelope")
			{
				// find the index of this envelope in the array
				envelope_index = null;
				
				for (env = 0; env < envelopes.length; env++)
				{
					if (envelopes[env].id == ev.envelope)
					{
						envelope_index = env;
						break;
					}
				}
				
				// write the instructions if we've found that instrument
				if (envelope_index !== null)
				{
					output.push(DATA_ENVELOPE | ev.track);				
					output.push(envelope_index);
				}
			}
			
			// noise enable/disable instruction
			else if (ev.type == "noise")
			{
				output.push (DATA_NOISE | (ev.state & 0x1));
				output.push(ev.tap & 0x7);
			}
		}
	}
	
	output.push(DATA_RESTART);
	
	//console.log(output);
	
	output_buffer = Buffer.from(output);
	
	fs.writeFileSync(process.argv[3], output_buffer);
}

process_mml();

