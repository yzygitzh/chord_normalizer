import os
import midi
import json

input_path = os.path.abspath("output/")
output_dir = os.path.abspath("midi_output/")
midi_velocity = 90

midi_bpm = 120
tbp = midi_bpm

mode_sequence = {"major": [0, 2, 4, 5, 7, 9, 11, 12], \
                 "minor": [0, 2, 3, 5, 7, 8, 10, 12]}
octave_height = [0, 12, 24, 36, 48, 60, 72, 84]

serialized_dict = {}

def insert_note(note, curr_note_pos):
    note_length = note["length"] * tbp
    tone_pitch_list = map(lambda x: int(x), list(note["sequence"]))
    tone_height_list = map(lambda x: int(x), list(note["height"]))
    if note["type"] == "chord":
        for i in range(len(tone_pitch_list)):
            if (1 <= tone_pitch_list[i] and 7 >= tone_pitch_list[i] and \
                1 <= tone_height_list[i] and 7 >= tone_height_list[i]):
                # insert noteon (curr_note_pos)
                if serialized_dict.has_key(curr_note_pos) == False:
                    serialized_dict[curr_note_pos] = []
                serialized_dict[curr_note_pos].append(\
                    [midi_velocity, \
                     octave_height[tone_height_list[i]] + \
                     mode_sequence[phrase_class][tone_pitch_list[i]]])
                # insert noteoff
                if serialized_dict.has_key(curr_note_pos + note_length) == False:
                    serialized_dict[curr_note_pos] = []
                serialized_dict[curr_note_pos].append(\
                    [0, \
                     octave_height[tone_height_list[i]] + \
                     mode_sequence[phrase_class][tone_pitch_list[i]]])
    else: # broken chord or melody
        interval = note_length / len(tone_pitch_list)
        for i in range(len(tone_pitch_list)):
            if (1 <= tone_pitch_list[i] and 7 >= tone_pitch_list[i] and \
                1 <= tone_height_list[i] and 7 >= tone_height_list[i]):
                # insert noteon (curr_note_pos)
                if serialized_dict.has_key(curr_note_pos) == False:
                    serialized_dict[curr_note_pos] = []
                serialized_dict[curr_note_pos].append(\
                    [midi_velocity, \
                     octave_height[tone_height_list[i]] + \
                     mode_sequence[phrase_class][tone_pitch_list[i]]])
                # insert noteoff
                if serialized_dict.has_key(curr_note_pos + interval) == False:
                    serialized_dict[curr_note_pos + interval] = []
                serialized_dict[curr_note_pos + interval].append(\
                    [0, \
                     octave_height[tone_height_list[i]] + \
                     mode_sequence[phrase_class][tone_pitch_list[i]]])
            curr_note_pos += interval

root_parent, root_dirnames, root_filenames = os.walk(input_path).next()
for input_file_name in root_filenames:
    with open(input_path + '/' + input_file_name, 'r') as input_file:
        input_json = json.load(input_file)
    output_file_name = input_json["filename"] + ".mid"

    serialized_dict = {}

    midi_pattern = midi.Pattern()
    midi_track = midi.Track()
    midi_pattern.append(midi_track)

    midi_track.append(midi.SetTempoEvent(tick=0,bpm=120))

    phrase_list = input_json["passage"]
    curr_note_pos = 0

    for phrase in phrase_list:
        phrase_pitch = phrase["pitch"]
        phrase_class = phrase["class"]
        noteset_list = phrase["phrase"]
        for noteset in noteset_list:
            max_note_len = 0
            for note in noteset:
                print note
                note_length = note["length"]
                if note_length > max_note_len:
                    max_note_len = note_length
                insert_note(note, curr_note_pos)
            curr_note_pos += max_note_len

    print serialized_dict

                    

















'''
pattern = midi.Pattern()
track = midi.Track()
pattern.append(track)

on = midi.NoteOnEvent(tick=0, velocity=90, pitch=midi.G_3)
track.append(on)
on = midi.NoteOnEvent(tick=0, velocity=90, pitch=midi.E_3)
track.append(on)

off = midi.NoteOffEvent(tick=10000, pitch=midi.G_3)
track.append(off)
off = midi.NoteOffEvent(tick=10000, pitch=midi.E_3)
track.append(off)

eot = midi.EndOfTrackEvent(tick=1)
track.append(eot)
'''
# Print out the pattern
# print pattern
'''
midi.write_midifile("example.mid", pattern)
'''