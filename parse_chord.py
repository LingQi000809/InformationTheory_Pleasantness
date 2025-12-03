import pandas as pd
from music21 import stream, chord, note, tempo, metadata, pitch, interval, key as m21key, roman
import re

ROMAN_BASES = {
    "I": "C", 
    "II": "D", 
    "III": "E",
    "IV": "F", 
    "V": "G", 
    "VI": "A", 
    "VII": "B"
}

""" ALPHABET
[1] "r"     "V+"    "ii"    "V"     "I"     "VI"    "iii"   "vi"    "IV"    "II"    "v"     "i"     "-VII" 
[14] "-III"  "-VI"   "-vii"  "vii"   "III"   "iv"    "-iiio" "VII"   "-iii"  "-II"   "#vo"   "#io"   "#iio" 
[27] "#ivo"  "vo"    "viio"  "-III+" "-viio" "iio"   "vio"   "-II+"  "-V"    "#I"    "#II"   "#IV"   "#V"   
[40] "iiio"  "I+"    "-iio"  "-vio"  "#iv"   "VII+"  "-ii"   "#IV+"  "-vi"   "io"    "III+"  "ivo"   "-I"   
[53] "-IV"   "-VII+" "#v"    "IV+"   "-i"    "-vo" 
"""

def roman_to_chord(symbol: str, key: str = "C"):
    """
    Convert a Roman numeral chord symbol to a music21.chord.Chord, 
    interpreted relative to the given key.
    
    Args:
        symbol (str): Roman numeral chord symbol (e.g. 'ii', 'V+', '-IIIo', etc.)
        key (str): Tonal center (e.g. 'C', 'G', 'Bb', 'F#', 'a', etc.)
    
    Returns:
        music21.chord.Chord: The constructed chord object.
    """
    # Extract modifiers (#, -) and core numeral (I–VII)
    match = re.match(r"([#\-]*)([ivIV]+)([+o]*)", symbol)
    if not match:
        return chord.Chord([])  # return an empty chord (rest) as fallback

    try:
        accidental_part, numeral, quality_part = match.groups()

        # Determine base root pitch
        numeral_upper = numeral.upper()
        if numeral_upper not in ROMAN_BASES:
            return chord.Chord([])
        # Get base degree in scale
        tonic_key = m21key.Key(key)
        base_pitch = pitch.Pitch(ROMAN_BASES[numeral_upper])
        # Transpose base_pitch to key context
        key_interval = interval.Interval(noteStart=pitch.Pitch("C"), noteEnd=tonic_key.tonic)
        root = key_interval.transposePitch(base_pitch)

        # Apply accidentals
        semitone_shift = accidental_part.count('#') - accidental_part.count('-')
        root.transpose(semitone_shift, inPlace=True)

        # Determine chord quality (major/minor/aug/dim)
        is_minor = numeral.islower()
        is_aug = '+' in quality_part
        is_dim = 'o' in quality_part

        # Construct chord tones via intervals
        third = interval.Interval('m3' if is_minor or is_dim else 'M3')
        fifth = interval.Interval('d5' if is_dim else ('A5' if is_aug else 'P5'))
        chord_pitches = [root, third.transposePitch(root), fifth.transposePitch(root)]

        return chord.Chord(chord_pitches)
    except Exception as e:
        print(f"Got an error when parsing {symbol}; fall back to parse as a rest: {e}")
        return chord.Chord([])  # return an empty chord (rest) as fallback
