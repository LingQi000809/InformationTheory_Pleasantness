import pandas as pd
from music21 import stream, chord, note, tempo, metadata, pitch, interval, layout
from parse_chord import roman_to_chord


# ---- Parsing CSV into sequences ----
# seq row  values  -> List[List[Dict[str, str]]]
def read_into_sequences(csv_path: str):
    """Read the CSV file and split into sequences."""
    sequences = []
    orig_seq, swap_seq = [], []
    function_type = ""
    with open(csv_path, 'r', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        for row in reader:
            if all(value == '' for value in row.values()):
                if orig_seq and swap_seq:
                    for row in orig_seq:
                        row['Function'] = function_type
                    for row in swap_seq:
                        row['Function'] = function_type
                    sequences.append(orig_seq)
                    sequences.append(swap_seq)
                orig_seq, swap_seq = [], []
                function_type = ""
            else:
                if row['Orig_Function'] and row['Swap_Function']:
                    function_type = "same" if row['Orig_Function'] == row['Swap_Function'] else "diff"
                seq_num = row['Seq'] if 'Seq' in row else ''
                orig_seq.append({
                    'Seq': seq_num + 'Orig' if seq_num else '',
                    'Row': row['Row'],
                    'Symbol': row['Orig_Symbol'],
                    'Order': row['Orig_Order'],
                    'IC': row['Orig_IC'],
                    'Entropy': row['Orig_Entropy'],
                    'Function': function_type
                })
                swap_seq.append({
                    'Seq': seq_num + 'Swap' if seq_num else '',
                    'Row': row['Row'],
                    'Symbol': row['Swap_Symbol'],
                    'Order': row['Swap_Order'],
                    'IC': row['Swap_IC'],
                    'Entropy': row['Swap_Entropy'],
                    'Function': function_type
                })
        # Add the last block if not empty
        if orig_seq and swap_seq:
            sequences.append(orig_seq)
            sequences.append(swap_seq)
    return sequences


def round_value(val):
    try:
        return round(float(val), 2)
    except Exception:
        return val

def write_seqs_to_xmls(df, label, max_batch=1):
    same_score = stream.Score()
    same_part = stream.Part()
    diff_score = stream.Score()
    diff_part = stream.Part()

    batch_idx = 0
    for sc in (same_score, diff_score):
        sc.insert(0, tempo.MetronomeMark(number=240))
        sc.metadata = metadata.Metadata(title=f"SwapPairs_Label{label}_Batch{batch_idx}")

    current_seq = None  # to track when a new sequence begins

    num_seq = 0
    max_seq = 200
    for iter, row in df.iterrows():
        if row.isna().all():
            continue

        # # Detect new sequence and add spacer between sequences
        if current_seq is not None and row['Seq'] != current_seq:
            spacer = note.Rest(quarterLength=4)
            if row['Function'] == 'same':
                same_part.append(layout.SystemLayout(isNew=True))
                same_part.append(spacer)
            else:
                diff_part.append(layout.SystemLayout(isNew=True))
                diff_part.append(spacer)
            num_seq += 1

            if num_seq >= max_seq - 1:
                if batch_idx >= max_batch - 1:
                    break

                same_score.append(same_part)
                diff_score.append(diff_part)
                same_score.write('musicxml', f'./stimuli/{label}_{batch_idx}_same_function.musicxml')
                diff_score.write('musicxml', f'./stimuli/{label}_{batch_idx}_different_function.musicxml')
                
                same_score = stream.Score()
                same_part = stream.Part()
                diff_score = stream.Score()
                diff_part = stream.Part()
                batch_idx += 1
                num_seq = 0
                for sc in (same_score, diff_score):
                    sc.insert(0, tempo.MetronomeMark(number=60))
                    sc.metadata = metadata.Metadata(title=f"SwapPairs_Label{label}_Batch{batch_idx}")

        current_seq = row['Seq']

        chord = roman_to_chord(str(row['Symbol']))
        chord.quarterLength = 4
        ic, ent = round_value(row.get('IC', '')), round_value(row.get('Entropy', ''))
        lyric1 = f"  IC: {ic}; Ent: {ent}  "
        lyric2 = f"{row['Seq']}_{row['Row']}"
        chord.addLyric(lyric1)
        chord.addLyric(lyric2)

        if row['Function'] == 'same':
            same_part.append(chord)
        else:
            diff_part.append(chord)

    # Add parts to scores
    same_score.append(same_part)
    diff_score.append(diff_part)

    # Write MusicXML
    same_score.write('musicxml', f'./stimuli/{label}_{batch_idx}_same_function.musicxml')
    diff_score.write('musicxml', f'./stimuli/{label}_{batch_idx}_different_function.musicxml')

import csv

def generate(label, max_batch = 5, read_to_sequences=False):
    if read_to_sequences:
        sequences = read_into_sequences(f'./stimuli/swap_pairs_{label}.csv')
        fieldnames = sequences[0][0].keys()
        with open(f"./stimuli/sequences_{label}.csv", 'w', newline='', encoding='utf-8') as f:
            writer = csv.DictWriter(f, fieldnames=fieldnames)
            writer.writeheader()
            for seq in sequences:
                writer.writerows(seq)
    df = pd.read_csv(f"./stimuli/sequences_{label}.csv")
    write_seqs_to_xmls(df, label, max_batch=max_batch)

generate(1, max_batch=999, read_to_sequences=False)