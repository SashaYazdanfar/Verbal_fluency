"""
GOAL: Merge childLex frequencies into the fluency dataframe.
"""
from pathlib import Path
import time
import pandas as pd
import spacy
import numpy as np
start = time.time()
nlp = spacy.load("de_core_news_sm")
parent_dir = Path(__file__).parent.parent


def normalize_response (item):
        if item =="fussball":
            item = "fußball"
        return item.lower().strip().replace("_","").replace("-","")

def  get_childlex_freq(row, childlex_df, childlex_types):
    """
    Process each row of the dataframe by:
    1. Search for a matching entry in ChildLex.
    2. Lemmatise if no matches found
    2. Extract max frequency if there are several candidates
    3. Extract noun frequency for semantic task
    """
    freqs = None

    # Normalize response
    response = normalize_response(str(row["Response"]))
    
    if response =="fussball":
         response = "fußball"

    # Create a lowercase comparison column for our df
    matches = childlex_df[childlex_types==response]

    if  matches.empty:
            # Extract lemma
            doc = nlp(str(response))
            lower_lemma = normalize_response(doc[0].lemma_)
            print(lower_lemma)
            matches = childlex_df[childlex_types == lower_lemma]

    # For semantic tasks --> preferrably nouns
    if len(str(row["Aufgabe"])) > 1 and row["Aufgabe"]:
                noun_matches = matches[matches["pos"] == "NN"]
                if len(noun_matches) > 0:
                    freqs = max(noun_matches["lemma.norm"].tolist())
                elif len(matches) > 0:
                    freqs = max(matches["lemma.norm"].tolist())
                else:
                    freqs = None

    # If it is a letter task, pick the max lemma regardless of POS.
    if not freqs and len(matches) > 0:
                        freqs = max(matches["lemma.norm"].tolist())
    if not freqs:
                        freqs = None

    return freqs


# Call functions
fluency_df = pd.read_excel(parent_dir / "data/processed/2026_08_24_fluencyAoAFreqs_df.xlsx")
childlex_df = pd.read_excel(parent_dir / "data/norms/childlex_0.17.01c.xlsx")
childlex_types = childlex_df["type"].str.lower().str.strip().str.replace("_","").str.replace("-","")

freqlist = []
for i, row in fluency_df.iterrows():
    freqs = get_childlex_freq(row,childlex_df,childlex_types )
    freqlist.append(freqs)

pd.set_option("display.max_rows", 500)
pd.set_option("display.max_columns", 500)
fluency_df["freq"] = freqlist

# with open (Path(__file__).parent.parent/"results/AoAFreqs_Nones.txt", "a") as f:
#      f.write(f'Missing Freqs:{fluency_df["freq"].isna().sum()}\n')

# fluency_df.to_excel(parent_dir /"results/2026_08_24_fluencyAoAFreqs_df.xlsx")
print(time.time() - start)
