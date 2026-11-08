# """
# GOAL: Merge childLex frequencies into the fluency dataframe.
# """
from pathlib import Path
# import time
import pandas as pd
import spacy
# import numpy as np
# start = time.time()
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




# Report number of matches>1
# def find_matches(response, childlex_df, childlex_types):
#     """Return (matches, source); source is 'form', 'lemma' or 'none'."""
#     matches = childlex_df[childlex_types == response]
#     if not matches.empty:
#         return matches, "form"
#     lemma = normalize_response(nlp(response)[0].lemma_)
#     matches = childlex_df[childlex_types == lemma]
#     return matches, ("lemma" if not matches.empty else "none")


# def match_diagnostics(response, childlex_df, childlex_types):
#     matches, source = find_matches(normalize_response(str(response)),
#                                    childlex_df, childlex_types)
#     nouns = matches[matches["pos"] == "NN"]
#     return {
#         "Response": response,
#         "match_source": source,
#         "n_matches": len(matches),
#         "n_noun_matches": len(nouns),
#         "n_noun_distinct_freq": nouns["lemma.norm"].nunique(),
#     }
# def is_semantic_task(aufgabe):
#     """Letter tasks are single letters (d, h, n, s); semantic tasks have longer names."""
#     return pd.notna(aufgabe) and len(str(aufgabe)) > 1

# fluency_df = pd.read_excel(parent_dir / "data/processed/2026_08_24_fluencyAoAFreqs_df.xlsx")
# childlex_df = pd.read_excel(parent_dir / "data/norms/childlex_0.17.01c.xlsx")
# childlex_types = childlex_df["type"].str.lower().str.strip().str.replace("_","").str.replace("-","")

# sem = fluency_df[fluency_df["Aufgabe"].apply(is_semantic_task)]
# sem_words = sem["Response"].drop_duplicates()
# assert len(sem) == (fluency_df["Condition"] == 2).sum()

# diag = pd.DataFrame([match_diagnostics(w, childlex_df, childlex_types)
#                      for w in sem_words])

# summary = {
#     "unique semantic words":                    len(diag),
#     "no match at all":                          (diag.n_matches == 0).sum(),
#     "matched only via lemma":                   (diag.match_source == "lemma").sum(),
#     "exactly 1 noun match":                     (diag.n_noun_matches == 1).sum(),
#     ">1 noun match":                            (diag.n_noun_matches > 1).sum(),
#     ">1 noun match AND different frequencies":  ((diag.n_noun_matches > 1) &
#                                                  (diag.n_noun_distinct_freq > 1)).sum(),
#     "no noun, other POS used":                  ((diag.n_noun_matches == 0) &
#                                                  (diag.n_matches > 0)).sum(),
# }
# print(pd.Series(summary))

# # the same at response level (every naming counts)
# resp_level = sem.merge(diag, on="Response", how="left")
# print((resp_level.n_noun_matches > 1).sum(), "of", len(resp_level), "responses")


# # unique semantic words                      947
# # no match at all                            290
# # matched only via lemma                       1
# # exactly 1 noun match                       607
# # >1 noun match                               44
# # >1 noun match AND different frequencies     11
# # no noun, other POS used                      6
# # dtype: int64
# # 381 of 4125 responses


# def match_diagnostics_any_pos(response, childlex_df, childlex_types):
#     matches, source = find_matches(normalize_response(str(response)),
#                                    childlex_df, childlex_types)
#     return {
#         "Response": response,
#         "n_matches": len(matches),
#         "n_distinct_freq": matches["lemma.norm"].nunique(),
#     }

# lex = fluency_df[~fluency_df["Aufgabe"].apply(is_semantic_task)]
# lex_words = lex["Response"].drop_duplicates()
# diag_lex = pd.DataFrame([match_diagnostics_any_pos(w, childlex_df, childlex_types)
#                          for w in lex_words])

# print("unique lexical words:", len(diag_lex))
# print(">1 match:", (diag_lex.n_matches > 1).sum())
# print(">1 match AND different frequencies:",
#       ((diag_lex.n_matches > 1) & (diag_lex.n_distinct_freq > 1)).sum())