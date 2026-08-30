"""
GOAL: Extract Age of Acquisition for German words using Kuperman et al. English norms.
Pipeline for each candidate response:
 1) lowercase/strip the German response
 2) map it to a translated English term
 3) normalize the English term for lexicon matching
 4) try direct AoA lookup
 5) if missing, try singular form
 6) if still missing, try multi-token fallback and lemma fallback
"""

from deep_translator import GoogleTranslator
import pandas as pd
import spacy
import numpy as np
import inflect
from pathlib import Path
from enum import Enum

nlp = spacy.load("en_core_web_sm")
p = inflect.engine()

# Functions
def normalize(item: str):
     return item.replace("-", " ").replace("'s","").replace("_", " ") if not pd.isna(item) else np.nan

def singularize(item: str):
      if pd.isna(item):
           return np.nan
      sing = p.singular_noun(item) 
      return sing if sing else np.nan

def lookup_single_word(aoa_lookup, translation, corrects):
    """
    Normalization is applied only after the translation has been resolved.
    This preserves the required order:
    raw German -> lower/strip -> translation -> normalize English -> match AoA

    Input: aoa_lookup - dictionary with Kuperman words and their ratings
            translation - translation of the word which is not necessarily in kuperman ratings
            corrects - hypernym and spelling corrections for translations to find them in kuperman ratings
    Output: rating - 
            target = match word
            translation =  translation with of corrects
    """
    translation = normalize(corrects.get(translation, translation))
    rating = aoa_lookup.get(translation, np.nan)
    target = translation
    if pd.isna(rating):
            # check sing form for multi tokens
            sing = singularize(translation) 
            target = sing if sing else np.nan
            rating = aoa_lookup.get(target, np.nan)
    if pd.isna(rating):
            target = np.nan
    return rating, target, translation



def extract_aoa(word, translation, corrects, aoa_lookup, nlp=nlp, p=p, aggMethod = np.nanmax):
    """
    main function
    input: word and translation + default: nlp + p (inflect engine)
    output: aoa_rating --> float
            match_word --> translation whose rating was found and extracted from kuper
    """
    if  pd.isna(word) or pd.isna(translation):
        return np.nan, np.nan, np.nan
    
    aoa_rating, match_word, translation = lookup_single_word(aoa_lookup, translation, corrects)
    if  pd.isna(aoa_rating) and len(translation.split()) >1:
         # check each token for multi tokens
         ratings_toks = []
         match_toks = []
         for tok in translation.split():
            aoa_rating_tok, match_tok, tok_translation = lookup_single_word(aoa_lookup, tok, corrects)
            if pd.isna(aoa_rating_tok):
                 # check lemma for multi tokens
                 lemma = nlp(tok)[0].lemma_
                 aoa_rating_tok = aoa_lookup.get(lemma, np.nan)
                 match_tok = lemma if not pd.isna(aoa_rating_tok) else np.nan
            match_toks.append(match_tok)
            ratings_toks.append(aoa_rating_tok)

         valid_ratings = [rating for rating in ratings_toks if not pd.isna(rating)]
         if not valid_ratings:
             aoa_rating = np.nan
             match_word = np.nan
         else:
             aoa_rating = aggMethod(valid_ratings)
             match_word = str(match_toks)

    if pd.isna(aoa_rating):
         # check lemma
         lemma = nlp(translation)[0].lemma_
         aoa_rating = aoa_lookup.get(lemma, np.nan)
         match_word = lemma if not pd.isna(aoa_rating) else np.nan

    if pd.isna(aoa_rating):
           return np.nan, np.nan, np.nan
    return aoa_rating, match_word, translation


# Dict of corrections for translations, so the words match the ratings
# use regex to extract hyphen, strip, get infinitive

# SPELLING_CORRECTS contains orthographical corrections, plural in singular, failed translations, infinitiv instgead of gerund
SPELLING_CORRECTS = {
            "t-shirt": "shirt",
            "t-shirts": "shirt",
            "cycling": "cycle",
            "tshirt": "shirt", 
            "ball over the line": "volleyball", # orig: ballüberdieschnur
            "socks": "sock",
            "drohnen": "drone",
            "kniestrumpf": "knee stocking",
            "berliner_ballen": "berliner doughnuts",
            "kirschtorte": "cherry pie",
            "keine_antwort": "no answer",
            "grizzlybär": "grizzly bear",
            "kartoffelchips": "potato chips",
            "fruchtbonbons": "fruit candy",
            "karamellbonbon": "caramel candy",
            "neurodermitis": "atopic dermatitis",
            "trekking": "trek",
            "haltevorrichtung": "holding device",
            "jackett": "jacket",
            "to swim": "swim",
            "dran": "your turn",
            "shuttlecock": "birdie",
            "chips":"crisp",
            "cookies":"cookie",
            "chewing gum": "bubblegum",
            "crocodiles": "crocodile",
            "hai": "shark",
            "seals": "seal",
            "muscle shirt": "sleeveless shirt",
            "jell-o": "jelly",
            "smelled": "smell",
            "cameleon":"chameleon",
            "sports": "sport",
            "foodball": "football",
            "essapier": "edible paper",
            "chocolte": "chocolate",
            "schuppe": "shovel",
            "doughnuts": "doughnut",
            "treggings":"leggings",
            "sailing": "sail",
            "lama": "llama",
            "shoes": "shoe",
            "had": "have",
            "has": 'have',
            "mr": "mister",
            "throwing": 'throw',
            "stones": "stone",
            "are": "be",
            "motorball": "motorcycle ball",
            "snoring": "snore",
            "piercing": "pierce",  
            "skydiving": "skydive",
            "surfing": "surf",  
            "sedentariness":"sedentary",
            "??jump": "jump",
            "rubgy": "rugby",
            "hairband": "hair band"
                      
}

HYPERNYM_SUBSTS = {
            "snow pants": "pants",
            "long underpants": "underpants",
            "langeunterhose": "underpants",
            "saurefritten": "sour gummy candy",
            "formel1": "sport",
            "einhundertmeterlauf": "100 meter dash",
            "saure_gummibärchen": "sour gummy candy",
            "water polo": "sport",
            "sour_gummy bears": "sour gummy candy",
            "chocolate bar": "chocolate",
            "fish species": "fish",
            "hip-hop": "dance",
            "blue whale": "whale",
            "knee socks": "sock",
            "run a marathon": "marathon",
            "ski jumping": "skiing",
            "table tennis": "tennis",
            "burning ball": "ball",
            "gummy bears": "gummy candy",
            "flagfoodball": "football",
            "hiphop": "dance",
            "twix": "chocolate",
            "haribo": "gummy candy",
            "maoam": "gummy candy",
            "kajtes": "gummy candy",
            "m&m's": "chocolate candy",
            "chess skewer": "skewer", # orig: SPieß, keine spezifizierung über Domain
            "tictac":"candy",
            "orio":"cookie",
            "mentos":"candy",
            "toffifee":"candy",
            "duplo":"chocolate",
            "milkyway": "chocolate",
            "nutella": "chocolate",
            "milka": "chocolate",
            "katjes": "gummy candy",
            "nestle": "chocolate",
            "m&ms": "chocolate candy",
            "pringles": "crisp",
            "lindt": "chocolate",
            "bueno": "chocolate",
            "m&ms": "chocolate candy",
            "tiramisu": "cake",
            "anorak": "jacket",
            "hoodie": "sweatshirt",
            "kitkat": "chocolate",
            "maltese": "dog",
            "german": "language",
            "germany": "country",
            "taketwo": "fruit candy",
            "breakdancing": "dance",
            "chupachups": "lollipop",
            "haribos": "gummy candy",
            "doner": "kebab",
            "strictly": "strict",
            "oreos": "cookie"
        }

ALL_CORRECTIONS = {**SPELLING_CORRECTS, **HYPERNYM_SUBSTS}

overlap = set(SPELLING_CORRECTS) & set(HYPERNYM_SUBSTS)
if overlap:
    print(f"Conflicts: {overlap}")

# Calling Functions
# Read Kuperman norms and interpret missing data correctly
# https://stackoverflow.com/questions/78423794/pandas-read-csv-with-keep-default-na-false-causing-change-in-data-type-of-valu
aoa_normen = pd.read_excel(Path(__file__).parent.parent/"data/norms/AoA_ratings_Kuperman_et_al_BRM.xlsx",  keep_default_na=False, dtype={"Word": str})
aoa_normen['Rating.Mean'] = pd.to_numeric(aoa_normen['Rating.Mean'], errors='coerce')

# Read translations for English AoA Ratings
translations_df = pd.read_excel(Path(__file__).parent.parent/"data/raw/translations.xlsx",dtype=str,
    keep_default_na=False)

# translations: german : english
translations = dict(zip(translations_df["word"], translations_df["translation"]))

#aoa_lookup : word - rating
aoa_lookup = (
    aoa_normen
    .set_index("Word")["Rating.Mean"]
    .to_dict()
)

fluency_df = pd.read_excel(Path(__file__).parent.parent/"data/raw/merged_and_cleaned_data.xlsx", keep_default_na=False, dtype=str)

tpl = fluency_df["Response"].apply(lambda x: extract_aoa(x, translations.get(x.lower().strip() if pd.notna(x) else x, np.nan), ALL_CORRECTIONS, aoa_lookup, aggMethod = np.nanmax))
fluency_df["AoA"], fluency_df["Match_word"], fluency_df["Translation"]= [t[0] for t in tpl], [t[1] for t in tpl], [t[2] for t in tpl]

# pd.set_option('display.max_columns', None)
# # pd.set_option('display.max_rows', None)


# Without spelling corrects
df_raw = fluency_df.copy()
# df_raw["AoA"] =  df_raw["Response"].apply(lambda x: aoa_lookup.get(translations.get(x.lower().strip() if pd.notna(x) else x, np.nan)))
# print(df_raw.isna().sum()) # 1750

# with spelling corrects
# tpl = fluency_df["Response"].apply(lambda x: extract_aoa(x, translations.get(x.lower().strip() if pd.notna(x) else x, np.nan), ALL_CORRECTIONS, aoa_lookup, aggMethod = np.nanmean))

# df_raw["AoA"] =  df_raw["Response"].apply(lambda x: extract_aoa(x, translations.get(x.lower().strip() if pd.notna(x) else x, np.nan), SPELLING_CORRECTS, aoa_lookup, aggMethod = np.nanmean))
# print(df_raw.isna().sum()) # 1750   

# Final missing-value logs after cleaning.
# We keep the pipeline order fixed: lowercase -> translate -> normalize -> match AoA.
spelling_only = df_raw["Response"].apply(
    lambda x: lookup_single_word( aoa_lookup,
        translations.get(x.lower().strip() if pd.notna(x) else x, np.nan),
        SPELLING_CORRECTS
    )[0] 
)
spelling_compounds = df_raw["Response"].apply(
    lambda x: extract_aoa(
        x,
        translations.get(x.lower().strip() if pd.notna(x) else x, np.nan),
        SPELLING_CORRECTS,
        aoa_lookup,
        aggMethod=np.nanmax,
    )[0] 
)


with open (Path(__file__).parent.parent/"results/AoA_Nones.txt", "w") as f:
     f.write(f"Missing AoA after cleaning with spelling corrections only:{spelling_only.isna().sum()}\n")
     f.write(f"Missing AoA after cleaning with spelling corrections + compounds: {spelling_compounds.isna().sum()}\n")
     f.write(f"Missing AoA after cleaning and aggregating missing compounds {fluency_df['AoA'].isna().sum()}\n")
# Save to excel
fluency_df.to_excel(Path(__file__).parent.parent/"data/processed/2026_08_24_fluencyAoAFreqs_df.xlsx")


# Sanity check
# print(lookup_single_word(aoa_lookup, "language", ALL_CORRECTIONS)) # works
# print(singularize('socks')) # works
# print(normalize("  LaNguage"))

# print(lookup_single_word(aoa_lookup, np.nan, ALL_CORRECTIONS)) # does not work
# print(singularize(np.nan)) # works
# print(normalize(np.nan))

# print(lookup_single_word(aoa_lookup, "None", ALL_CORRECTIONS)) # works
# print(type(singularize('None')), singularize('None')) # not work
# print(type(normalize("None")), normalize("None")) #works

# Check if pd.isna recognizes all types of Nones
# for word in ["", "None", "NaN", "null", None, "NA", np.nan, "np.nan"]:
#     if  pd.isna(word):
#             print(f"{word} is None")

# # Check which of None values are in the dataframe/ norms
# suspicious = ["NA", "null", "None", "nan", "N/A", "NULL", None, np.nan]
# for s in suspicious:
#     print(s, s in fluency_df["Response"].values)
