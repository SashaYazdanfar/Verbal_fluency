import pandas as pd

from preprocessing_pyproject.extract_freqs import normalize_response, get_childlex_freq


def test_normalize_response_removes_case_and_separators():
    assert normalize_response("  FUSS-BALL_ ") == "fussball"
    assert normalize_response("MÜSLI-Riegel") == "müsliriegel"


def test_get_childlex_freq_exact_match_prefers_noun_in_semantic_task_1():
    childlex_df = pd.DataFrame(
        {
            "type": ["fußball", "ball", "maske", "gute", "Gute"],
            "pos": ["NN", "NN", "NN", "ADJ", "NN"],
            "lemma.norm": [10.0, 8.0, 12.0, 16, 15],
        }
    )
    childlex_types = childlex_df["type"].str.lower().str.strip().str.replace("_", "").str.replace("-", "")

    row = {"Response": "Fussball", "Aufgabe": "Sport"}
    assert get_childlex_freq(row, childlex_df, childlex_types) == 10.0

def test_get_childlex_freq_exact_match_prefers_noun_in_semantic_task_2():
    childlex_df = pd.DataFrame(
        {
            "type": ["fussball", "ball", "maske", "gute", "Gute"],
            "pos": ["NN", "NN", "NN", "ADJ", "NN"],
            "lemma.norm": [10.0, 8.0, 12.0, 16, 15],
        }
    )
    childlex_types = childlex_df["type"].str.lower().str.strip().str.replace("_", "").str.replace("-", "")

    row = {"Response": "gute", "Aufgabe": "Kleidung"}
    assert get_childlex_freq(row, childlex_df, childlex_types) == 15.0

def test_get_childlex_freq_exact_match_prefers_max_in_letter_task():
    childlex_df = pd.DataFrame(
        {
            "type": ["fussball", "ball", "maske", "gute", "Gute"],
            "pos": ["NN", "NN", "NN", "ADJ", "NN"],
            "lemma.norm": [10.0, 8.0, 12.0, 16, 15],
        }
    )
    childlex_types = childlex_df["type"].str.lower().str.strip().str.replace("_", "").str.replace("-", "")

    row = {"Response": "gute", "Aufgabe": "g"}
    assert get_childlex_freq(row, childlex_df, childlex_types) == 16.0

def test_get_childlex_freq_exact_match_letter():
    childlex_df = pd.DataFrame(
        {
            "type": ["hund", "hunde", "hundes"],
            "pos": ["NN", "NN", "NN"],
            "lemma.norm": [15.0, 20.0, 25.0],
        }
    )
    childlex_types = childlex_df["type"].str.lower().str.strip().str.replace("_", "").str.replace("-", "")

    row = {"Response": "Hunde", "Aufgabe": "h"}
    result = get_childlex_freq(row, childlex_df, childlex_types)
    assert result == 20.0


def test_get_childlex_freq_notexact_match_letter():
    childlex_df = pd.DataFrame(
        {
            "type": ["läuft", "laufen", "laufen"],
            "pos": ["VV", "VV", "VV"],
            "lemma.norm": [ 20.0, 25.0, 27],
        }
    )
    childlex_types = childlex_df["type"].str.lower().str.strip().str.replace("_", "").str.replace("-", "")

    row = {"Response": "Lief", "Aufgabe": "h"}
    result = get_childlex_freq(row, childlex_df, childlex_types)
    assert result == 27.0
