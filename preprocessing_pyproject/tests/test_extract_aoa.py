# call with 
# python3 -m pytest preprocessing_pyproject/tests/test_extract_aoa.py -v
import warnings

import pytest
import pandas as pd
import numpy as np
from preprocessing_pyproject.extract_aoa import extract_aoa, ALL_CORRECTIONS, translations, aoa_lookup
def test_extract_aoa_against_gold():
    fluency_df = pd.DataFrame({
     "Response": ["deutsch", "langarmshirt", "Kontaktlinse", 'rennradfahren', "Scheibenhonig ",  "      unteRwäsche", "ballüberdieschnur", "Frauenstasche", "Haarband", "None", "null", None, np.nan, "tshirt", "socke", "language",
     "t-shirt",  "none" ],
     "Gold": [6.79, (4.24+4.94+3.53)/3, (7.78+9.53)/2, 6.75, (5.69+5.44)/2, 3.94, 7.05, np.nan, np.nan, np.nan, np.nan, np.nan, np.nan, 3.53, 2.94,  np.nan, np.nan, np.nan ]
                    })

    tpl = fluency_df["Response"].apply(
        lambda x: extract_aoa(
            x,
            translations.get(x.lower().strip() if pd.notna(x) else x, np.nan),
            ALL_CORRECTIONS, aoa_lookup, aggMethod=np.nanmean
        )
    )
    fluency_df["AoA"] = [t[0] for t in tpl]


    for response, gold, actual in zip(fluency_df["Response"], fluency_df["Gold"], fluency_df["AoA"]):
        if pd.isna(gold):
            assert pd.isna(actual), f"Mismatch for '{response}': expected NaN, got {actual}"
        else:
            assert actual == pytest.approx(gold, abs=0.01), f"Mismatch for '{response}': expected {gold}, got {actual}"


def test_extract_aoa_no_runtime_warning_for_all_nan_compound():
    with warnings.catch_warnings(record=True) as captured:
        warnings.simplefilter("always")
        result = extract_aoa(
            "zzzz qqqq",
            "zzzz qqqq",
            ALL_CORRECTIONS,
            aoa_lookup,
            aggMethod=np.nanmean,
        )

    assert pd.isna(result[0])
    assert len(captured) == 0, "All-NaN compound fallback should not emit a runtime warning"