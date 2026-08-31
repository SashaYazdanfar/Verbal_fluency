# Verbal Fluency Data Analysis

This project enriches verbal fluency responses with lexical predictors and performs a follow-up statistical analysis of how word frequency and age of acquisition relate to performance across tasks and groups.

## Zipf Scores 
Zipf scores were calculated according to van Heuven (2014) : 
Zipf = (log10 (frequency count + 1)/ corpus size + number of word types) + 3.0
According to Schroeder et al. (2015) the corpus size is 10 Million without interpunctuation and 7.8 Million without interpunctuation. We use the number without punctuation.
Since our frequency values are computed at the lemma level, we use the number of lemma types reported by Schroeder et al. (2015; N = 120,000) rather than the number of word types (N = 180,000), to keep the numerator and denominator of the Zipf formula at the same level of aggregation. Note that the current childLex file yields a somewhat lower word type count (N = 164,798)

## Code Overview

The workflow is organized around three main stages:

1. Prepare the raw verbal fluency dataset.
2. Add lexical predictors such as ChildLex frequency and Kuperman age-of-acquisition (AoA).
3. Run statistical analyses in R to model missing frequency values, calculate Zipf scores, and compare group differences.

The Python scripts are used for data preprocessing and feature extraction. The R script then handles the modeling and visualization steps.

## Folder structure

- `data/raw/`: the first dataset used in the project. This contains the raw fluency data without lexical variables. It is the starting point for analysis.
- `data/interim/`: intermediate files produced after partial cleaning or preprocessing steps. These are temporary working files that sit between the raw data and the final processed dataset.
- `data/norms/`: lexical norms used for enrichment. This includes:
  - ChildLex frequency norms
  - Kuperman AoA norms
- `data/processed/`: final merged dataframe containing the verbal fluency data enriched with the extracted `freq` and `AoA` values.
- `preprocessing_pyproject/`: Python preprocessing package.
  - `extract_freqs.py`: merges ChildLex frequency estimates into the fluency dataset.
  - `extract_aoa.py`: adds AoA scores based on English Kuperman norms with translation and normalization steps.
  - `tests/`: manual validation tests for the extraction logic.
- `results/`: outputs from analysis. This includes the missing-value summary file (`AoAFreqs_Nones.txt`), as well as model diagnostics and residual-related plots in `results/plots/`.
- `statistics_R/`: R analysis scripts for:
  - linear prediction of missing word frequencies
  - conversion of frequencies to Zipf scores
  - group/task comparisons in R
  - residual and diagnostic plots
- `conftest.py`: pytest configuration file. In this project it is currently empty, but it is the place for shared pytest setup or fixtures if needed.

## Testing

The project includes a small set of tests in `preprocessing_pyproject/tests/`.

These tests are designed for manual QA and regression checking of the frequency and AoA extraction logic.

Run them with:

```bash
pytest preprocessing_pyproject/tests -q
```

## Requirements

This project is primarily Python-based for preprocessing and R-based for statistics.

### Python requirements

Install the following Python packages:

```bash
pip install pandas numpy spacy inflect openpyxl pytest
```

The project uses spaCy language models that must be downloaded separately:

```bash
python -m spacy download de_core_news_sm
python -m spacy download en_core_web_sm
```

### R requirements

The R script depends on the following packages:

- `dplyr`
- `tidyr`
- `performance`
- `readxl`
- `ordinal`
- `lme4`
- `lmerTest`
- `stringr`
- `tidytable`
- `broom`
- `purrr`
- `writexl`

These packages should be installed before running the analysis script in R.

> Note: the project does not currently require `deep_translator` or `GoogleTranslator`; these imports are not used in the active preprocessing pipeline.

## Typical run order

```bash
# 1. install Python dependencies
pip install -r requirements.txt
python -m spacy download de_core_news_sm
python -m spacy download en_core_web_sm

# 2. run preprocessing
python preprocessing_pyproject/extract_aoa.py
python preprocessing_pyproject/extract_freqs.py

# 3. run tests
pytest preprocessing_pyproject/tests -q

# 4. run the R analysis
Rscript statistics_R/2026_08_27_analysis.R
```

## Notes

- The repository currently uses a raw-to-processed pipeline rather than a full reproducible pipeline manager.
- Intermediate outputs can be viewed in `data/interim/` and the final merged files in `data/processed/`.
- The missing-value summary in `results/AoAFreqs_Nones.txt` is important for tracking how much data remains unmatched after frequency and AoA extraction.

## References
Kuperman, Victor, Hans Stadthagen-Gonzalez & Marc Brysbaert. 2012. Age-of-acquisition ratings for 30,000 English words. Behavior Research Methods 44(4). 978–990.

Schroeder, S., Würzner, K.-M., Heister, J., Geyken, A., & Kliegl, R. (2015). childLex: A lexical database of German read by children. Behavior Research Methods, 47, 1085-1094. doi:10.3758/s13428-014-0528-1
