# PenguinoidRTools

[![R](https://github.com/GavinDuley/PenguinoidRTools/actions/workflows/r.yml/badge.svg)](https://github.com/GavinDuley/PenguinoidRTools/actions/workflows/r.yml)

A collection of useful R functions for statistical analysis and colour science.

## Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("GavinDuley/PenguinoidRTools")
```

## Functions

### ANOVA Summary Functions

#### `aovSummaryTable(aov_data, group_var, output_file, return_raw, output_name)`

Generates a comprehensive summary table of ANOVA results for a whole dataframe without interaction terms.

**Parameters:**
- `aov_data` - Data frame containing the data to be analysed
- `group_var` - Name of the grouping variable column (in quotes)
- `output_file` - Optional Excel filename (.xlsx) to save the table
- `return_raw` - Logical; if TRUE, stores raw ANOVA and Tukey test outputs
- `output_name` - Name for the global variable storing raw outputs (default: "aov_results")

**Output:** A summary table containing:
- Group means with Tukey HSD letter groupings
- P-values and F-values for each variable
- Significance indicators
- Benjamini-Hochberg (BH) corrected p-values and significance

**Example:**
```r
library(agricolae)
library(PenguinoidRTools)

# Load example data
data(greenhouse, package = "agricolae")
aov_data <- greenhouse$greenhouse1

# Run analysis
result <- aovSummaryTable(
  aov_data = aov_data,
  group_var = "variety",
  output_file = "results.xlsx",
  return_raw = TRUE
)
print(result)
```

#### `aovInteractSummaryTable(aov_data, group_vars, output_file, return_raw, output_name)`

Generates a summary table of ANOVA results including interaction terms for two, three, or four-way factorial designs.

**Parameters:**
- `aov_data` - Data frame containing the data to be analysed
- `group_vars` - Vector of grouping variable column names
- `output_file` - Optional Excel filename (.xlsx) to save the table
- `return_raw` - Logical; if TRUE, stores raw ANOVA and Tukey test outputs
- `output_name` - Name for the global variable storing raw outputs (default: "aov_results")

**Output:** A summary table containing:
- Group means with Tukey HSD letter groupings for all factor combinations
- P-values and F-values for main effects and all interactions
- Significance indicators for each effect
- BH-corrected p-values and significance for all effects

**Sums of squares and unbalanced designs:** P-values and F-values use Type II sums of squares – each effect is tested after all other effects that do not contain it, against the residual mean square of the full model – so the results do not depend on the order of `group_vars`. In a balanced design (the same number of observations in every combination of the grouping variables) these are identical to the sequential (Type I) results of `summary(aov())`. In an unbalanced design they differ, and a warning names the affected variables. A design can be balanced overall but unbalanced for a particular variable, because observations with an `NA` in that variable are dropped for its ANOVA. Group means and Tukey letters are unaffected.

**Example:**
```r
library(agricolae)
library(PenguinoidRTools)

# Load example data
data(greenhouse, package = "agricolae")
greenhouse1_data <- greenhouse$greenhouse1

# Two-way ANOVA with interaction
result <- aovInteractSummaryTable(
  aov_data = greenhouse1_data,
  group_vars = c("variety", "method"),
  output_file = "interaction_results.xlsx"
)
print(result)
```

---

### CIELab Colour Functions

R functions for calculating CIELab colour coordinates from spectrophotometry data using the OIV-MA-AS2-11 (R2006) method with D65 illuminant and 10 degree standard observer.

#### `cielab_spectroscopy_percentage_to_fraction(transmission_pct)`

Converts spectrophotometry transmission data from percentage (0-100) to fraction (0-1) format.

**Example:**
```r
# Convert 50% transmission to fraction
cielab_spectroscopy_percentage_to_fraction(50)
# Returns: 0.5

# Convert a vector of percentages
cielab_spectroscopy_percentage_to_fraction(c(10, 50, 90))
# Returns: c(0.1, 0.5, 0.9)
```

#### `cielab_from_spectrum(data, sample_col, path_mm)`

Converts spectrophotometry transmission data to CIELab colour values using the OIV method. **Automatically detects data format** - just pass your dataframe and get CIELab values back.

**Input:** A data.frame in either format:

1. **Wide format** (typical from spectrophotometers):
   - Sample identifier column (e.g., `WineName`, `Sample`)
   - Wavelength columns named `X780`, `X779`, ... `X380`
   - Values as transmission fraction (0-1)

2. **Long format**:
   - Sample identifier column (e.g., `WineName`)
   - `lambda` - Wavelength in nm
   - `T` - Transmission as fraction (0-1)

**Parameters:**
- `data` - Your spectroscopy dataframe (wide or long format)
- `sample_col` - Column to group by. **This controls averaging:**
  - `"WineName"` (default) - One result per wine, averages replicates
  - `"Replicate"` - One result per replicate, no averaging
- `path_mm` - Cuvette path length in mm (default: 10); must be a single positive number

**Output:** A data.frame with one row per unique value of `sample_col`:
- `<sample_col>` - Your sample identifier
- `CIELab_L` - L* lightness (0-100)
- `CIELab_a` - a* green-red axis
- `CIELab_b` - b* blue-yellow axis
- `CIELab_C` - Chroma (saturation)
- `CIELab_H` - Hue angle in degrees (0-360)

**Note:** Transmission values slightly > 1 (e.g., 1.003) are normal instrument noise and ignored. Only values > 1.1 trigger a warning.

**Example:**
```r
# Wide format - just pass your data directly
result <- cielab_from_spectrum(spectroscopy_data)
# Returns one row per WineName, replicates averaged

# Get individual results per replicate (no averaging)
result <- cielab_from_spectrum(spectroscopy_data, sample_col = "Replicate")

# Long format works too
result <- cielab_from_spectrum(spectroscopy_long)

# Use different path length
result <- cielab_from_spectrum(my_data, path_mm = 5)
```

#### `lab_to_C(a, b)`

Calculate chroma (C*) from CIELab a* and b* values.

**Example:**
```r
lab_to_C(30, 40)  # Returns 50
```

#### `hue_deg(a, b)`

Calculate hue angle in degrees from CIELab a* and b* values.

**Example:**
```r
hue_deg(1, 1)   # Returns 45
hue_deg(-1, 1)  # Returns 135
```

#### `delta_h_signed(h2, h1)`

Calculate signed hue difference, correctly handling wraparound at 360 degrees.

**Example:**
```r
delta_h_signed(10, 350)   # Returns 20 (crossing 0)
delta_h_signed(350, 10)   # Returns -20
```

#### `deltaE76(L1, a1, b1, L2, a2, b2)`

Calculate CIE76 colour difference (Delta E) between two colours.

**Delta E interpretation (rough):**
- < 1: Generally not perceptible to the human eye
- 1-2: Perceptible through close observation
- > 2: Increasingly perceptible at a glance

CIE76 Delta E is unbounded and not perceptually uniform; for perceptually
uniform differences use the CIE2000 method (`calc_pairwise_dE(..., method = "CIE2000")`).

**Example:**
```r
deltaE76(50, 10, 20, 55, 15, 25)
```

#### `deltaE00(L1, a1, b1, L2, a2, b2)` / `deltaE94(L1, a1, b1, L2, a2, b2)`

Standalone CIE2000 and CIE94 colour differences between two colours, as peers of `deltaE76()`. Arguments are recycled to a common length and compared pairwise. Both delegate to `farver::compare_colour()`. Like all Delta E metrics, these always compare a *pair* of colours.

**Example:**
```r
deltaE00(50, 10, 20, 55, 15, 25)
deltaE94(50, 10, 20, 55, 15, 25)
```

#### `cielab_to_hex(L, a, b, fixup = TRUE)`

Convert CIELab coordinates to hexadecimal sRGB colour strings (e.g. for plotting). Requires the `colorspace` package.

**Example:**
```r
cielab_to_hex(c(30, 90), c(50, -5), c(30, 10))
# Returns e.g. c("#8F0F1B", "#E1E5CF")
```

#### `calc_colour_diff(data, compare_by, reference_level, group_by)`

Calculate centroid-to-centroid colour differences between groups. Computes the mean L*, a*, b* for each group and returns differences (dL, dC, dh, dE76, dE00) relative to a reference level.

**Parameters:**
- `data` - Data frame with `CIELab_L`, `CIELab_a`, `CIELab_b` columns
- `compare_by` - Column name of factor to compare (default: "EtOH")
- `reference_level` - Reference level for comparisons (default: "CTRL")
- `group_by` - Optional grouping columns for nested designs (e.g., compare within each wine)

**Output:** A data.frame with columns:
- `contrast` - Description of comparison (e.g., "Treatment – CTRL")
- `dL` - Difference in L* (lightness)
- `dC` - Difference in C* (chroma)
- `dh` - Signed difference in hue angle (degrees)
- `dE76_cent` - CIE76 Delta E between centroids
- `dE00_cent` - CIE2000 Delta E between centroids

**Example:**
```r
# Compare ethanol treatments to control
results <- calc_colour_diff(
  data = wine_colours,
  compare_by = "EtOH",
  reference_level = "CTRL"
)

# Compare within each wine variety
results <- calc_colour_diff(
  data = wine_colours,
  compare_by = "Treatment",
  reference_level = "Control",
  group_by = "WineType"
)
```

#### `calc_pairwise_dE(data, compare_by, reference_level, target_levels, group_by, method)`

Calculate mean pairwise colour differences between all observations in different groups. This gives a distribution of Delta E values rather than a single centroid-based value.

**Parameters:**
- `data` - Data frame with `CIELab_L`, `CIELab_a`, `CIELab_b` columns
- `compare_by` - Column name of factor to compare (default: "EtOH")
- `reference_level` - Reference level for comparisons (default: "CTRL")
- `target_levels` - Specific levels to compare (NULL = all non-reference levels)
- `group_by` - Optional grouping columns for nested designs
- `method` - Either "CIE1976" (default) or "CIE2000" (not case-sensitive); any other value is an error

**Output:** A data.frame with columns:
- `comparison` - Description of comparison
- `dE_mean` - Mean Delta E across all pairwise comparisons
- `dE_sd` - Standard deviation of Delta E values

**Example:**
```r
# Compare all treatments to control using CIE76
results <- calc_pairwise_dE(
  data = wine_colours,
  compare_by = "EtOH",
  reference_level = "CTRL"
)

# Use CIE2000 for more perceptually uniform differences
results <- calc_pairwise_dE(
  data = wine_colours,
  compare_by = "Treatment",
  reference_level = "Control",
  method = "CIE2000"
)

# Compare specific treatments only
results <- calc_pairwise_dE(
  data = wine_colours,
  compare_by = "Treatment",
  reference_level = "Control",
  target_levels = c("Low", "High")
)
```

#### `cielab_kinetics(data, time_col, group_by, comparison, time_levels)`

Tracks how wine colour changes across an ordered series of timepoints. For each combination of grouping variables (e.g. wine and treatment), it computes the centroid (mean L\*, a\*, b\*) at each timepoint and reports colour differences relative to the first timepoint (`"baseline"`), between consecutive timepoints (`"consecutive"`), or `"both"` (default). A time-ordered companion to `calc_colour_diff()`.

**Parameters:**
- `data` - Data frame with `CIELab_L`, `CIELab_a`, `CIELab_b` columns
- `time_col` - Column giving the timepoint. Numeric, ordered factor, or labels with embedded numbers (e.g. `"0M"`, `"3M"`, `"12M"`) are ordered chronologically automatically; for an unordered factor with non-numeric labels, pass `time_levels`.
- `group_by` - Column(s) identifying each colour series (e.g. `c("Wine", "Treatment")`); NULL treats all rows as one series
- `comparison` - `"both"` (default), `"baseline"`, or `"consecutive"`
- `time_levels` - Optional explicit chronological order of timepoints

**Output:** A data.frame with `comparison_type`, `from_time`, `to_time`, `contrast`, `n_from`, `n_to`, and the colour differences `dL`, `dC`, `dh`, `dE76`, `dE00`. Differences are (later − earlier), so positive `dL` means the wine lightened over time.

**Example:**
```r
# Colour change of each wine x treatment over storage time
kin <- cielab_kinetics(
  data = wine_colours,
  time_col = "Months",
  group_by = c("Wine", "Treatment")
)

# Only cumulative change from the first timepoint
kin <- cielab_kinetics(wine_colours, "Months", comparison = "baseline")
```

#### `cielab_swatch(CIELab, file, ...)`

Create and save colour swatch visualisations to various formats (PNG, PDF, SVG, etc.).

**Input:** A data.frame with columns:
- `WineName` - Labels for each colour swatch
- `CIELab_L` - L* values (0-100)
- `CIELab_a` - a* values
- `CIELab_b` - b* values

**Optional parameters:**
- `file` - Output filename (extension determines format)
- `chips_per_row` - Swatches per row (NULL = auto)
- `cm_per_chip` - Size of each tile in cm
- `border_col` - Border colour
- `font_size_pt` - Font size in points
- `dpi` - Resolution for raster formats

**Example:**
```r
df <- data.frame(
  WineName = c("Red", "White", "Rose"),
  CIELab_L = c(30, 90, 70),
  CIELab_a = c(50, -5, 20),
  CIELab_b = c(30, 10, 15)
)
cielab_swatch(df, "swatches.png")
cielab_swatch(df, "swatches.pdf")
cielab_swatch(df, "swatches.svg")
```

#### `process_spectrum(folder_path, ...)`

Process a folder of spectrophotometer CSV files into a combined data frame.

**Parameters:**
- `folder_path` - Path to folder containing CSV files
- `skip` - Header rows to skip (default 13)
- `wavelength_col` - Wavelength column name (default "WL.nm.")
- `transmission_col` - Transmission column name (default "X.T")
- `wl_min`, `wl_max` - Wavelength range (default 300-800)

**Output:** A data.frame with columns:
- `sample_name` - Sample identifier from filename
- `lambda` - Wavelength in nm
- `T` - Transmission as fraction (0-1)

**Example:**
```r
# Process all CSV files in a folder
spectra <- process_spectrum("path/to/spectrophotometer/data")

# Then calculate CIELab - just pass the result directly!
result <- cielab_from_spectrum(spectra)
```

---

### Agilent MassHunter UV-DAD Peak Table Functions

#### `read_agilent_dad_peaks(file_path, sep, col_peak, col_rt, col_height, col_area)`

Parses a multi-sample UV-DAD peak table exported from Agilent MassHunter Quantitative Analysis. Returns a long-format data frame with one row per detected peak per sample, ready to pass to `align_peaks_by_rt()`.

**Parameters:**
- `file_path` - Path to the exported CSV or tab-delimited file
- `sep` - Field separator; `NULL` (default) auto-detects tab vs comma from the first non-blank line
- `col_peak`, `col_rt`, `col_height`, `col_area` - Column names in the export header row (defaults: `"Peak"`, `"RT"`, `"Height"`, `"Area"`)

**Output:** A long-format data frame:
- `sample` - Sample name (filename stem; path and `.d` suffix removed)
- `peak` - Peak index as assigned by MassHunter (integer)
- `rt` - Retention time (numeric, minutes)
- `height` - Peak height
- `area` - Peak area

**Notes:**
- Sample order in the output matches order of appearance in the file. The first sample is used as the reference for drift estimation by `align_peaks_by_rt()`, so file order matters.
- Column positions are read from the header row within each block, so the function is robust to exports with different column ordering or subsets.
- Sample names are extracted from the text after the `Ref=` setting on each block's header line (e.g. `Ref=off 10.PBDEAL_CTRL_RT_0M_2.d` → `10.PBDEAL_CTRL_RT_0M_2`); a reference wavelength such as `Ref=360,100` is handled in the same way.

---

#### `align_peaks_by_rt(peaks_long, max_drift, rt_tol, drift_model)`

Aligns peaks across samples by retention time in three steps:

1. **Drift correction** – each sample's retention-time drift relative to the reference sample (the first sample) is estimated and removed.
2. **Grouping** – the drift-corrected retention times of all samples are grouped into peak groups, with at most one peak per sample in a group. Peaks absent from the reference form groups in exactly the same way as those present in it.
3. **Flagging** – peaks whose assignment is a close call are flagged for manual review.

**Parameters:**
- `peaks_long` - Long-format data frame from `read_agilent_dad_peaks()` (columns `sample`, `peak`, `rt`, `height`, `area`; any further columns are carried through)
- `max_drift` - The largest RT shift (minutes) expected between a sample and the reference for the same compound (default: `0.5`). It limits which peak pairs are considered when estimating drift; it is not the width of a matching window. Check `result$drift_stats` to calibrate: if `max_abs_diff` is consistently under 0.1 min you can tighten it.
- `rt_tol` - The largest spread (minutes) of drift-corrected RTs allowed within one peak group. `NULL` (default) estimates it from the scatter left after drift correction; the value used is reported in a message and in `result$params`.
- `drift_model` - `"auto"` (default) lets drift vary along the run when a sample has enough anchor peaks (a straight line in RT with four or more, plus a smooth curve with ten or more) and otherwise uses a single offset; `"constant"` always uses a single offset per sample; `"none"` applies no drift correction.

**Output:** A named list with four elements:

`$aligned` — Long-format table with one row for every combination of peak group and sample:
- `sample`, `peak`, `rt`, `height`, `area` — as parsed (`NA` where the sample has no peak in the group)
- `peak_group` — group number, in elution order
- `ref_rt` — the group's RT on the reference sample's time scale: the reference sample's own peak RT if it has one in the group, otherwise the mean drift-corrected RT of the group
- `rt_corrected` — RT after removing the sample's drift
- `rt_diff` — raw `rt` minus `ref_rt` (i.e. including drift)
- `flag` — `"OK"`, `"AMBIGUOUS"`, or `"MISSING"`
- `flag_reason` — plain-English explanation for ambiguous rows

`$summary` — One row per peak group:
- `peak_group`, `ref_rt`, `in_reference` (whether the reference sample has a peak in the group)
- `mean_rt`, `sd_rt`, `cv_pct` — of the raw RTs of the `OK` peaks; equivalent to the average/SD/CV% columns in a manual Excel alignment
- `n_matched` (`OK` peaks), `n_ambiguous`, `n_missing` (samples with no peak in the group)

`$drift_stats` — One row per sample:
- `mean_rt_diff`, `sd_rt_diff`, `mean_abs_diff`, `max_abs_diff` — overall drift vs reference
- `n_matched`, `n_ambiguous`, `n_missing` — peak counts
- `n_anchors`, `drift_model` — how many anchor peaks the drift estimate rests on, and the model applied

`$params` — The settings used: `reference`, `max_drift`, `rt_tol`, `drift_model`.

**How it works:**
- *Drift estimation.* For each sample, every pairing of one of its peaks with a reference peak within `max_drift` is a candidate. The anchors are the longest chain of candidates that keeps the elution order in both samples and along which the RT difference changes smoothly; chance pairings would need a jump in drift and are passed over. A drift model is then fitted to the anchors.
- *Grouping.* The drift-corrected RTs are partitioned so that no group contains two peaks from the same sample or spans more than `rt_tol`; among all such partitions the one with the least within-group scatter (plus a penalty per group, so that peaks are not split needlessly) is found exactly. A sample containing two close peaks therefore forces them into two groups, which is what separates close pairs.

**Flags:**
- `AMBIGUOUS` — the peak could plausibly belong to a neighbouring group instead: its corrected RT is also within `rt_tol` of that group, the group has no peak from the same sample, and the peak is not clearly closer to the other members of its own group. Ambiguous peaks stay in their assigned group but are left out of the group's mean, SD and CV%. Manual review is recommended.
- `MISSING` — the sample has no peak in this group (the row is a placeholder with `NA` values).

**Limits:** retention time alone cannot separate two compounds whose spacing is similar to the random peak-to-peak scatter remaining after drift correction. Such regions are flagged rather than resolved and should be checked by hand (e.g. against spectra). Drift is estimated relative to the first sample, so a reference with few peaks gives few anchors.

**Example:**
```r
library(PenguinoidRTools)
library(tidyr)

peaks_long <- read_agilent_dad_peaks("polyphenols_peaktable.csv")
result     <- align_peaks_by_rt(peaks_long, max_drift = 0.25)

result$summary      # mean RT, SD, CV% per peak group
result$drift_stats  # per-sample drift vs reference — use to tune max_drift
result$aligned      # full table with flags for review
result$params       # settings used, including the estimated rt_tol

# Peaks to review by hand
subset(result$aligned, flag == "AMBIGUOUS")

# Pivot to wide format
area_wide <- pivot_wider(
  result$aligned,
  id_cols     = c(peak_group, ref_rt),
  names_from  = sample,
  values_from = area
)
```

---

## Dependencies

### For ANOVA functions
- agricolae (>= 1.3-7)
- dplyr (>= 1.1.4)
- gtools (>= 3.9.5)
- openxlsx (>= 4.2.5.2)

### For CIELab colour difference functions (`calc_colour_diff`, `calc_pairwise_dE`)
- dplyr (>= 1.1.4)
- farver (>= 2.1.0)

### For CIELab visualisation (`cielab_swatch`)
- ggplot2
- colorspace
- svglite (for SVG output only)

### For data processing (`process_spectrum`)
- Base R only (no additional packages required)

### For Agilent MassHunter peak table functions (`read_agilent_dad_peaks`, `align_peaks_by_rt`)
- Base R only for parsing and alignment
- tidyr (>= 1.3.0) recommended (optional) for pivoting `$aligned` to wide format

---

## OIV Method Reference

The CIELab functions implement the colour calculation method specified in:

**OIV-MA-AS2-11 (R2006)**: Determination of chromatic characteristics according to CIELab

The method uses:
- D65 standard illuminant
- 10 degree standard observer
- 5 nm wavelength intervals (380-780 nm)
- 10 mm standard path length (with Beer-Lambert correction for other path lengths)

---

## License

GPL-3.0
