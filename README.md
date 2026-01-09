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
- `path_mm` - Cuvette path length in mm (default: 10)

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

**Delta E interpretation:**
- 0-1: Not perceptible by human eye
- 1-2: Perceptible through close observation
- 2-10: Perceptible at a glance
- 11-49: Colours are more similar than opposite
- 100: Colours are exact opposites

**Example:**
```r
deltaE76(50, 10, 20, 55, 15, 25)
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

## Dependencies

### For ANOVA functions
- agricolae (>= 1.3-7)
- dplyr (>= 1.1.4)
- gtools (>= 3.9.5)
- openxlsx (>= 4.2.5.2)

### For CIELab visualisation (`cielab_swatch`)
- ggplot2
- colorspace
- svglite (for SVG output only)

### For data processing (`process_spectrum`)
- Base R only (no additional packages required)

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
