# PenguinoidUtils



* *aovSummaryTable*: This function takes a dataframe and returns a summary
  table of key ANOVA results without an interaction model.
* *aovInteractSummaryTable*: This function takes a dataframe and returns a
  summary table of key ANOVA results for a two, three, or four way interaction model.



\# CIELab Colour Functions



R functions for calculating CIELab colour coordinates from spectrophotometry data using the OIV-MA-AS2-11 (R2006) method with D65 illuminant and 10 degree standard observer.



\### Data Conversion



\#### `cielab\_spectroscopy\_percentage\_to\_fraction(transmission\_pct)`



Converts spectrophotometry transmission data from percentage (0-100) to fraction (0-1) format.



```r

\# Convert 50% transmission to fraction

cielab\_spectroscopy\_percentage\_to\_fraction(50)

\# Returns: 0.5



\# Convert a vector of percentages

cielab\_spectroscopy\_percentage\_to\_fraction(c(10, 50, 90))

\# Returns: c(0.1, 0.5, 0.9)

```



\### Core CIELab Conversion



\#### `cielab\_from\_spectrum(data, sample\_col = NULL, path\_mm = 10)`



Converts spectrophotometry transmission data to CIELab colour values using the OIV method. \*\*Automatically detects data format\*\* - just pass your dataframe and get CIELab values back.



\*\*Input:\*\* A data.frame in either format:



1\. \*\*Wide format\*\* (typical from spectrophotometers):

&nbsp;  - Sample identifier column (e.g., `WineName`, `Sample`)

&nbsp;  - Wavelength columns named `X780`, `X779`, ... `X380`

&nbsp;  - Values as transmission fraction (0-1)



2\. \*\*Long format\*\*:

&nbsp;  - Sample identifier column (e.g., `WineName`)

&nbsp;  - `lambda` - Wavelength in nm

&nbsp;  - `T` - Transmission as fraction (0-1)



\*\*Parameters:\*\*

\- `data` - Your spectroscopy dataframe (wide or long format)

\- `sample\_col` - Column to group by. \*\*This controls averaging:\*\*

&nbsp; - `"WineName"` (default) - One result per wine, averages replicates

&nbsp; - `"Replicate"` - One result per replicate, no averaging

\- `path\_mm` - Cuvette path length in mm (default: 10)



\*\*Output:\*\* A data.frame with one row per unique value of `sample\_col`:

\- `<sample\_col>` - Your sample identifier

\- `CIELab\_L` - L\* lightness (0-100)

\- `CIELab\_a` - a\* green-red axis

\- `CIELab\_b` - b\* blue-yellow axis

\- `CIELab\_C` - Chroma (saturation)

\- `CIELab\_H` - Hue angle in degrees (0-360)



\*\*Note:\*\* Transmission values slightly > 1 (e.g., 1.003) are normal instrument noise and ignored. Only values > 1.1 trigger a warning.



```r

\# Wide format - just pass your data directly!

\# spectroscopy\_data has columns: WineName, EtOH, Replicate, X780, X779, ... X380

result <- cielab\_from\_spectrum(spectroscopy\_data)

\# Returns 2 rows (one per WineName), replicates averaged



\# Get individual results per replicate (no averaging)

result <- cielab\_from\_spectrum(spectroscopy\_data, sample\_col = "Replicate")

\# Returns 6 rows (one per Replicate)



\# Long format works too

result <- cielab\_from\_spectrum(spectroscopy\_long)



\# Use different path length

result <- cielab\_from\_spectrum(my\_data, path\_mm = 5)

```



\### Colour Metrics



\#### `lab\_to\_C(a, b)`



Calculate chroma from CIELab a\* and b\* values.



```r

lab\_to\_C(30, 40)  # Returns 50

```



\#### `hue\_deg(a, b)`



Calculate hue angle in degrees from CIELab a\* and b\* values.



```r

hue\_deg(1, 1)   # Returns 45

hue\_deg(-1, 1)  # Returns 135

```



\#### `delta\_h\_signed(h2, h1)`



Calculate signed hue difference, correctly handling wraparound at 360 degrees.



```r

delta\_h\_signed(10, 350)   # Returns 20 (crossing 0)

delta\_h\_signed(350, 10)   # Returns -20

```



\#### `deltaE76(L1, a1, b1, L2, a2, b2)`



Calculate CIE76 colour difference (Delta E) between two colours.



Delta E interpretation:

\- 0-1: Not perceptible by human eye

\- 1-2: Perceptible through close observation

\- 2-10: Perceptible at a glance

\- 11-49: Colours are more similar than opposite

\- 100: Colours are exact opposites



```r

deltaE76(50, 10, 20, 55, 15, 25)

```



\### Visualisation



\#### `cielab\_swatch(CIELab, file, ...)`



Create and save colour swatch visualisations to various formats (PNG, PDF, SVG, etc.).



\*\*Input:\*\* A data.frame with columns:

\- `WineName` - Labels for each colour swatch

\- `CIELab\_L` - L\* values (0-100)

\- `CIELab\_a` - a\* values

\- `CIELab\_b` - b\* values



\*\*Optional parameters:\*\*

\- `file` - Output filename (extension determines format)

\- `chips\_per\_row` - Swatches per row (NULL = auto)

\- `cm\_per\_chip` - Size of each tile in cm

\- `border\_col` - Border colour

\- `font\_size\_pt` - Font size in points

\- `dpi` - Resolution for raster formats



```r

df <- data.frame(

&nbsp; WineName = c("Red", "White", "Rose"),

&nbsp; CIELab\_L = c(30, 90, 70),

&nbsp; CIELab\_a = c(50, -5, 20),

&nbsp; CIELab\_b = c(30, 10, 15)

)

cielab\_swatch(df, "swatches.png")

cielab\_swatch(df, "swatches.pdf")

cielab\_swatch(df, "swatches.svg")

```



\### Data Processing



\#### `process\_spectrum(folder\_path, ...)`



Process a folder of spectrophotometer CSV files into a combined data frame.



\*\*Parameters:\*\*

\- `folder\_path` - Path to folder containing CSV files

\- `skip` - Header rows to skip (default 13)

\- `wavelength\_col` - Wavelength column name (default "WL.nm.")

\- `transmission\_col` - Transmission column name (default "X.T")

\- `wl\_min`, `wl\_max` - Wavelength range (default 300-800)



\*\*Output:\*\* A data.frame with columns:

\- `sample\_name` - Sample identifier from filename

\- `lambda` - Wavelength in nm

\- `T` - Transmission as fraction (0-1)



```r

\# Process all CSV files in a folder

spectra <- process\_spectrum("path/to/spectrophotometer/data")



\# Then calculate CIELab - just pass the result directly!

result <- cielab\_from\_spectrum(spectra)

```



\## Dependencies



\### Required

\- R (>= 3.5.0)



\### For visualisation (`cielab\_swatch`)

\- ggplot2

\- colorspace

\- svglite (for SVG output only)



\### For data processing (`process\_spectrum`)

\- Base R only (no additional packages required)



\## OIV Method Reference



These functions implement the colour calculation method specified in:



\*\*OIV-MA-AS2-11 (R2006)\*\*: Determination of chromatic characteristics according to CIELab



The method uses:

\- D65 standard illuminant

\- 10 degree standard observer

\- 5 nm wavelength intervals (380-780 nm)

\- 10 mm standard path length (with Beer-Lambert correction for other path lengths)



