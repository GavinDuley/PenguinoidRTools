# Column names referenced via non-standard evaluation inside dplyr verbs and
# ggplot2 aesthetics. Declaring them here silences the spurious
# "no visible binding for global variable" notes from R CMD check.
utils::globalVariables(c(
  # cielab_from_spectrum / calc_colour_diff / cielab_kinetics centroids
  "CIELab_L", "CIELab_a", "CIELab_b",
  "L", "a", "b", "C", "h",
  "L_ref", "a_ref", "b_ref", "C_ref", "h_ref",
  # calc_pairwise_dE crossed columns
  "CIELab_L_ref", "CIELab_a_ref", "CIELab_b_ref",
  "CIELab_L_target", "CIELab_a_target", "CIELab_b_target",
  "dE",
  # cielab_swatch plotting aesthetics
  "hex", "label", "label_col"
))
