# Build PNADc Panel

This function builds a panel dataset from PNADC data, identifying
households and individuals.

## Usage

``` r
build_pnadc_panel(dat, panel)
```

## Arguments

- dat:

  Data frame with PNADC data, sorted into a single panel.

- panel:

  A `character` with the type of panel identification. Use "none" for no
  paneling, "basic" for basic paneling, "advanced_1" for advanced stage
  1 paneling, "advanced_2" for advanced stage 2 paneling, and
  "advanced_3" for the fuzzy-matching stage 3 paneling.

## Value

A modified dataset with added identifiers for household (`id_dom`) and
individual (`id_ind`, and progressively `id_rs1`, `id_rs2`, or `id_rs3`)
based on the chosen panel algorithm.

## Examples

``` r
if (FALSE) { # interactive()
# Example usage:

panel_data <- build_pnadc_panel(dat = pnad_sample, panel = "advanced_3")
}
```
