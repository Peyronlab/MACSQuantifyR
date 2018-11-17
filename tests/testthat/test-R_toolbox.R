# Tests for functions in toolbox script
set.seed(42)

# load a minimal example data set (subset of nbt dataset)
filepath <- system.file("extdata", "drugs.xlsx",
    package = "MACSQuantifyR")
load_MACSQuant(filepath)


# Tests for coordinates correspondances to well names
# --------------------------------------------------------------------------------

# context("coordinates to well names")
