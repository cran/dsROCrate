# dsROCrate (development version)

# dsROCrate 0.0.2

* This patch addresses an issue with the vignettes. The `safe_output.opal()` S3
generic now uses `overwrite = TRUE` to update the root (`./`) entity.

# dsROCrate 0.0.1

* Initial CRAN submission.
* This version contains standard functions for auditing (`audit()`), reporting
(`report()`) and extracting five safe principle components (`safe_*()`).
* This version currently only supports OBiBa's Opal as the backend.
