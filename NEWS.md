# dsROCrate 0.1.0

## New Features

* Added `check_permissions()` support for backend connections, allowing users to verify whether a connection has sufficient privileges to run `safe_*` auditing functions before execution (#15).
* Permission validation now provides actionable feedback, including guidance on how to configure Opal users and roles when required permissions are missing (#15).
* Added Opal version validation during backend checks to ensure compatibility with APIs required for auditing and permission verification workflows (#22).

## Improvements

* Refactored backend validation into an extensible S3 framework, providing a cleaner architecture for supporting additional backends in future releases (#23).
* Introduced a unified backend validation workflow that standardises connection, version and permission checks across auditing functions (#23).
* Replaced use of `opalr::opal.datasources()` with `opalr::opal.projects()`, reducing the permissions required for metadata extraction and improving compatibility with auditor accounts (#21).
* Updated auditing workflows to support Opal's **Audit System** role, reducing the need for administrator-level accounts when generating RO-Crates and audit reports (#20).

## Bug Fixes

* Fixed generation of orphaned permission entities in RO-Crates by excluding administrator and auditor permissions from dataset permission records where corresponding user entities are not included (#19).
* Improved consistency between `safe_data()` and `safe_people()` permission handling (#19).

## Documentation

* Updated deployment vignette to reference the FederatedMethods-maintained DataSHIELD Docker deployment resources and repository structure (#16).
* Updated user documentation to reflect support for Opal's **Audit System** role and revised permission requirements (#20).
* Reviewed documentation for internal helper functions and adjusted visibility of internal APIs where appropriate (#17).
* Added documentation and examples for backend permission validation workflows (#15).

## Internal Changes

* Improved separation of concerns between connection validation, backend capability checks and permission validation (#22, #23).
* Refined internal validation infrastructure to simplify future backend integrations.
* General maintenance, testing and documentation updates.

# dsROCrate 0.0.2

* This patch addresses an issue with the vignettes. The `safe_output.opal()` S3
generic now uses `overwrite = TRUE` to update the root (`./`) entity.

# dsROCrate 0.0.1

* Initial CRAN submission.
* This version contains standard functions for auditing (`audit()`), reporting
(`report()`) and extracting five safe principle components (`safe_*()`).
* This version currently only supports OBiBa's Opal as the backend.
