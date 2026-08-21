# dsROCrate 0.2.2

## Bug fixes

* Fixed an issue in `safe_output()` where Opal audit logs containing no `AGGREGATE` events could cause the audit pipeline to fail when constructing the calls table.
* `safe_output()` now handles audit sessions with logs but no `AGGREGATE` calls gracefully, returning an empty calls table rather than attempting to operate on `NULL`.
* Fixed an issue where an RO-Crate without an explicit Opal profile attribute could cause safe_output() to fail when filtering audit logs. The default "default" Opal profile is now used when no profile is supplied.
* Improved robustness of provenance extraction for audit sessions with incomplete or limited audit activity.

# dsROCrate 0.2.1

## Bug Fixes

* Removed a duplicate, conflicting definition of `as.data.frame.safe_call()` (two files each defined this S3 method with different output columns; only one could ever take effect, silently). The `safe-call.R` version — timestamp, user, r_cmd, fx, args, session, profile — is now the sole implementation.
* Fixed `print.safe_symbol()` referencing stale `column`/`parent` fields left over from before the 0.2.0 symbol-model refactor; it now correctly displays the symbol's `asset`, `expr`, and `session` (guarding on both `NULL` and `NA`, since these fields default to `NA_character_`).
* Fixed `as.data.frame.symbol_registry()`, which previously iterated over the columns of the underlying tibble instead of its rows (via `lapply()` on a data frame), causing it to error whenever called on a non-empty registry. It also referenced non-existent `parent`/`column` fields. The method now simply returns `x$symbols` coerced to a data frame, which already carries the correct columns.
* Fixed `register_symbol()` erroring on a brand-new, empty registry (the very first symbol registered in any session): the emptiness check compared `nrow()` of a plain, pre-tibble `list()` before checking `length()`, producing a zero-length logical passed to `||`. The `length()` check is now evaluated first, so it short-circuits correctly.
* Fixed `as.data.frame.safe_call()`'s `args` column having an unstable name and shape depending on how many arguments the underlying call had (a single-argument call collapsed the column into one named after that argument, e.g. `x`; multiple arguments spread into several columns instead of one). The argument list is now wrapped with `I(list(...))` so it always appears as a single, reliably-named `args` list-column.
* Added regression tests for `safe_call()`/`as.data.frame.safe_call()`, `print.safe_symbol()`, and `register_symbol()`/`as.data.frame.symbol_registry()`, none of which previously had any test coverage.

## Internal Changes

* Removed unused internal helpers: `has_symbol()`/`has_symbol.symbol_registry()` (and its S3 export) and `safe_symbol_reference()`/`new_safe_symbol_reference()`, none of which were called anywhere in the package.
* Removed leftover commented-out code from `safe_symbol()` and `print.safe_call()`.
* Added roxygen2 documentation (`@param`, `@returns`, `@keywords internal`, `@noRd`) and explanatory comments to the internal symbol-tracking functions introduced in 0.2.0 (`safe_call()`, `safe_symbol()`, `safe_reference()`, `symbol_registry()`, and their supporting utilities), with no change in behaviour.
* Updated spell-check `WORDLIST`.
* Re-rendered vignettes to refresh example output.

# dsROCrate 0.2.0

## Breaking Changes

* The audit log tibble returned via `safe_output()` no longer includes a `table` column. It has been replaced by three more granular columns: `kind` (whether the tracked object is a `table`, `resource` or `expression`), `asset` (the resolved table/resource name) and `expr` (the associated R expression, when applicable). Code that reads the `table` column directly will need to be updated.

## New Features

* Added a symbol registry and call-tracking system (`safe_symbol()`, `safe_call()`, `safe_reference()`, `symbol_registry()`) that replaces regex-based parsing of Opal logs, modelling assignments and aggregate function calls as objects and resolving arguments back to the symbols/tables they reference.
* Added a generic backend abstraction layer (`backend_logs()`, `backend_options()`, `backend_users()`, `backend_projects()`, `backend_sys_perms()`, and others) so internal code no longer calls `opalr::` directly, paving the way for additional backends.
* Added early Armadillo backend scaffolding: `check_permissions()`, `init()`, `report()`, `safe_data()`, `safe_output()`, `safe_people()` and `safe_setting()` now all have `ArmadilloCredentials` methods, returning clear "not yet implemented" errors ahead of full support.
* Added `default` S3 methods for `audit()`, `audit_engine()` and `check_permissions()`, giving clearer errors when an unsupported connection object is supplied.
* Added a `verbose` argument to `check_permissions()` (opal method) to optionally print a success message when permissions are sufficient.
* Added `print.safe_call()` and `print.safe_symbol()` methods, plus `as.data.frame()` methods for `safe_call` and `symbol_registry` objects, for readable inspection of the new internal tracking objects.

## Improvements

* `safe_people()` now determines admin/auditor status from actual system permissions and group membership, rather than a hardcoded name check against `"admin"`/`"administrator"`, improving compatibility with deployments that use differently named privileged accounts.
* Permission lookup failures during `safe_people()` now fall back gracefully to an empty result instead of aborting the whole audit.
* Consolidated backend-version and connection-validation logic into the new generic backend framework, reducing duplication between Opal-specific code paths.

## Documentation

* Removed a vignette section demonstrating direct calls to internal helper functions, which have since been renamed as part of the backend abstraction work.
* Updated documentation to reflect that `init()`, `safe_data()`, `report()` and related functions now accept either `opal` or `ArmadilloCredentials` connections.

## Internal Changes

* Added test coverage for `audit()`, `audit_engine()` and `check_permissions()`, along with a mock-connection test helper, reducing reliance on a live demo Opal server.
* Added `uuid` to `Imports`, used to generate unique IDs for tracked symbols.
* Bumped the `testthat` version requirement in `Suggests` from `>= 3.0.0` to `>= 3.1.4`.

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
