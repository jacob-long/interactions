## Test environments
* Local macOS (aarch64), R 4.5.1
* Linux and Windows (R-hub), R 4.6.2 and R-devel, with the `jtools` 2.3.2
  and `panelr` 1.1.0 candidates for integration checks

## R CMD check results
R-hub: 0 ERRORs | 0 WARNINGs | 0 NOTEs. The Windows checker reported an INFO
for the `quantreg::rq` help link because `quantreg` was not installed.

Local checks: 0 ERRORs | 0 WARNINGs; environment-only NOTEs for unavailable
clock verification and outdated HTML Tidy. A separate URL check returned HTTP
403 for two vignette citations; both DOIs were verified with Crossref.

## Reverse dependencies
`bruceR` 2026.1 passed with both `interactions` 1.2.0 and 1.3.0.
