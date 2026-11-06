## Release scope

Version 1.3.0 adds component-aware panel-model plots and contrast-based
simple slopes/Johnson-Neyman inference through S3 methods. It also fixes
refitting with weights, offsets, and subsets for ordinary model workflows.
It requires jtools 2.3.2; the optional panel interface requires panelr 1.1.0.

## Test environments

* Local macOS (aarch64), R 4.5.1, ggplot2 4.0.3, lme4 2.1-0.
* Targeted tests also pass with ggplot2 3.4.4, reformulas 0.4.2,
  lme4 1.1-38, and cowplot 1.1.3 (336 assertions, no warnings or skips).
* R-hub Windows and Linux, R-release (R 4.6.2) and R-devel (Windows r90836,
  Linux clang r90827), all pass with the coordinated candidate dependencies.

## R CMD check results

Final local `--as-cran`, including incoming checks, vignettes and PDF manual:

0 ERRORs | 0 WARNINGs | 1 NOTE

The NOTE is an environment limitation: HTML Tidy is too old to validate
the HTML manual. CRAN incoming feasibility passes. This is a
release-preparation report, not a submission.

The standard packaged suite passes 360 assertions. Since `--as-cran`
excludes Enhances libraries, a separate installed-package run verifies the
optional panelr integration: 394 assertions pass, with no failures or
warnings and only visual tests skipped. Ordinary-model plots, slopes, and
Johnson-Neyman checks also pass with panelr physically absent.

## Visual and downstream checks

The full visual suite passes 463 assertions. Paired baseline/candidate
review under identical dependencies found 64 of 67 legacy SVGs identical;
the other three render pixel-identically. All refreshed snapshots match
those reviewed renders. New panel axes, moderator values, and inference
captions were reviewed separately.

bruceR 2026.1 passes local baseline/candidate package checks with no errors,
warnings, or notes. These run examples, tests, and vignette R code, without
building PDF manuals or rebuilding vignettes.

## Hosted integration checks

All four R-hub platform checks finish with Status: OK (no errors, warnings,
or notes). Each suite passes 396 assertions with no test warnings and 54
visual/optional Bayesian-package skips. No panelr integration tests are skipped.
Windows has an informational Rd cross-reference message because quantreg is
unavailable on those runners.

The hosted archive uses test-only dependency metadata: pinned jtools 2.3.2
and panelr 1.1.0 snapshots, with panelr in Suggests rather than Enhances to
exercise its integration during --as-cran checks. All other source files are
identical to the original uploaded release candidate. Subsequent release-note
edits do not change executable code. Those metadata changes are not for CRAN.
Windows Builder R-release/R-devel uploads are accepted; reports are pending.
