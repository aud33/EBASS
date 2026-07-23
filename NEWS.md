# EBASS 0.1.1

- **Title**: The length of the title in `DESCRIPTION` has been reduced to less than 65 characters to comply with CRAN requirements.
- **Description / References**: References describing the methods developed in the package have been added to the `Description` field in `DESCRIPTION`.
- **Documentation**:
  - Documentation for exported functions has been clarified (roxygen2).
  - The description of function return values has been explicitly documented
- **Messages**:
  - Calls to `print()` and `cat()` used to display objects or messages have been replaced with `message()`  where appropriate, to follow best practices and CRAN recommendations.
- **License**:
  - The `LICENSE` file has been removed.
  - The reference to `LICENSE` in the `License` field of `DESCRIPTION` has been removed
  

- Resubmission after CRAN removal; issues fixed in this version.
- Corrected the note about vignette generation.
- Updated roxygen2 version from 5.0.1 to 7.3.3.
- Added `Encoding: UTF-8` required by roxygen2.
- Improved function documentation by using roxygen.

# EBASS 0.1.0
- initial CRAN submission