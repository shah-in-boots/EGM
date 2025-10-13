# EGM (development version)

* Updates to `{pkgdown}` settings for documentation organization
* Debugging the color issues when adding a color theme to `ggm()`
* Creation of subclass of `egm` objects called `ecg` to identify 12-lead ECG subtypes

# EGM 0.1.1

This is an updated release to *CRAN* for this package.
The changes to this version are as follows.

* Update to `ggm()` function to allow for appropriate theming for dark and light color themes (removes issues with importing colors)

* Update to code and testing suite to assess compatibility with the `{ggplot2}` __v4.0.0__ release

* Addition of a series of functions for F wave extraction and analysis

* Addition of rudimentary functions for windowing signal based on surface ECG

* New function to read in Prucka (CardioLab) for EP study recordings with the `read_prucka()` function

# EGM 0.1.0

This is the first *CRAN* release for this package. 
The initial version contains key features for working with intracardiac electrograms (EGM) and surface electrocardiograms (ECG), visualizing signals, and working with annotations stored in the `WFDB` format.
A single, major class is introduced here.

- `egm` objects contain signal data and meta-data with specific dispatch methods, and are composed of three internal classes
	- a `signal_table` that contains the raw signal data
	- a `header_table` that contains meta-data about the signal data
	- an `annotation_table` that identifies samples and labels them with specific annotations

Additional I/O features are introduced to work with data stored in the `WFDB` format:

- `read_wfdb()` reads in `WFDB` data and returns an `egm` object
- `write_wfdb()` writes an `egm` object to a `WFDB`-compatible format

This first version also allows working with ECG and EGM data using:

- `read_muse()` for ECG data (GE MUSE, v9)
- `read_lspro()` for EGM data (LabSystem Pro)

	
