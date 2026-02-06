# Wetland Patch Verification App

R Shiny application for validating raster patches created as training data for a wetland classification deep learning model. Each patch is displayed on a satellite imagery basemap using Leaflet, allowing reviewers to visually compare the classification labels against real-world imagery.

## Requirements

- R (>= 4.0)
- R packages: `shiny`, `leaflet`, `terra`, `sf`

Install the required packages:

```r
install.packages(c("shiny", "leaflet", "terra", "sf"))
```

## Data Setup

Place your GeoTIFF patch files in `Data/R_Patches/`. Files should follow the naming convention:

```
cluster_<NUM>_huc_<CODE>_patch_<NUM>.tif
```

Each raster should contain a `MOD_CLASS` band with wetland classification labels:

| Value | Code | Description           |
|-------|------|-----------------------|
| 0     | EMW  | Emergent Wetland      |
| 1     | FSW  | Forested Wetland      |
| 2     | OWW  | Open Water Wetland    |
| 3     | SSW  | Shrub-Scrub Wetland   |

## Launching the App

From the project directory, run one of the following:

**In R/RStudio:**

```r
shiny::runApp()
```

**From the terminal:**

```bash
Rscript -e "shiny::runApp()"
```

The app will open in your default browser.

## Usage

1. **Enter your name** in the Reviewer Name field at the top of the sidebar. This is required before you can submit reviews.
2. **Browse patches** using the Previous/Next buttons or jump ahead with "Jump to Next Unreviewed."
3. **Toggle the classification overlay** on/off to compare the `MOD_CLASS` labels against the satellite imagery.
4. **Filter by cluster or HUC** to focus on a specific subset of patches.
5. **Add optional comments** in the text box to note anything about the patch.
6. **Mark each patch as Valid or Invalid.** The app auto-saves and advances to the next patch.
7. **Export your review log** to CSV using the download button.

## Multi-Reviewer Workflow

Each reviewer gets their own log file, saved automatically as `Data/review_log_<name>.csv`. This avoids conflicts when multiple people review patches from a shared folder.

To collaborate:

1. Share the project folder (e.g., via a network drive, OneDrive, or Google Drive).
2. Each reviewer runs the app locally and enters their own name.
3. Review logs are saved separately and can be merged later.

To start a fresh review, simply delete your `review_log_<name>.csv` file from the `Data/` directory. The app will create a new one automatically.

## Review Log Format

The exported CSV contains the following columns:

| Column       | Description                          |
|--------------|--------------------------------------|
| `patch_file` | Filename of the reviewed patch       |
| `cluster`    | Cluster number                       |
| `huc`        | HUC code                             |
| `patch_num`  | Patch number within the cluster/HUC  |
| `status`     | `valid` or `invalid`                 |
| `comment`    | Optional reviewer comments           |
| `reviewer`   | Name of the reviewer                 |
| `timestamp`  | Date and time of the review          |
