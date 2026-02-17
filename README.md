# Wetland Patch Verification App

R Shiny application for validating training data for a wetland classification deep learning model. Each patch is displayed on a satellite imagery basemap using Leaflet. Reviewers can compare raster classification labels and editable vector polygons against real-world imagery, record a confidence score, and flag altered polygons for downstream correction.

## Requirements

- R (>= 4.0)
- R packages: `shiny`, `shinyjs`, `leaflet`, `leafpm`, `terra`, `sf`, `stringr`

Install the required packages:

```r
install.packages(c("shiny", "shinyjs", "leaflet", "leafpm", "terra", "sf", "stringr"))
```

## Data Setup

Two patch directories are required:

- **`Data/R_Patches_Labels/`** — GeoTIFF rasters (21 bands) with the naming convention:
  ```
  *_cluster_<NUM>_huc_<CODE>_patch_<NUM>.tif
  ```
- **`Data/R_Patches_Vector/`** — GeoPackage polygon files (editable) with the naming convention:
  ```
  NHP_cluster_<NUM>_huc_<CODE>_patch_<NUM>.gpkg
  ```

The app pairs patches by matching the `cluster`, `huc`, and `patch` numbers across both directories. Any vector patch without a corresponding raster (or vice versa) is skipped. The raster prefix before `_cluster_` (e.g. `labels_only_NHP_`, `labels_only_NWI_`) is ignored during matching.

Each raster should contain a `MOD_CLASS` band with wetland classification labels:

| Value | Code | Description           |
|-------|------|-----------------------|
| 0     | EMW  | Emergent Wetland      |
| 1     | FSW  | Forested Wetland      |
| 2     | OWW  | Open Water Wetland    |
| 3     | SSW  | Shrub-Scrub Wetland   |
| 4     | UPL  | Upland                |

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

1. **Enter your name or initials** in the Reviewer Name field at the top of the sidebar. This is required before you can submit reviews. **Use the same name every session** — it determines your log filename and allows the app to resume where you left off.
2. **Browse patches** using the Previous/Next buttons or jump ahead with "Jump to Next Unreviewed."
3. **Toggle overlays** — use the raster overlay checkbox to show/hide the `MOD_CLASS` classification layer and adjust its opacity; the vector overlay checkbox shows/hides the editable polygon layer.
4. **Edit vector polygons** directly on the map using the Leaflet.PM toolbar (edit vertices, drag, delete, or draw new polygons). Any edits are flagged automatically.
5. **Filter by cluster or HUC** to focus on a specific subset of patches.
6. **Select a confidence score (1–10)** to indicate how confident you are in the patch. The Submit button is disabled until a score is chosen.
7. **Add optional comments** in the text box.
8. **Click Submit.** The app logs your review, saves an edited copy of the vector to `Data/Altered_R_Patches_Vector/` if you made changes, and advances to the next patch.
9. **Export your review log** to CSV using the download button.

## Multi-Reviewer Workflow

Each reviewer gets their own log file, saved automatically as `Data/review_log_<name>.csv`. This avoids conflicts when multiple people review the same patches.

### Getting Started (Reviewers)

1. **Clone the repository:**

   ```bash
   git clone <repo-url>
   cd shiny_wetland_verify
   ```

2. **Install R packages** (one-time setup):

   ```r
   install.packages(c("shiny", "shinyjs", "leaflet", "leafpm", "terra", "sf", "stringr"))
   ```

3. **Run the app:**

   ```r
   shiny::runApp()
   ```

4. **Enter your name** in the Reviewer Name field. Use a consistent, unique name (e.g., your initials) — this determines your log filename. **Important:** Use the exact same name every session. If your name changes (e.g., "Alice" vs "alice"), the app will create a separate log file and your previous progress won't carry over.

5. **Review patches.** The app auto-saves your progress to `Data/review_log_<name>.csv` after each submission.

### Submitting Your Reviews

When you've finished a batch of reviews, push your log file back to the repository:

```bash
git add Data/review_log_<name>.csv
git commit -m "Add review log for <name>"
git push
```

### Pulling Updates

If new patches are added or you want to see others' progress:

```bash
git pull
```

Since each reviewer has a separate CSV file, there should be no merge conflicts.

### Resuming Work

The app automatically detects your existing review log when you enter your name and jumps to the first unreviewed patch. Just `git pull` to get any new patches, then run the app.

### Starting Fresh

To start a fresh review, delete your `review_log_<name>.csv` file from the `Data/` directory. The app will create a new one automatically.

## Review Log Format

The exported CSV contains the following columns:

| Column               | Description                                              |
|----------------------|----------------------------------------------------------|
| `patch_file_vector`  | Filename of the reviewed vector patch                    |
| `cluster`            | Cluster number                                           |
| `huc`                | HUC code                                                 |
| `patch_num`          | Patch number within the cluster/HUC                      |
| `status`             | `reviewed` or `pending`                                  |
| `altered`            | `TRUE` if the reviewer edited the vector polygons        |
| `confidence`         | Reviewer confidence score (1–10)                         |
| `comment`            | Optional reviewer comments                               |
| `reviewer`           | Name of the reviewer                                     |
| `timestamp`          | Date and time of the review                              |
