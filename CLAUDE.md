# Wetland Patch Verification App

R Shiny application for validating deep learning training data. Displays small raster patches on a satellite imagery basemap using Leaflet.

## Purpose

Validate raster patches created as training data for a wetland classification deep learning model. Users can view patches overlaid on satellite imagery and toggle visibility.

## Data

- **Location**: `Data/R_Patches/`
- **Format**: GeoTIFF rasters with 21 bands
- **Label Band**: `MOD_CLASS` contains wetland classification labels

### MOD_CLASS Values

| Value | Code | Description |
|-------|------|-------------|
| 0 | EMW | Emergent Wetland |
| 1 | FSW | Forested Wetland |
| 2 | OWW | Open Water Wetland |
| 3 | SSW | Shrub-Scrub Wetland |

## Workflow

1. App loads one patch at a time
2. Patch displays on satellite basemap (ESRI or Google imagery)
3. User toggles MOD_CLASS layer on/off to compare against imagery
4. User marks patch as **valid** or **invalid**
5. Review status is tracked and can be exported to CSV

## Features

- Single patch view with navigation (next/previous)
- Toggle classification overlay visibility
- Valid/Invalid buttons for each patch
- Track review progress (reviewed vs. pending)
- Export review log to CSV

## Tech Stack

- R Shiny
- Leaflet (for map display and satellite basemap)
- terra or raster package (for reading GeoTIFFs)
- ESRI World Imagery or Google Satellite as basemap
