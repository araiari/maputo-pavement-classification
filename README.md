# Monitoring road infrastructure from satellite images in Greater Maputo

Repository for the paper: "Monitoring road infrastructure from satellite images in Greater Maputo"
Published in Statistical Methods and Applications – DOI: 10.1007/s10260-024-00772-y

*Authors*: Arianna Burzacchi, Matteo Landrò, Simone Vantini


## Overview

Road infrastructure conditions significantly impact transport operations, yet information on pavement surface types is rarely available in developing countries. This project addresses this gap by developing an automatic classification method using:

- **Google Earth satellite images** of road networks
- **OpenStreetMap (OSM)** road network data
- **DBSCAN clustering** for image segmentation
- **Object-oriented k-NN classification** algorithm

The approach was validated on the Greater Maputo area in Mozambique, achieving:
- **88.1%** overall accuracy using satellite images only
- **94.5%** accuracy when including street type information


## Repository structure
```
maputo-road-classification/
├── README.md
├── LICENSE
├── data/             # Input data (NOT included)
│   ├── raster/
│   └── shapefile/
├── processed-data/   # Intermediate output (NOT included)
│   ├── cpp-elaboration/
├── scripts/          # All R scripts used in the analysis
│   ├── utils/        # Utility functions
│   ├── .R
├── renv/             # Virtual R environment managed by renv(NOT included)
├── renv.lock         # Snapshot of all required R packages
├── .Rprofile         # Activates renv on project startup
└── .gitignore
```

## Requirements

### Software Dependencies
```r
R >= 4.0.5
RStudio >= 1.4.1106
QGIS >= 3.16.3
```

## How to run

### Initialization

1. **Clone the repository** from bash:
   ```bash
   git clone https://github.com/araiari/maputo-pavement-classification.git
   cd maputo-pavement-classification
   ```

2. **Install R dependencies** from R:
   ```r
   install.packages("renv")   # if not already installed
   renv::restore()
   ```


## 📚 Citation

If you use this code in your research, please cite:

```bibtex
@misc{burzacchi2025maputo,
  author       = {Andrea Burzacchi},
  title        = {maputo-road-classidication},
  year         = {2025},
  howpublished = {\url{https://github.com/araiari/maputo-road-classidication}},
  note         = {Reference paper: Monitoring road infrastructure from satellite images in Greater Maputo},
  url          = {\url{https://github.com/araiari/maputo-road-classidication}}
}
```

