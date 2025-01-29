# Tree diversity increases forest temperature buffering via enhancing canopy density and structural diversity

**Authors:**

Florian Schnabel*, Rémy Beugnon*, Bo Yang*, Ronny Richter, Nico Eisenhauer, Yuanyuan Huang, Xiaojuan Liu, Christian Wirth, Simone Cesarz, Andreas Fichtner, Maria D. Perles-Garcia, Georg J. A. Hähn, Werner Härdtle, Matthias Kunz, Nadia C. Castro Izaguirre, Pascal A. Niklaus, Goddert von Oheimb, Bernhard Schmid, Stefan Trogisch, Manfred Wendisch, Keping Ma†, Helge Bruelheide†

**Journal:** Ecology Letters

**Article DOI:** TBA

**Data DOI:** 10.5281/zenodo.13626945

## Folder description

This folder contains all scripts and data used to build the models and figures of our paper.

*Analysis scripts:*

- Fig1.R: script compiling the data, testing the effects of tree species richness effects on microclimate temperature on the daily and the monthly scale, and preparing Figure 1

- Fig2.R: script compiling the data, testing the effects of tree species richness effects on microclimate temperature buffering on the monthly and yearly scales, and preparing Figure 2

- Fig3.R: script compiling the data, testing the Structural Equation Models (SEMs) examining potential mediators of tree species richness effects on monthly temperature buffering, and preparing Figure 3

- mod-day-pred.RDS and hourly-model.csv: pre-fitted daily models 

*Datasets:*

- inventory_data.csv: forest inventory measurement 2015 - 2020
Columns:
  "Site_plot": measurement site and plot identifier
  "year mean height": yearly mean tree height
  "year mean crown base height": yearly mean tree crown base height
  "year crown length": yearly mean crown length
  "year basal area": yearly mean basal area

- LAI.csv: Leaf Area Index measurements.
Columns:
  "site": measurement site
  "site.plot": measurement site and plot identifier
  "time": sampling time
  "LAI": LAI measure

- macroclimate.csv: macroclimate estimation at landscape level from CRU
Columns:
  "year": measurement year
  "month": measurement month
  "tmp_celsius": monthly mean temperature
  "tmn_celsius": monthly minimum temperature
  "tmx_celsius": monthly maximum temperature

- spei.csv: macroclimate SPEI estimation at landscape level from CRU
Columns:
  "year": measurement year
  "SPEI12": 12 months SPEI estimation
  
- spei1.csv: macroclimate SPEI estimation at landscape level from CRU
Columns:
  "year": measurement year
  "month": measurment month
  "spei": 1 months SPEI estimation

- microclimate_data.csv.zip: 1 m air temperature measurements
Columns:
  "site": site identifier
  "plot": plot identifier
  "TreeDiv": tree species richness
  "year": measurement year
  "month": measurement month
  "day": measurement day
  "hour": measurement hour
  "T": temperature measurement

- plots_Site_A.csv: Site A planting design and plot details
Columns:
  "PLOT_NO": plot identifier
  "TREE_R": tree species richness
  "TREE_CMP": tree species composition
  "ALTITUDE": plot altitude
  "EASTNESS": plot eastness
  "NORTHNESS": plot northness
  "SLOPE": plot slope
  "CURV_PR" & "CURV_PL": plot curvature

- SSCI_Data_2019.csv: Terrestrial laser scanning site A measurements
Columns:
  "PLOT_NO": plot identifier
  "MeanFrac": average number of layer
  "ENL": effective number of layers
  "SSCI": plot structural complementarity



  
  
