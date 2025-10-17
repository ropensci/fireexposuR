---
title: 'fireexposuR: An R package for computing and visualizing wildfire exposure'
tags:
- R
- wildfire
- Wildland fire
- risk assessment
date: "21 May 2025"
output:
  html_document:
    df_print: paged
  pdf_document: default
authors:
- name: Air M. Forbes
  orcid: "0000-0002-9842-7648"
  affiliation: 1
- name: Jennifer L. Beverly
  orcid: "0000-0001-8033-9247"
  affiliation: 1
bibliography: paper.bib
affiliations:
- name: Department of Renewable Resources, University of Alberta, Edmonton, AB T6G
    2H1, Canada
  index: 1
---

# Summary

`fireexposuR` (v1.1.0) is an R package that automates wildfire exposure 
methodologies presented in several scientific publications 
[@beverly:2010; @beverly:2021; @beverly:2023], providing an accessible and 
adaptable platform to a broad user base. Wildfire exposure is a numeric rating 
of potential wildfire transmission to, or from, a location based on the 
composition and configuration of the surrounding wildland fuels (i.e., 
flammable vegetation). Exposure assessments can be applied at multiple spatial 
scales (e.g., neighbourhood, region, country) to inform a wide range of 
management objectives and decisions (e.g., fuel management priorities, 
evacuation planning, land-use planning, habitat conservation, firefighting 
resource optimization); and are particularly well-suited for scenario and 
contingency planning. `fireexposuR` has functions for computational analysis 
and generating visualizations of results in summary tables, plots, and maps. 

# Background

Wildfire exposure assessments were originally introduced to assess fire entry 
points to the built environment [@beverly:2010]. The method is promoted 
nationally by FireSmart Canada as a tool for community protection planning 
[@firesmart:2018]. The approach was later adapted for landscape scale 
assessments by @beverly:2021, which included a performance validation 
confirming real wildfires burn preferentially in high exposure areas. Exposure 
assessments are omnidirectional but can be summarized locally to assess the 
directional vulnerability of a given location to potential fire encroachment 
using radial graphs developed by @beverly:2023. Outputs of wildfire exposure 
and directional vulnerability assessments are typically well received and 
easily understood by diverse audiences due to the intuitive visualizations. The 
methods and data requirements are also comparatively simpler than alternative 
wildfire risk models, making them an ideal option when constrained by budget 
or time. These methods were developed in Alberta, Canada; but have since been 
adapted successfully to other geographic areas. @schmidt:2024 customized 
exposure assessments for use in Alaska, USA, @khan:2025 have applied them in 
Portugal, and a publication is currently in preparation that applies and 
validates the exposure metric across the entire Canadian land base 
[@beverly:2025]. Research is also underway to adapt the methods for use in the 
United Kingdom. The exposure metric has been used as an input to evacuation 
planning [@kim:2024], fuel treatment planning [@karimi:2024], and to assess 
telecommunication vulnerability [@kuiper:2025].

# Statement of need

Broad distribution and uptake of exposure and directional vulnerability 
assessments has been constrained by the technical nature of the original 
documentation within scientific publications, some with access fees; and the 
need for proficiency in, and access to, geographic information system (GIS) 
software. The published methods were described in relation to Esri Inc. ArcGIS 
software and geoprocessing tools which has a subscription-based model 
that can be financially prohibitive to some users. 

Conceptually, the methodologies are relatively simple, but they involve a 
detailed sequence of data processing steps that can intimidate new users, 
especially those lacking foundational knowledge in GIS and spatial analysis. 
New users may introduce unintentional errors, by misinterpreting processing 
steps or input data requirements and can lack confidence in their understanding 
of the methods and their process for executing the analysis independently. 
The objectives of `fireexposuR` are to reduce barriers to access and give users 
structured support to independently apply the methods with confidence. The 
analysis functions were developed and vetted in collaboration with the primary 
author of the wildfire exposure publications upon which `fireexposuR` is based. 
Custom parameterization options have been designed and described for 
adaptability within the suggested scope. In addition, a validation function has 
been included to confirm any custom decisions in alternative use cases or 
geographic areas. The package was developed in the R environment [@R:2024], 
an open-source platform with no financial fees. Package documentation is 
presented in plain language to ensure access to a non-technical audience. 
Access to the package requires basic proficiency in the R environment, which 
could present a barrier to some users; however, abundant online resources and 
tutorials are available to anyone willing to learn. 

# State of the field

At the time of writing, `fireexposuR` is the only fully open-source software 
option for computing and visualizing wildfire exposure. A custom Python toolbox
has also been developed to replicate the wildfire exposure methods in 
@schmidt:2024, and is freely available on 
[GitHub](https://github.com/gina-alaska/wildfire-exposure-toolbox?tab=readme-ov-file#readme) 
(v1.0.5; @gina:2024) but requires an ArcGIS license. The toolbox guides users 
through the steps of defining a hazardous fuel raster, computing ember exposure 
for fixed distances, and other additional steps specific to the methods 
presented in @schmidt:2024. In comparison, `fireexposuR` includes 
transmission distance customization, directional vulnerability assessments, and 
rapid visualization of results with sensible defaults.

# Target audience

The target audience for `fireexposuR` includes researchers, agencies, 
practitioners, and individuals who are interested in conducting wildfire 
exposure assessments. Package functions are highly customizable, for ease of 
application in new geographic areas or use cases.  


# Typical workflow

The workflow for conducting wildfire exposure assessments using the functions 
within the `fireexposuR` package is demonstrated in \autoref{fig:one}. Input 
data requirements are detailed in the function documentation. 

![Figure 1. Suggested workflow of core `fireexposuR` functionality. 
Computational outputs, outlined in navy, can be further manipulated in R, 
visualized with `fireexposuR` functions, or 
exported. \label{fig:one}](flowchart.png)

# Acknowledgements

The `fireexposuR` project was funded by the National Research Council of Canada,
the Forest Resource Improvement Association of Alberta, and Housing, 
Infrastructure and Communities Canada. We thank ROpenSci reviewers Ronny 
Hernandez Mora ([\@ronnyhdez](https://github.com/ronnyhdez)) and Sherry Zhang 
([\@huizezhang-sherry](https://github.com/huizezhang-sherry)) for their 
helpful feedback, which significantly improved the code and documentation in 
this R package. We also thank the many early users of the package for providing 
feedback during initial development. 

# References
