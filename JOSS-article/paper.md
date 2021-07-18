---
title: 'home2park: An R package to assess the spatial provision of urban parks'
tags:
  - R
  - Distance decay
  - Dasymetric map
  - Outdoor recreation
  - Park provision
  - Recreation supply
  - Service area
  - Spatial distribution
  - Urban green space
  - Urban parks
authors:
  - name: Xiao Ping Song
    orcid: 0000-0002-8825-195X
    affiliation: 1
  - name: Kwek Yan Chong
    orcid: 0000-0003-4754-8957
    affiliation: 1
affiliations:
 - name: Department of Biological Sciences, National University of Singapore
   index: 1
citation_author: Song and Chong
date: 11 Jul 2021
year: 2021
bibliography: paper.bib
output: rticles::joss_article
csl: apa.csl
journal: JOSS
---

# Summary

In the wake of the COVID-19 pandemic, significant changes to human mobility and working environments have prompted a re-think of land use distribution in the city, particularly for office, residential and public spaces such as parks. One prominent trend is a rising demand for the outdoors, as urban dwellers turn to local parks for recreation and leisure [@Venter2020]. However, current metrics of park provision tend to focus on summaries of park area within a given region [@Tan2017]. There is a need to characterise the wide variety of parks that serve different groups of people [@Song2020a; @Song2020b], and to deal with the limitations of artificial boundaries when planning at fine spatial scales.

# Statement of Need

``home2park`` is an R package that provides a way to measure multiple aspects of park provision to homes, at the resolution of individual buildings. Coarse-scale population data is re-distributed into residential buildings using ‘dasymetric mapping’ [@Dmowska], which helps highlight important areas where more people will benefit from the presence of parks. Multiple attributes important for recreation (e.g. dense vegetation, length of waterfronts, open spaces, trails, etc.) may be summarised at each park, and the supply of each attribute to residential buildings can be calculated while accounting for ‘distance decay’, or the fact that supply from parks further away are reduced [@Rossi2015; @Tu2020b]. If user data are unavailable, ``home2park`` downloads and processes relevant data from OpenStreetMap using the R package ``osmextract`` [@Lovelace2019]. The user may also supply data for new buildings and parks, for the purpose of future scenario planning. By providing fine-grained measures for a variety of park attributes, we hope that ``home2park`` will contribute toward a more nuanced understanding of recreation that will improve the planning and design of parks and homes across the city.

# Acknowledgements

We thank Edwin Y. W. Tan and Justin K. H. Nai for useful discussions on the methodology. 

# References
