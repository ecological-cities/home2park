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

In the wake of the COVID-19 pandemic, significant changes to human mobility and working environments have prompted a re-think of land use distribution in the city, particularly for office, residential and public spaces such as parks. One prominent trend is a rising demand for the outdoors, as urban dwellers turn to local parks for recreation and leisure [@Venter2020]. However, current metrics of park provision tend to focus on summaries of park area within a given region [@Tan2017]. There is a need to characterise the wide variety of parks that serve different groups of people [@Song2020a], and to deal with the limitations of artificial boundaries when planning at fine spatial scales.

The package ``home2park`` provides a way to measure multiple aspects of park provision to homes, at the resolution of individual buildings. The key features include the ability to:  

1.	Download relevant data from OpenStreetMap (OSM) from any city worldwide (e.g. buildings, parks and features related to recreation), using the R package ``osmextract`` [@Lovelace2019]. The user may also supply data for new buildings and parks, for the purpose of future scenario planning. 
2.	Summarise at each park multiple attributes that are important for recreation (e.g. dense vegetation, length of waterfronts, open spaces, trails, etc.).
3.	Calculate the supply (provision) of the park attributes to residential buildings while accounting for ‘distance decay’ [@Rossi2015; @Tu2020b], or the fact that supply from parks further away are reduced.
4.	Redistribute coarse-scale population data per census unit into residential buildings, also known as ‘dasymetric mapping’ [@Dmowska], which helps highlight specific areas where more people will benefit from the presence of parks.

See the [package website](https://ecological-cities.github.io/home2park/) for detailed documentation and examples of how regional summaries derived from building-level metrics vary widely from conventional metrics of park area provision.

# Statement of Need

Numerous metrics have been used to measure the spatial provision of parks in cities. These include summaries of the per capita park area (i.e. park provision ratio) or ‘service radius’ (a distance buffer) around parks within geographical areal units such as census blocks or administrative boundaries [@Tan2017]. Such metrics have been used to investigate park use and accessibility, as well as social-cultural issues related to spatial equity and environmental justice [@Sister2010; @Tan2017]. However, the presence of artificial boundaries gives rise to the ‘modifiable areal unit problem’ , where differences in spatial scale can result in considerable variation in the value of summarised metrics [@Sister2010]. Since urban development typically occurs at fine spatial scales, metrics at the scale of individual buildings provided by ``home2park`` can help overcome such limitations [@Gao2017]. Finally, most metrics of park provision do not differentiate between the wide variety of parks that would likely affect usage by different groups of people. Other than park area, features such as waterfronts, open lawns, natural vegetation, trails and fitness amenities each relate to different types of park use and user groups [@Song2020a; @Song2020b]. A more holistic measure of park provision should thus include a variety of park attributes that are important for recreation and leisure. ``home2park`` enables the user to summarise a variety of spatial attributes such as points (e.g. playgrounds, sports and fitness amenities), lines (e.g. cycling and walking trails) and rasters (e.g. land cover types) at each park. Customisable parameters provide the user with flexibility when summarising specific attributes, such as specifying a minimum patch size for a land cover class, or including spatial features within a certain distance beyond park boundaries. By providing fine-grained measures for a variety of park attributes, we hope that ``home2park`` will contribute toward a more nuanced understanding of recreation that will improve the planning and design of parks and homes across the city.

## State of the Field

To our knowledge, there are currently no software (e.g. R packages) that measure the spatial provision of parks and recreation. 


# Acknowledgements

We thank Edwin Y. W. Tan and Justin K. H. Nai for useful discussions on the methodology. 

# References
