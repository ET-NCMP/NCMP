## Synopsis

National Climate Monitoring Products are simple summaries of the weather and climate conditions in a particular country. The WMO comission for climatology Expert Team on National Climate Monitoring Products (ET-NCMP) has defined a small set of standard NCMPs and the code in this repository was written to calculate the NCMPs in a standard way.

## Code Example

Please read the documentation (R_NCMPs_User_Manual.docx)

## Motivation

The main aim of the project is to make some simple tools that any National Met Service can pick up and use to calculate NCMPs for their country or region on a regular basis. These tools will make it easier for Met Services and their staff to do routine climate monitoring, particularly in cases where the service has few resources to develop or purchase their own software. It is hoped that this will help a more diverse range of local experts contribute to national, regional and global climate assessments, including the WMO annual statement on the status of the global climate, the Bulletin of the American Meteorological Society State of the Climate reports and the IPCC assessments.

The main output of the code is the NCMPs, which are simple summaries of how the climate has changed at a national scale over time. However, there is potential to output addition useful products. Some of these are currently produced in the code but are not easily accessible to the user. For example, the code calculates a range of common climate indices at a station level as well as interpolated maps of those station-based indices. Other products could be produced with small modifications. For example, outputting regional (as opposed to national) averages, sector specific indices.

The Expert Team on NCMPs was initially formed to specify a short list of NCMPs that could be produced consistently and easily by National Hydrological or Meteorological Services around the world. The ET-NCMP has written general guidance that specifies what the 6 NCMPs should be. The software in this repository is a specific implementation of the general guidance produced by the team. The preliminary list of 6 NCMPs

1. Mean temperature anomaly averaged over the country
2. Total precipitation as a percentage of the normal, averaged across the country
3. Standardised precipitation index, averaged across the country
4. Number of days in the month when Tmax exceeded the 90th percentile, averaged across the country
5. Number of days in the month when Tmin was below the 10th percentile, averaged across the country
6. Count of stations which broke high maximum and low minimum temperature records and daily precipitation records


## Installation

Please read the documentation (R_NCMPs_User_Manual.docx)

## Contributors

The code was originally developed by Lucie Vincent and Megan Hartwell from the Climate Research Division of Environment and Climate Change Canada. The ET-NCMP would like to thank Simon Grainger and James Adams for their important contributions to the development and testing of the computer routines. More information on the work of the Expert Team ET-NCMP can be found at their web page http://www.metoffice.gov.uk/hadobs/opace2_tt_ncmp/

## License


