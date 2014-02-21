Introduction
===============

The Townsend Material Deprivation score was originally devised by Townsend et al
(1988). It is based on the following four Census variables:

	1. Percent of households without access to a car.
	2. Percent of households with more than one person per room.
	3. Percent of households not owner-occupied.
	4. Percent of individuals who are economically active unemployed.

The four domains above are then combined to produce the overall Townsend Score.
This score can be calculated for any geographical area that is available in the
Census tables, for example LSOA, MSOA, Ward, LAD, etc.

Because of the way the score is combined, only the relative rank of the scores
is meaningful. In practice, this means it is possible to say an area is more or
less deprived than another, but not what ‘amount’ of deprivation it experiences.
For this reason it is common to create a score based on a national dataset. This
script uses data from England and Wales, as Scotland and Northern Ireland tables
were not available at the time of publication (February 2014). Thus, the scores
reflect the relative deprivation based on England and Wales.

Purpose
===============

This R script is to prepare the Census data:
	1. Load in Census tables and convert them in to usable form.
	2. Convert the four domains in to z-scores so they can be combined.
	3. Combine the z-scores to form one overall index.
	4. Rank the scores and produce deciles.

Using the Script
================

Copy the script (source.R) to your system.

You need four data tables, one for each domain of deprivation, at the geographical level you’re interested in (LSOA, Ward, etc.). They’ll probably 
all be called ‘bulk.csv’ when you download them, so rename these:

1. car.csv
2. oc.csv (overcrowd)
3. tenure.csv
4. unemp.csv

I’ve provided four example files from Nomis Web at the LAD (district/unitary) 
level, which you can use for testing or if this is the geography you want. Additional tables are available from: http://www.nomisweb.co.uk/census/2011/data_finder

Place the four data files in the same folder as source.R.

Run the script file. This should produce one file - master.csv - which contains geography code, overall Townsend Score (z-score), and decile of deprivation for each geography in England and Wales. With the geography code this can then be imported in to GIS software and joined to appropriate Census boundary files.

License
===============

The code is GPL v3 license. See LICENSE.txt.
The data (available from Nomis Web) is from the UK Census and is Crown Copyright. See census-copyright.txt

Contact
===============
phil dot jones at sheffield dot ac dot uk (no spaces)

Thanks and acknowledgements
===========================

Thanks to Robin Lovelace - https://github.com/Robinlovelace - for having
a once-over of the script and his suggestions.

References
===============

Office for National Statistics, 2011 Census: Aggregate data (England and Wales) [computer file]. Downloaded from: http://www.nomisweb.co.uk/census/2011 [Accessed February 2014]. This information is licensed under the terms of the Open Government Licence [http://www.nationalarchives.gov.uk/doc/open-government-licence/version/2].

Office for National Statistics, 2011 Census: Digitised Boundary Data (England and Wales) [computer file]. UK Data Service Census Support. Downloaded from: http://edina.ac.uk/census

Townsend, Peter, Phillimore, Peter and Beattie, Alastair (1988) 'Health and Deprivation: Inequality and the North'. London: Croom Helm