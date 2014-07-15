\name{cip}
\docType{data}
\alias{cip}
\title{CIP Indicators data}
\description{%
  Dataframe containing the sub-indicators for the CIP construction.
  
 Manufacturing Value Added per capita (MVApc): captures the level of industrialization of a country, and is expressed per capita to adjust for country size. 
  %The MVApc is the relative value of the total net manufacturing output to the population size. Unlike gross output, MVA is free of double counting as the cost of intermediate consumption is excluded and, also, it is measured at basic prices (no tax distortion). Provided data availability, instead of assuming the total population as producer of the total net manufacturing output it would be more accurate to consider the MVA in relation to the total number of employees or, even better, the total number of hours worked (see also UNIDO, 2010:217). 
  %If industry were fully exposed to international competition, then MVApc alone would capture not only the 'production efficiency' element but also the 'competitive' one. As domestic industries are not fully (and equally) exposed to international competition, the export performance must be factored in. 
  (Data source: UNIDO National Accounts database and World Development Indicators).

Manufacturing Export per capita (MXpc): captures the ability of a country to produce goods competitively, and implicitly to keep up with technological changes. Like the MVA, MX is expressed per capita to adjust for country size. 
  Data on manufactured exports indicate prima facie international efficiency and show structural trends. 
  However, data on the MXpc of large economies are biased by the existence of large internal demand and incentives/biases towards domestic markets. 
  (Data source for MXpc: UN Comtrade database). 
  
Share of MVA in GDP (MVAsh), which captures manufacturing's weight in the economy. 
  
Share of Medium and High-Tech Activities in total MVA (MHVAsh): captures the technological complexity of manufacturing. Medium tech and high tech activities are grouped. 
  For MHVAsh the OECD classification is used. 
  (Data sources: INDSTAT 4 and WDI databases)

Share of Medium and High-Tech Products in Manufactured Exports (MHXsh): captures the technological content and complexity of exports. Medium tech and high tech activities are grouped. 
  For MHXsh the OECD classification is used. 
  
Impact of a country on world MVA (ImWMVA) measured by its share in world MVA, which indicates a country's relative performance and impact in manufacturing.
  
Impact of a country on world Manufactures Trade (ImWMT) measured by its share in world manufactured exports. The latter shows a country's competitive position relative to others in international markets. That is, gains in world market shares reflect more competitiveness; losses signal deterioration.
}
\usage{data(cip)}
\format{Dataframe of size (7399 x  11). First column contains country code, second column contains year, remaining columns contain 
sub-indicator values.}
\source{UNIDO}
\keyword{datasets}




