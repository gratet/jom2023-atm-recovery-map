FROM rocker/geospatial:4.0.2

# Complements to rocker/geospatial
RUN install2.r --error --repos 'https://cran.rstudio.com/' \
	ggspatial

# Tables
#RUN install2.r --error --repos 'https://cran.rstudio.com/' \
#	tables \
#	table1 \
#	furniture \
#	arsenal \
#	kableExtra

# Visualisations
RUN install2.r --error --repos 'https://cran.rstudio.com/' \
#	ggthemes \
#	ggpubr \
#	patchwork \
	cowplot \
#	ggforce \
	ggrepel \
#	sparkline \
#	DiagrammeR \
	biscale
	
# Imputation on missing values
#RUN install2.r --error --repos 'https://cran.rstudio.com/' \
#	missCompare \
#	mice \
#	VIM

