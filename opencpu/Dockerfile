FROM opencpu/rstudio

RUN ["apt-get","update","-y"]
RUN ["apt-get","install","libcurl4-openssl-dev","libssl-dev","libxml2-dev","-y"]
RUN ["apt-get","install","r-cran-tidyverse","-y"]

RUN useradd --create-home --shell /bin/bash sjcd
COPY . /home/sjcd/
# RUN ["/usr/bin/R","-e","utils::install.packages(\"tidyverse\")"]
RUN ["/usr/bin/R","-e","utils::install.packages(\"/home/sjcd/\", type=\"source\", repos = NULL)"]
# commonmark > roxygen2 > usethis > devrdevtools