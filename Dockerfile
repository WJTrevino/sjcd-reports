FROM opencpu/rstudio

RUN useradd --create-home --shell /bin/bash sjcd
WORKDIR /home/sjcd
# COPY . .

EXPOSE 8004