FROM rocker/shiny:4.3.2

WORKDIR /srv/shiny-server/lume
COPY . /srv/shiny-server/lume

RUN R -e "install.packages(c('renv'), repos='https://cloud.r-project.org')" && \
    R -e "source('packages.R')"

EXPOSE 3838
ENV LUME_LOG_DIR=/srv/shiny-server/lume/logs
ENV LUME_LOG_FILE=/srv/shiny-server/lume/logs/app.log
ENV LUME_ENABLE_AUTO_ERROR_REPORT=false

CMD ["/usr/bin/shiny-server"]
