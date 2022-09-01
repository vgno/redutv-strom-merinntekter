FROM rocker/r-ver:4.2.1

RUN apt-get update -qq
RUN apt-get install -y --no-install-recommends \
    libxml2-dev \
    libcairo2-dev \
    libgit2-dev \
    libsasl2-dev \
    libsqlite3-dev \
    libssh2-1-dev \
    libcurl4-openssl-dev \
    ca-certificates \
    lsb-release \
    libclang-dev \
    libssl-dev \
    psmisc \
    procps \
    python-setuptools \
    sudo \
    wget


RUN rm -rf /var/lib/apt/lists/*
RUN rm -rf /tmp/downloaded_packages

WORKDIR /app
RUN Rscript -e "install.packages(c('renv'))"

ADD renv.lock renv.lock

ENV RENV_PATHS_LIBRARY renv/library

RUN R -e "renv::restore()"
RUN R -e "renv::clean()"

ADD R R
ADD run.R run.R

CMD ["Rscript", "run.R"]
