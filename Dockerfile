FROM registry.gitlab.com/registercentrum/statistikenheten/r-docker-images/r42:main

WORKDIR /workspaces/

COPY . .

RUN Rscript -e 'pak::pak(c("devtools", "covr", "lintr", "DT", "htmltools"))' && \
    Rscript -e "pak::local_install_dev_deps(upgrade = FALSE)"
