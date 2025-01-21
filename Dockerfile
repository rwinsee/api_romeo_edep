# Étape 1 : Utiliser une image de base R avec Shiny
FROM inseefrlab/onyxia-rstudio:r4.3.2-2024.02.13

# Étape 2 : Installer les dépendances nécessaires
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Étape 3 : Copier l'application dans le conteneur
COPY app.R /srv/shiny-server/
COPY scripts/fonction_fetchAppelations.R /srv/shiny-server/scripts/
COPY scripts/fonction_fetchAppelationsContexte.R /srv/shiny-server/scripts/
COPY scripts/fonction_getFichesMetier.R /srv/shiny-server/scripts/
COPY scripts/fonction_getAccessToken.R /srv/shiny-server/scripts/
COPY scripts/fonction_loadNAF.R /srv/shiny-server/scripts/
COPY scripts/fonction_loadNAF_normalisee.R /srv/shiny-server/scripts/
COPY scripts/fetch_predictions_with_context_normalisationNAF.R.R /srv/shiny-server/scripts/

# Étape 4 : Installer les packages R nécessaires
RUN R -e "install.packages(c('shiny', 'DT', 'httr', 'jsonlite', 'shinyjs'), repos='http://cran.rstudio.com/')"

# Étape 5 : Configurer les permissions
RUN chown -R shiny:shiny /srv/shiny-server

# Étape 6 : Exposer port where shiny app will broadcast
ARG SHINY_PORT=3838
EXPOSE $SHINY_PORT
RUN echo "local({options(shiny.port = ${SHINY_PORT}, shiny.host = '0.0.0.0')})" >> /usr/local/lib/R/etc/Rprofile.site

# Étape 7 : Démarrer Shiny Server
CMD ["Rscript", "-e", "shiny::runApp('/srv/shiny-server')"]
