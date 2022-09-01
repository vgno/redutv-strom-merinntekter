# redutv-strom-merinntekter

Dette repoet inneholder VGs løpende beregning av statens merinntekter fra vannkraften, basert på forutsetninger fra Energi Norge.

Vi bruker løpende data om kraftproduksjon og spotpriser fra Nord Pool. Disse er ikke vedlagt, men eksempler på struktur ligger i `./examples`.

Spørsmål? Ta kontakt med jari@vg.no.

## Kjør med Docker

Sett miljøvariabler

    API_PRICES_DAILY="https://..."
    API_PROD_DAILY="https://..."
    API_PROD_HOURLY="https://..."
    API_PRICES_HOURLY="https://..."

og så:

    make docker-run

## Deploy

    DOCKER_REGISTRY="..." make deploy