name: Auto-Download Malawi UV Data

on:
  schedule:
    - cron: '30 * * * *' 
  workflow_dispatch:

jobs:
  run-script:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout repository
        uses: actions/checkout@v3

      - name: Setup R
        uses: r-lib/actions/setup-r@v2
        with:
          r-version: 'release'

      - name: Install System Rules
        run: |
          sudo apt-get update
          sudo apt-get install -y libcurl4-openssl-dev libssl-dev libxml2-dev

      - name: Install R Packages with pak
        run: |
          Rscript -e 'install.packages("pak", repos="https://cloud.r-project.org/")'
          Rscript -e 'pak::pkg_install(c("ecmwfr", "googledrive"))'

      - name: Run Script
        env:
          CDS_API_KEY: ${{ secrets.CDS_API_KEY }}
          GDRIVE_PERSONAL: ${{ secrets.GDRIVE_PERSONAL }}
        run: |
          mkdir -p gdrive_token
          echo "$GDRIVE_PERSONAL" | base64 --decode > gdrive_token/c13dc354db9600c8cd9b2bd868d1bf25_jacobn158@gmail.com
          Rscript download_era5_malawi.R
