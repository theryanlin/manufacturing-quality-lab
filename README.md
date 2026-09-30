# 製程品質實驗室 (Manufacturing Quality Lab)

A synthetic-data R Shiny teaching app for manufacturing quality decisions: observe, predict, and plan validation experiments using MES × equipment × QMS data. All data is synthetic — this is a learning project, not real factory data.

## What's in this folder

- `app.R` — the Shiny app (Traditional Chinese UI)
- `R/` — data joins, logistic regression model, heatmap stats, improvement search
- `data/` — synthetic MES / equipment / QMS CSVs (18,000 products, 90,000 equipment-stage rows)
- `tests.R` — data / model / decision checks (run with `Rscript tests.R`)
- `preview.html` — self-contained interactive preview, no R needed
- `README-project.md` — the project's original readme (the three training exercises, model assumptions, limitations)
- `VALIDATION.md` — the project's own validation notes

## Verified working (2026-09-29)

- `Rscript tests.R` passes all 3 scenario checks (R 4.3.3, shiny 1.8.0)
- The Shiny app launches and serves HTTP 200 on `127.0.0.1:3838`
- Rendered numbers match the documented expectations (e.g. in the material scenario, stage 3 / machine B ≈ 28.1%, other machines ≈ 9.4%)
- Saved as a web artifact: **Manufacturing Quality Lab Preview** (in the Library — open it anytime, no R needed)

## How to run it yourself

1. Install R with the shiny package. On Ubuntu: `apt-get install -y --no-install-recommends r-cran-shiny`
   (If `apt-get update` stalls, the `mirror.cogentco.com` entry in `/etc/apt/sources.list.d/ubuntu.sources` was dead on this machine and had to be removed.)
2. From this folder, run the checks: `Rscript tests.R`
3. Launch the app: `LANG=C.UTF-8 R -e 'shiny::runApp(".", port=3838, launch.browser=FALSE)'`
   then open `http://127.0.0.1:3838`. The `LANG=C.UTF-8` part matters — without a UTF-8 locale the Chinese text breaks.

## The three scenarios

1. **Machine effect** — one machine at one stage truly runs worse.
2. **Material confounding** — a bad material batch (M03) is routed mostly through machine B, so B looks guilty.
3. **Machine × material interaction** — extra risk only when stage-3 machine B meets M03.

Each scenario has 6,000 products over 30 days. The quality model is logistic regression trained on days 1–20 and tested on days 21–30, and the improvement search tries 81 temperature/speed combinations under defect-risk and throughput constraints.
