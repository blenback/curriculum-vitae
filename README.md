<!-- badges: start -->
[![License](https://img.shields.io/github/license/blenback/curriculum-vitae)](LICENSE)
[![Deploy CV to Pages](https://github.com/blenback/curriculum-vitae/actions/workflows/pages.yml/badge.svg)](https://github.com/blenback/curriculum-vitae/actions/workflows/pages.yml)
<!-- badges: end -->

This is the repository for the CV of Ben Black adapted from the repository of [Mickaël Canouil](https://github.com/mcanouil).

## Themes

The CV styling follows the Ben Black personal brand (warm, earthy palette;
Satoshi headings + Spectral body). Three render-time themes are available:

| Theme    | Look                                                                 |
|----------|----------------------------------------------------------------------|
| `warm`   | **(default)** Cream paper + warm sand sidebar, forest-green section titles, burnt-orange dates/dots. |
| `forest` | Cream main column + deep forest-green sidebar with cream/tan text (mirrors the website footer). Bold, high-contrast. |
| `subtle` | Near-white paper, light sidebar, brand fonts + forest/burnt accents only. Least ink. |

Each theme is a token override stylesheet in [`themes/`](themes/) layered on top
of the structural styles + brand tokens in [`cv.css`](cv.css).

**Select a theme** by setting `theme:` in [`config.yaml`](config.yaml):

```yaml
theme: warm   # one of: warm | forest | subtle
```

…or override at render time without editing the file via the `CV_THEME`
environment variable (takes precedence over `config.yaml`):

```sh
CV_THEME=forest Rscript -e 'rmarkdown::render("curriculum-vitae.Rmd")'
```

