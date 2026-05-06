<p align="center">
  <h1 align="center">◆ Lume Data Analytics</h1>
  <p align="center">A modular, no-code data visualization workbench built with R and Shiny</p>
</p>

<p align="center">
  <img src="https://img.shields.io/badge/R-4.3%2B-276DC3?logo=r&logoColor=white" />
  <img src="https://img.shields.io/badge/Shiny-1.8%2B-4479A1?logo=rstudio&logoColor=white" />
  <img src="https://img.shields.io/badge/Bootstrap-5-7952B3?logo=bootstrap&logoColor=white" />
  <img src="https://img.shields.io/badge/Plotly-Interactive-3F4F75?logo=plotly&logoColor=white" />
  <img src="https://img.shields.io/badge/license-MIT-green" />
</p>

---

## What is Lume?

Lume is an open-source, browser-based analytics platform that lets you go from raw CSV/XLSX/TSV files to interactive, shareable dashboards — **without writing a single line of code**.

It's built entirely in **R + Shiny** using the modern `bslib` layout framework, and ships with:

- **Smart Upload** — Drag-and-drop file ingestion with automatic type detection, date parsing, encoding fallback, and large-dataset sampling.
- **Visual Transformation Builder** — Point-and-click filtering, column operations, aggregation, pivoting, missing-value handling, text cleaning, and calculated columns — all tracked as a replayable pipeline.
- **10+ Chart Types** — Bar, line, scatter, box, histogram, heatmap, and pie/donut charts powered by Plotly with full appearance and label customization.
- **Dashboard Builder** — Drag-and-drop canvas with KPI metric cards, annotation blocks, and chart references. Export to HTML, PDF, CSV, PNG, or a reproducible R script.
- **AI Assistant** — Optional LLM-powered insights via OpenAI, Gemini, or Groq. Generates dataset summaries, transformation suggestions, and executive dashboard notes (nothing is auto-applied).
- **Auto Insights** — Instant KPI extraction, narrated data profiling, distribution/trend/correlation charts generated on upload.

---

## Project Structure

```
lume/
├── app.R                  # Main entry point (UI + server wiring)
├── config.R               # Theme, env vars, global constants
├── packages.R             # Dependency management (auto-install)
├── modules/
│   ├── upload_module.R    # File upload, validation, preview
│   ├── insights_module.R  # Auto-generated KPIs and charts
│   ├── transform_module.R # Transformation pipeline builder
│   ├── viz_module.R       # Interactive chart builder (Plotly)
│   └── dashboard_module.R # Dashboard canvas, export, share
├── utils/
│   ├── ai_helper.R        # Multi-provider LLM integration
│   ├── error_handler.R    # Structured error handling
│   ├── helpers.R          # Misc utility functions
│   └── validators.R       # Input/file validation
├── www/
│   ├── custom.css          # Design system styles
│   └── styles.css          # Supplemental overrides
├── data/
│   └── sample_sales.csv   # Built-in demo dataset
├── logs/                  # Runtime logs (app.log)
├── tests/                 # Test suite
├── docs/                  # Documentation
├── Dockerfile             # Container deployment
├── .env.example           # Environment variable template
└── .gitignore
```

---

## Getting Started

### Prerequisites

| Requirement | Version |
|---|---|
| **R** | 4.3+ |
| **RStudio** (optional) | 2024+ |
| **System libraries** | `libcurl`, `libxml2`, `openssl` (for `httr` / `readxl`) |

### 1. Clone the repository

```bash
git clone https://github.com/Darkcoder-droid/lume.git
cd lume
```

### 2. Configure environment

```bash
cp .env.example .env
```

Edit `.env` with your preferred AI provider and API key (optional — the app works fully without AI):

```env
LUME_AI_PROVIDER=groq          # openai | gemini | groq
LUME_GROQ_API_KEY=your_key     # only if using Groq
LUME_OPENAI_API_KEY=            # only if using OpenAI
LUME_GEMINI_API_KEY=            # only if using Gemini
```

### 3. Install dependencies & run

```r
# From R console or RStudio:
source("packages.R")   # Auto-installs missing packages
shiny::runApp(".", port = 3838, launch.browser = TRUE)
```

Or from the terminal:

```bash
Rscript -e "source('packages.R'); shiny::runApp('.', port=3838, launch.browser=TRUE)"
```

The app will open at **http://localhost:3838**.

### 4. Docker (optional)

```bash
docker build -t lume .
docker run -p 3838:3838 --env-file .env lume
```

---

## Usage

### Upload Data
Navigate to **Overview → Data Source** and drag-drop a CSV, TSV, XLSX, or XLS file (up to 50 MB). A built-in sample dataset is preloaded for quick exploration.

### Transform
Go to **Workspace → Transformations** to build a multi-step pipeline:
- Filter rows with multi-condition logic (AND/OR)
- Select, rename, or retype columns
- Create calculated columns (arithmetic or text concatenation)
- Aggregate with group-by and pivot (wide ↔ long)
- Clean missing values, duplicates, and text

### Visualize
Switch to **Workspace → Visualization** to build interactive charts:
- Choose from templates (distribution, trend, comparison, correlation) or go custom
- Map X, Y, color, size, and facet dimensions
- Fine-tune palettes, scales, grid lines, legend position, and labels
- Add charts to a grid view

### Build Dashboards
Open **Dashboard Builder** to compose a shareable dashboard:
- Add chart references, KPI metric cards, and markdown annotations
- Rearrange cards via drag-and-drop
- Export as interactive HTML, PDF snapshot, CSV data, PNG chart, or R script

### AI Features (optional)
When an API key is configured, AI-powered features activate:
- **Upload summary** — Natural-language dataset read on ingestion
- **Transformation suggestions** — AI proposes a cleaning/enrichment pipeline
- **Dashboard notes** — Executive commentary generated from current data + card layout

---

## Environment Variables

| Variable | Default | Description |
|---|---|---|
| `LUME_AI_PROVIDER` | `groq` | AI backend: `openai`, `gemini`, or `groq` |
| `LUME_OPENAI_API_KEY` | — | OpenAI API key |
| `LUME_OPENAI_MODEL` | `gpt-4o-mini` | OpenAI model name |
| `LUME_GEMINI_API_KEY` | — | Google Gemini API key |
| `LUME_GEMINI_MODEL` | `gemini-2.0-flash` | Gemini model name |
| `LUME_GROQ_API_KEY` | — | Groq API key |
| `LUME_GROQ_MODEL` | `llama-3.1-8b-instant` | Groq model name |
| `LUME_MAX_UPLOAD_MB` | `50` | Max upload file size in MB |
| `LUME_SAMPLE_THRESHOLD` | `100000` | Row count above which data is auto-sampled |
| `LUME_SHINY_TRACE` | `false` | Enable Shiny debug tracing |

---

## Keyboard Shortcuts

| Shortcut | Action |
|---|---|
| `Alt + Shift + E` | Open export dialog (Dashboard Builder) |
| `Alt + Shift + S` | Save dashboard config |
| `Alt + Shift + A` | Add chart card |

---

## Tech Stack

- **Backend**: R, Shiny, bslib (Bootstrap 5), shinyjs
- **Charts**: Plotly (interactive), base R graphics (insights)
- **Tables**: DT (DataTables)
- **Data**: tidyverse (dplyr, readr, tidyr, ggplot2), readxl, janitor
- **AI**: httr (REST calls to OpenAI / Gemini / Groq APIs)
- **Export**: htmlwidgets, webshot2, jsonlite
- **Deployment**: Docker (rocker/shiny base image)

---

## Contributing

1. Fork the repo
2. Create a feature branch (`git checkout -b feature/my-feature`)
3. Commit your changes (`git commit -m "Add my feature"`)
4. Push to the branch (`git push origin feature/my-feature`)
5. Open a Pull Request

---

## License

This project is open source and available under the [MIT License](LICENSE).
