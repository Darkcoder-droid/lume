# Deployment Guide

## Environment Variables
- `LUME_LOG_DIR`: directory for application logs (default: `logs`)
- `LUME_LOG_FILE`: log file path (default: `logs/app.log`)
- `LUME_ENABLE_AUTO_ERROR_REPORT`: `true|false` for optional auto-reporting

## Run Locally
```bash
Rscript -e "shiny::runApp('.', host='0.0.0.0', port=3838)"
```

## Docker
```bash
docker build -t lume-dashboard .
docker run -p 3838:3838 lume-dashboard
```

## Production Notes
- Use reverse proxy (nginx) with HTTPS.
- Mount persistent volume for `logs/`.
- Increase memory limits for larger datasets.
- Ensure `webshot2` browser dependencies are available for PDF/PNG export.
