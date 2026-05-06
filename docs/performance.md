# Performance and Profiling

## Implemented Optimizations
- Large-file sampling at upload (`>100k` rows).
- Parsed upload caching for repeated delimiter/sample toggles.
- Debounced visualization preview reactivity.
- `bindEvent`-based upload processing trigger.

## Profiling with profvis
```r
profvis::profvis({
  shiny::runApp('.')
})
```

## Recommended Monitoring
- Track upload durations and transformation timings via `logs/app.log`.
- Monitor memory consumption when switching from sampled to full data.
