# Lume Design System Spec

## 1) Global Art Direction: "The Draftsman"
This product should feel like precision engineering software: meticulous, grounded, and structural.

- Vibe: high-contrast, technical, calm, robust.
- Visual motifs: visible grid systems (subtle 1px lines), structural borders, tabular clarity, sharp geometry.
- Strict bans:
  - No drop shadows.
  - No glows.
  - No abstract fluid/3D decorative renders.

## 2) Color System (Industrial & Tactile)

### Core tokens
- `--bg`: `#F4F4F0` (Drafting Paper)
- `--text`: `#171717` (Onyx Ink)
- `--accent`: `#D9532E` (Safety Rust)
- `--surface`: `#E5E5E5` (Concrete)
- `--surface-strong`: `#FFFFFF`
- `--border`: `#171717`

### Dark mode tokens
- `--bg-dark`: `#0D0D0D` (Carbon)
- `--text-dark`: `#EDEDED`
- `--surface-dark`: `#171717`
- `--border-dark`: `#EDEDED`

### Usage rules
- Primary backgrounds use Drafting Paper.
- All primary dividers and frames use 1px Onyx borders.
- Safety Rust is used only for actions, key highlights, and critical calls.
- Avoid decorative gradients.

## 3) Typography System

### Font stacks
- Primary (headings/body): `"Geist", "Inter", -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif`
- Monospace (data/nav/code): `"JetBrains Mono", "IBM Plex Mono", "SFMono-Regular", Menlo, Consolas, monospace`

### Hierarchy
- H1: 72px, 600, tight tracking (`letter-spacing: -0.02em`)
- H2/H3 section labels: 14px, uppercase, monospace, wide tracking (`letter-spacing: 0.14em`)
- Body: 14–16px, line-height 1.6
- Data labels/tags/tooltips: monospace 12–13px

## 4) Component Language

### Buttons
- Radius: 0–2px only.
- Border: 1px solid Onyx.
- Default: Onyx background with Drafting Paper text.
- Hover/active: immediate snap to Safety Rust.
- Transition: none or <= 80ms linear; no soft fades.

### Cards/containers
- No shadows.
- Border: 1px solid Onyx.
- Background: Concrete or white depending on hierarchy.
- Spacing: strict rhythm (8/12/16/24).

### Inputs/selects
- Height: 36px.
- Border: 1px solid Onyx.
- Focus ring: 2px Safety Rust outline offset from border.
- Labels: monospace micro-labels where appropriate.

### Tables
- Always visible row/column lines.
- Monospace headers and metadata rows.
- Prefer compact density with strong legibility.

## 5) Motion & Interaction
- Micro-interactions are mechanical, not playful.
- No floating entrance animations.
- Allowed: reveal-up text/section animation via clipped container.
- Hover states should be immediate and binary.
- Destructive actions must have explicit confirmation.

## 6) Data Visualization Style
- Prefer Tufte-like restraint and high data-ink ratio.
- Keep palettes mostly monochrome + Safety Rust highlight.
- Gridlines subtle but present for readability.
- Tooltips in monospace.
- Favor:
  - sparklines
  - compact bar/line
  - simple scatter
  - raw metric strips / JSON summaries for diagnostic views

## 7) Information Architecture (Portfolio-style framing adapted to Lume)

### A. Hero (Overview top)
- Two-column strict grid.
- Left:
  - Kicker (monospace): `// SYSTEM ARCHITECT & ML ENGINEER`
  - Headline: `Bridging Research & Reality.`
  - Subheadline: concise systems statement.
  - CTA: `[ View Architecture ]` / `[ Initialize Contact ]`
- Right:
  - syntax-highlighted code block or architecture ASCII map.

### B. Technical Philosophy
- Full-width section.
- Three numbered manifesto blocks (`01`, `02`, `03`) in monospace.
- Pull-quote scale with strong contrast.

### C. Core Competencies
- Border-heavy grid/table layout.
- Blocks:
  - AI/ML
  - Full-Stack
  - Strategy
- Hover reveals monospace tool stack or hard data points.

### D. Case Studies
Each case card uses strict metadata chain:
`[PROBLEM] -> [ML SOLUTION] -> [FULL-STACK IMPLEMENTATION] -> [BUSINESS OUTCOME]`

## 8) Accessibility Requirements
- Contrast target: WCAG AAA where feasible; minimum AA everywhere.
- Keyboard navigation for all controls.
- Focus always visible (never removed).
- ARIA labels for icon-only actions.
- Maintain semantic heading order and table semantics.

## 9) Implementation Notes for Current Shiny App
Although the original framework note mentions Next.js/Astro, this app is Shiny; apply the same visual system here.

### Required updates
1. Replace all card shadows with 1px borders in `www/custom.css`.
2. Normalize corner radii to `0px` or `2px`.
3. Set global color/font tokens via CSS custom properties.
4. Convert top sections (Overview) into a strict grid-first hero structure.
5. Update module cards (`upload`, `insights`, `transform`, `viz`, `dashboard`) to use the structural style.
6. Enforce monospace on data-heavy labels, code snippets, KPIs metadata, tooltips.
7. Ensure Plotly theme defaults match monochrome + rust accent.

## 10) Acceptance Checklist
- [ ] No box-shadow remains anywhere.
- [ ] All interactive controls meet contrast and focus requirements.
- [ ] Safety Rust used consistently for action/critical emphasis.
- [ ] H2/H3 rendered in uppercase monospace with tracking.
- [ ] Overview has strict two-column hero behavior on desktop.
- [ ] Mobile/tablet keeps structural grid without decorative collapse.
- [ ] Chart styling follows restrained industrial palette.
- [ ] All icon-only buttons include ARIA labels.

## 11) AI Handoff Prompt Snippet
Use this when prompting design-generation tools:

"Apply a Draftsman-style industrial UI: drafting-paper background (#F4F4F0), onyx text/borders (#171717), rust accent (#D9532E), sharp geometry, no shadows, no glow, no gradients. Typography is Geist/Inter with JetBrains Mono for technical labels. Section labels are uppercase monospace with wide tracking. Layout is strict grid with visible 1px structure lines. Interactions are mechanical and immediate. Accessibility must pass WCAG AA minimum, target AAA contrast where possible."
