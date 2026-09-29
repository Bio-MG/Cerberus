# PATHWAY_NETWORK_CONTRACT.md — Interactive enrichment network (STAT-S2 V2)

> Contract written 2026-09-28. Règle contract-first : toute évolution passe
> SIMULTANÉMENT par le code, le test de gel et ce document.

## 1. Frozen files

- Code: `R/core/pathway_helpers.R` — `build_pathway_network_data()`,
  `pathway_network_layout()`, `plot_pathway_network_interactive()`
- Test: `tests/testthat/test-pathway-network-interactive.R`
- Thresholds: `config/thresholds.R` — `TS_PATHWAY_NETWORK_MAX_TERMS`,
  `TS_PATHWAY_NET_MIN_SIM`, `TS_PATHWAY_NETWORK_LAYOUT_SEED`
- Consumers: `modules/bulk/mod_bulk_pathways.R`, `modules/sc/mod_sc_pathways.R`
  (checkbox `network_interactive`, default **static** — V1 unchanged by default)

## 2. Library decision (cadrage §2bd.5 #3, decided 2026-09-28)

**plotly + igraph** — zero new dependency (both already in `renv.lock` and
installed; PLOT-S6 precedent for the plotly toggle). visNetwork is **deferred,
not rejected**: adding it would require the renv.lock justification precedent
(CellChat/liana) and was judged not worth the dependency for this version.

| Axis | plotly + igraph (this version) | visNetwork (evaluated, deferred) |
|---|---|---|
| Physics layout | FR layout, seeded, static coordinates | live physics, drag nodes |
| Multi-select / click | plotly events (`plotly_click` available but not wired) | native selection events |
| Dependency cost | 0 | new lock entry + justification in AGENTS §5 |
| Upgrade path | none needed if hover suffices | revisit if users ask for drag/select UX |

## 3. Frozen data structure — `pathway_network_data`

`build_pathway_network_data(df, top_n, mode)` returns a list of class
`pathway_network_data` with exactly these three names:

- `nodes`: data.frame `id`, `label`, `kind` (`"term"` / `"gene"`), `count`
  (integer, `NA` for genes), `p.adjust` (numeric, `NA` for genes)
- `edges`: data.frame `from`, `to`, `weight` — emap: `from`/`to` are sorted
  term IDs (undirected, one edge per pair), `weight` = Jaccard similarity ≥
  `TS_PATHWAY_NET_MIN_SIM`; cnet: term→gene membership, `weight` = 1
- `meta`: list `mode`, `top_n` (terms actually taken), `n_edges`,
  `min_similarity` (emap only, `NA` for cnet), `gene_separator` (`"/"`,
  clusterProfiler convention)

Gene sets come from the `geneID` column split on `/` (clusterProfiler format).

## 4. Refusals (all `errorCondition(class = "pathway_error")`)

| Condition | Message anchor |
|---|---|
| `df` not a data.frame | `df doit être un data.frame` |
| `enrich_obj` attribute absent | `Aucun objet d'enrichissement brut attaché` |
| `top_n` not numeric >= 2 | `top_n doit être un nombre >= 2` |
| fewer than 2 enriched terms | `Au moins deux voies enrichies` |
| required columns missing (`ID`, `Description`, `Count`, `p.adjust`, `geneID`) | `Colonnes manquantes` |
| `plot_pathway_network_interactive()` given a foreign object | `net doit être la sortie de build_pathway_network_data()` |

## 5. Determinism

`pathway_network_layout()` computes the Fruchterman–Reingold layout inside
`withr::with_seed(TS_PATHWAY_NETWORK_LAYOUT_SEED, ...)` — two calls with the
same input return identical coordinates. Frozen by the test.

## 6. Caps

`top_n` is silently capped at `TS_PATHWAY_NETWORK_MAX_TERMS` (300) — the
cadrage's 200–300-term budget; the UI widget caps at 100. No refusal at the
cap: the network is descriptive.

## 7. UI behavior (consumers)

- A checkbox `network_interactive` (label `Réseau interactif (survol des nœuds)`)
  switches the plot area between `plotOutput` (V1 static, unchanged) and
  `plotlyOutput` — same router pattern as `mod_bulk_filter.R`
  `pca_interactive` / PLOT-S6.
- Hover: term → label, gene count, p.adjust; gene → identifier.
- The plotly mode bar provides PNG export of the interactive view; the module
  PNG download buttons stay bound to the static renderer.
- **Open item (not wired in this version):** clicking a term to filter the
  pathway table (`plotly_click`). Deferred — needs a product decision on the
  target panel and an i18n-reviewed empty state.
