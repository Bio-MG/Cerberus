# Reproductibilité de l'environnement R (renv)

> **À fusionner dans le README.md existant** — ce fichier n'a pas été
> généré à partir du README réel (non fourni dans cette session), il est
> donc livré en autonome. Collez cette section où elle a du sens
> (typiquement après "Installation" / avant "Utilisation").

## Pourquoi renv ?

Ce projet dépend d'une pile R assez large (Seurat v5, DESeq2/edgeR/limma,
SingleR/celldex, ComplexHeatmap, plusieurs paquets GitHub-only comme
BPCells/spacexr — voir `global.R`). Sans figer les versions, un
`install.packages()` fait à des dates différentes sur deux machines peut
installer des versions différentes de Seurat/Bioconductor, avec un risque de
dérive silencieuse (résultats différents, crash imprévisible). `renv` fige
l'état exact (CRAN, Bioconductor et GitHub) dans un fichier `renv.lock`
versionné avec le code.

## Installation initiale (une fois par machine)

Depuis la racine du projet (dossier contenant `app.R`) :

```r
Rscript scripts/renv_bootstrap.R
```

Ce script :
1. installe `renv` et `BiocManager` si absents ;
2. détecte automatiquement la release Bioconductor correspondant à VOTRE
   version de R installée (voir note ci-dessous — jamais de version codée
   en dur) ;
3. lance `renv::init()`, qui scanne les `source()`/`library()` du projet
   (fonctionne très bien pour une app Shiny à plat, sans `DESCRIPTION`) et
   crée `renv.lock`, `.Rprofile` et `renv/activate.R`.

### Paquets GitHub-only (à installer explicitement APRÈS le bootstrap)

`global.R` documente déjà ces dépendances non-CRAN ; utilisez
`renv::install()` (pas `remotes::install_github()` directement, pour que
renv trace bien la source) :

```r
renv::install("bnprks/BPCells/r")            # backend disque spatial — obligatoire
renv::install("dmcable/spacexr")             # RCTD (déconvolution avec référence)
renv::install("cellgeni/schard")             # lecteur .h5ad robuste
renv::install("mojaveazure/loomR")           # lecteur .loom — obligatoire depuis le
                                             # 2026-11-05 (QW-3 option a : retiré de
                                             # ignored.packages ; import .loom natif,
                                             # hard-stop mod_import_sc.R:1008 neutre)
# Optionnels, non requis par le pipeline par défaut :
# renv::install("prabhakarlab/Banksy@devel")
# renv::install("satijalab/seurat-wrappers")
# renv::install("zdebruine/sceasy")           # conversion sce↔anndata — optionnel-guarded,
                                              # reste dans ignored.packages (décision QW-3)
```

Puis les paquets CRAN/Bioconductor restants (`RANN`, `irlba`,
`topicmodels`, `slam`, `STdeconvolve`, `DESeq2`, `edgeR`, `limma`,
`ComplexHeatmap`, `SingleR`, `celldex`, etc.) :

```r
renv::install(c(
  "shiny", "bslib", "Seurat", "ggplot2", "dplyr", "DT", "patchwork",
  "viridis", "plotly", "bsicons", "future", "shinyFiles", "harmony",
  "destiny", "fs", "igraph", "Matrix", "reshape2", "shinyjs", "circlize",
  "rmarkdown", "zip", "mirai", "sf", "leaflet", "scattermore", "ape",
  "RANN", "irlba", "png", "RColorBrewer", "shinyWidgets", "shinycssloaders",
  "leiden", "topicmodels", "slam"
))
renv::install(c("SingleR", "celldex", "SingleCellExperiment", "DESeq2",
                "edgeR", "limma", "ComplexHeatmap", "clusterProfiler",
                "org.Hs.eg.db", "org.Mm.eg.db", "ReactomePA", "AnnotationDbi"))
```

### Figer l'état (à faire après toute installation/mise à jour de paquet)

```r
renv::snapshot()
```

Committez ensuite `renv.lock`, `.Rprofile` et `renv/activate.R` (le
`.gitignore` généré par `renv::init()` exclut déjà `renv/library/`, qui ne
doit jamais être versionné).

## Pour un nouveau contributeur (ou une nouvelle machine)

```r
# après avoir cloné le repo et ouvert R depuis la racine :
renv::restore()
```

`renv` réinstalle exactement les versions figées dans `renv.lock` — CRAN,
Bioconductor et GitHub (SHA de commit inclus) — sans devoir deviner quoi
installer.

## Note sur la version Bioconductor

Aucune version Bioconductor n'est codée en dur dans ce projet : les
releases Bioconductor sont couplées à la version mineure de R (ex. R 4.4.x
↔ Bioc 3.19/3.20 selon le point-release exact), donc figer un numéro en dur
dans un script casserait silencieusement `BiocManager::install()` sur toute
machine tournant une autre version de R. `scripts/renv_bootstrap.R` détecte
la release correcte via `BiocManager::repositories()`, et `renv.lock` (une
fois généré par `renv::snapshot()`) devient la source de vérité unique —
consultez `BiocManager::version()` localement pour connaître la release
active sur votre machine, et reportez-la ici une fois `renv.lock` généré :

> **Release Bioconductor figée dans `renv.lock` : `_à compléter après le premier `renv::snapshot()`_`**

## Environnement Python (scCODA) — pin des versions

> Ajouté le 2026-11-04 (QW-2, Flux QW — `docs/ROADMAP.md` §2.0 row « → Flux QW » ;
> source `docs/ideation/USED_PACKAGE_GEMS_2026-11-03.md` §12.3, factcheck indépendant).

L'application utilise un venv Python local `python_env_sccoda/` (Python 3.13.2)
pour le module scCODA (déconvolution bayésienne de composition cellulaire) et
sa pile scanpy. Sans pin, un rebuild — ou une montée de Python/pip — peut
changer silencieusement les versions, donc les résultats.

- **Pin généré le 2026-11-04** — **copie canonique SUIVIE : `python-env-requirements.lock.txt`**
  (racine du dépôt, commitée — décision opérateur option B, 101 paquets, `pip 26.2.1 freeze`,
  Python 3.13.2). Versions clés mesurées : `scanpy==1.12.4` · `scCODA==0.1.9` ·
  `anndata==0.13.3.post0` · `tensorflow==2.21.0`.
  `python_env_sccoda/requirements.lock.txt` (dans le venv) = copie de travail régénérable,
  pas la référence.
- ⚠️ **Le venv entier reste gitigné** (`.gitignore` `/python_env_sccoda/`) — seule la copie
  racine est versionnée. Après toute installation/mise à jour dans le venv : re-générer
  la copie canonique (commande ci-dessous) et la commiter.
- **Reconstruire l'env à l'identique** :

  ```bash
  python -m venv python_env_sccoda
  python_env_sccoda/Scripts/python.exe -m pip install -r python-env-requirements.lock.txt
  ```

- **Re-générer le pin canonique après toute installation/mise à jour** :

  ```bash
  python_env_sccoda/Scripts/python.exe -m pip freeze > python-env-requirements.lock.txt
  ```
