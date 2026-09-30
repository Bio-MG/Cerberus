# TranscriptoShiny

Une plateforme R/Shiny modulaire, **local-first**, dédiée à l’exploration et à l’analyse guidées de données de bulk RNA-seq, de single-cell RNA-seq et de transcriptomique spatiale.

[![R](https://img.shields.io/badge/language-R-blue.svg)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**Langues :** [Français](#français) · [English](#english)

**Statut :** `V1.1.0-rc` — plateforme transcriptomique locale couvrant le single-cell RNA-seq, le bulk RNA-seq et la transcriptomique spatiale, avec des analyses avancées disponibles selon les dépendances installées (vitesse ARN, communication cellule-cellule importée, abondance différentielle Milo/scCODA, déconvolution spatiale, rapports reproductibles). La V1.1 est une vague UX/UI (pipeline auto 1 clic mutualisé, regroupement des panneaux, conteneur Spatial) — **zéro changement de comportement scientifique**.
> Certaines analyses avancées reposent sur des dépendances optionnelles, des paquets GitHub ou un environnement Python dédié. Consultez la section Installation avant de lancer un workflow.

---

# Français

## Pourquoi TranscriptoShiny ?

TranscriptoShiny est conçu pour l’exploration itérative de données transcriptomiques par des biologistes et des bioinformaticiens. L’application combine des interfaces guidées, des messages de validation proactifs, des visualisations interactives et des rapports reproductibles, tout en maintenant un contrôle strict des données locales par l’utilisateur.

L’application privilégie :

- **Une exécution local-first** : les données restent sur votre station de travail, afin de préserver confidentialité et contrôle.
- **Un accompagnement progressif** : des workflows intuitifs pour les non-experts, sans masquer les paramètres analytiques critiques aux utilisateurs avancés.
- **La scalabilité** : des stratégies conscientes de la mémoire, par exemple le sketching de Seurat v5 et les matrices sur disque BPCells, pour traiter de grands jeux single-cell et spatiaux sur du matériel standard.
- **La rigueur scientifique** : des garde-fous intégrés pour le design expérimental en expression différentielle bulk, ainsi que des étapes analytiques transparentes et exportables.
- **La traçabilité** : provenance produite à chaque étape d'analyse et compilée au moment du rapport (jamais reconstruite après coup), avec contrats de résultats figés et couverts par des tests de gel.

*Langue : interface bilingue français / anglais (commutateur en barre latérale). Les clés de traduction sont les chaînes françaises ; le basculement est live (shiny.i18n + traducteur de session).*

## Workflows pris en charge

TranscriptoShiny fournit des environnements modulaires dédiés à trois domaines transcriptomiques principaux :

1. **Single-cell RNA-seq** : de l'import de données 10x ou de matrices jusqu'au clustering, à l'annotation, à la recherche de marqueurs et à l'analyse de voies — plus vitesse ARN, communication cellulaire, abondance différentielle (Milo/scCODA) et rapport consolidé.
2. **Bulk RNA-seq** : des matrices de comptages bruts et métadonnées jusqu’à l’expression différentielle, à la comparaison multi-contrastes et à l’enrichissement fonctionnel.
3. **Transcriptomique spatiale** : des imports Visium/Xenium/CosMx/Slide-seq jusqu’au clustering spatial, à la déconvolution, à l’intégration multi-échantillons et à l’analyse de niches.

## Navigation dans l'application

| Onglet / zone | Rôle |
| :--- | :--- |
| Import Données | Charger des fichiers Single-Cell, Bulk RNA ou Spatial, ou depuis la **Source publique (GEO)** (dernière entrée du menu). Rappel « mapping des IDs » + bouton natif « Aller au mapping des IDs » dans les imports Single-Cell/Bulk/GEO. |
| Analyse Single-Cell | Cinq sections parent : **Préparation** (mapping, pipelines), **Analyse** (annotation, visualisation, marqueurs, corrélation, voies), **Dynamique** (trajectoire, pseudobulk, vitesse ARN), **Abondance cellulaire** (onglets internes : A. design — B. Milo/scCODA — C. vues croisées), **Livrables** (rapports/exports). |
| Analyse Bulk RNA | Étapes numérotées 1→4 : pipeline QC & filtrage (avec vue compacte « Résumé Pipeline Bulk »), mapping d'identifiants, expression différentielle & voies, puis « 4. Livrables — Rapport & Script R ». |
| Analyse Spatiale | Conteneur à sidebar : étapes numérotées en accordéon à gauche (une étape ouverte à la fois), résultats à droite ; dataset et statut des démons toujours visibles dans la sidebar. Pipeline spatial, QC, clustering, déconvolution, visualisation, intégration multi-échantillons, niches, rapport/export. |
| Barre latérale système | Changement de langue, usage mémoire, nettoyage RAM, limites mémoire/upload, état des objets chargés, sauvegarde/chargement de session, aide intégrée. |

**Paradigme pipeline mutualisé :** dans les trois domaines, le **pipeline automatique 1 clic** est le premier panneau de l'accordéon (0.), annoncé par un badge de paradigme en tête de sidebar (« auto dispo » / « mode guidé » / « auto async »). Le mode guidé pas-à-pas reste intégralement disponible ; les deux paradigmes partagent les mêmes calculs et le même état.

> Note : l'entrée **« Source publique (GEO) »** est une source de données, pas une quatrième modalité. Elle accepte les fichiers `series_matrix.txt` compatibles en import local, ainsi que le chargement par accession ; ce n'est pas un simple visualiseur hors-ligne.

## Capacités principales

### Single-cell RNA-seq

- **Import flexible** : prise en charge des dossiers 10x, ainsi que des formats `.rds`, `.h5`, `.h5ad` et `.loom`, avec conservation de `orig.ident` dans les workflows multi-échantillons.
- **Mapping des identifiants géniques** : conversion optionnelle et robuste des identifiants Ensembl/Entrez en symboles géniques avant l’analyse.
> Le mapping d'identifiants dépend de l'espèce, de la version d'annotation et de la qualité des identifiants d'entrée. Les identifiants non résolus, ambigus ou dupliqués doivent être vérifiés avant toute interprétation biologique ; conservez toujours les identifiants originaux dans vos exports.
- **Pipeline standard** : contrôle qualité, normalisation, sélection de gènes hautement variables, PCA, construction du graphe de voisins, clustering, UMAP et t-SNE optionnel. Disponible en **pipeline automatique 1 clic** (panneau 0 : mapping d'IDs optionnel, QC, normalisation/PCA/clustering/UMAP, t-SNE, puis étapes optionnelles annotation SingleR, marqueurs, corrélation génique, ORA sur top marqueurs, trajectoire) ou en mode guidé panneau par panneau.
> L'interface Single-Cell regroupe ses panneaux en cinq sections parent (Préparation / Analyse / Dynamique / Abondance cellulaire / Livrables). Vitesse ARN, communication cellule-cellule et abondance différentielle exigent des prérequis spécifiques et restent hors du pipeline automatique.
- **Correction de batch** : intégration avec Harmony lorsque plusieurs échantillons ou batchs sont présents.

#### Multi-échantillons single-cell

Le workflow Single-Cell est nativement multi-échantillons (ex: Contrôle vs Traitement, Jour 0 vs Jour 10 — chaque échantillon est analysé comme une entité distincte, comme dans le module Spatial) :

- **Import groupé** : plusieurs dossiers 10x, `.rds` ou `.h5` peuvent être importés dans une même session (Options A et B de l'import Single-Cell). Chaque import devient un échantillon distinct (`orig.ident`) ; dès que ≥ 2 échantillons sont chargés, un aperçu récapitulatif (échantillon, cellules, gènes, condition/batch si détectables) confirme visuellement la reconnaissance des entités.
- **Correction de batch** : Harmony est appliquée automatiquement sur l'identité d'échantillon (`orig.ident`) dès que ≥ 2 échantillons sont détectés — choisir « Harmony » comme méthode de réduction du pipeline.
- **Compatibilité aval** : pseudobulk, Milo et scCODA exploitent l'identité d'échantillon pour éviter la pseudo-réplication (l'unité de réplication est l'échantillon, jamais la cellule — voir la validation du plan expérimental dans « Abondance cellulaire »).

- **Scalabilité consciente de la mémoire** : workflows de sketch Seurat v5 (`SketchData` avec LeverageScore, `ProjectData`), gestion compatible BPCells, mise à l’échelle ciblée des variables et sous-échantillonnage stratifié pour les explorations coûteuses.
- **Annotation et exploration** : annotation automatique des types cellulaires via SingleR (références celldex), recherche de marqueurs (`FindAllMarkers`), corrélation génique, analyse de voies et visualisations variées (embeddings, FeaturePlots, violons, DotPlots, heatmaps, vues ridge/empilées).
- **Vitesse ARN** : import strict de données de vélocité pré-préparées (matrices spliced/unspliced et résultats/vecteurs compatibles) alignées sur l'objet Seurat. L'application effectue des contrôles de cohérence multi-états, fournit des visualisations phase-portrait et vectorielles, et exporte PNG/PDF/CSV. Elle visualise des résultats validés sans recalculer silencieusement un modèle de vélocité.
- **Communication cellule-cellule (panneau 8b)** : import-only de résultats externes (table/objet CellChat, means+p-values CellPhoneDB). Table canonique à 12 champs, appariement exact des identités, QC, vues exploratoires (dotplot, heatmap de pathways, réseau circulaire), centralité descriptive, filtres avec provenance. Pas de génération de novo depuis les comptages bruts (reste un item de parking futur).
- **Abondance différentielle (panneaux 8c–8f)** :
> Prérequis : les analyses d'abondance différentielle exigent des métadonnées identifiant l'unité de réplication biologique (ex. sample_id, donor_id, patient_id) et une condition expérimentale. Un cluster de cellules seul n'est pas un réplica biologique.
  - Validation du design expérimental qui **bloque la pseudo-réplication** (les cellules ne sont jamais traitées comme des réplicas biologiques ; planchers sur réplicas par condition, cellules par échantillon, etc.).
  - Milo (DA par voisinages, seed enregistrée, graphSpatialFDR).
  - scCODA (DA compositionnelle au niveau échantillon via environnement Python explicite ; diagnostics de convergence en pur R sur ESS / R-hat / divergences).
  - Vues croisées (catégories de concordance descriptives entre Milo et scCODA ; aucune p-value composite).
- **Trajectoire** : pseudotemps exploratoire sur graphe kNN **plus inférence de lignées Slingshot en option** (quand le paquet est installé). Plafond strict sur le nombre de cellules.
- **Pseudobulk** et **rapport Single-Cell consolidé** (compilateur d'état canonique + provenance uniquement — ne ré-exécute jamais les analyses ; HTML autonome, bundle d'export avec manifeste, tables fidèles, script R reproductible, sessionInfo).
- La provenance est enregistrée pour chaque étape d'analyse alimentant un rapport ; les sections sans provenance sont rejetées par le validateur.
- *Note scientifique* : la trajectoire par défaut reste le pseudotemps léger sur graphe ; Slingshot est proposé comme moteur optionnel quand il est installé. La détection native de doublets et la régression du cycle cellulaire ne sont pas encore intégrées.

### Bulk RNA-seq

> Pour DESeq2, edgeR et limma-voom, fournissez des comptages bruts au niveau gène, idéalement entiers ou de type entier, issus d'un outil de quantification compatible avec l'analyse sur comptages. N'utilisez pas de TPM, FPKM, CPM, log-counts ni de matrices corrigées du batch en entrée de l'expression différentielle.

- **Import intelligent** : prise en charge de matrices de comptages bruts fusionnées ou de fichiers de comptages par échantillon, avec alignement automatique des métadonnées et résolution des doublons de gènes.
- **Prise en charge de GEO** : parsing hors-ligne de fichiers GEO `series_matrix.txt`, sans accès réseau ni dépendance à `GEOquery`.
- **QC exploratoire** : filtrage, transformation stabilisant la variance (VST), PCA, scree plots et heatmaps de corrélation entre échantillons.
- **Expression différentielle** : workflows propulsés par DESeq2, edgeR et limma-voom.
> DESeq2, edgeR et limma-voom sont fournis comme moteurs distincts. Les comparer aide à explorer la robustesse, mais les résultats doivent être interprétés avec un design, des filtres, des facteurs de normalisation et des contrastes clairement documentés.
- **Garde-fous de design** : contrôles proactifs des covariables confondues, des valeurs manquantes invalidant le modèle et des covariables ne présentant qu’un seul niveau observé.
- **Gestion des contrastes** : contrastes standards, définis par l’utilisateur et pairwise, avec comparaison multi-méthodes et exploration par consensus de rangs.
- **Visualisation et enrichissement** : volcano plots, MA plots, heatmaps, comparaisons Venn/UpSet multi-contrastes et analyse de voies ORA/GSEA.
- *Note scientifique* : les résultats d’analyse de voies doivent être interprétés au regard de l’univers de fond choisi et du mapping des identifiants. Le consensus de rangs multi-méthodes est une aide exploratoire, pas une méta-analyse formelle.

### Transcriptomique spatiale

- **Import étendu** : Visium, Visium HD (layouts pris en charge), Xenium, CosMx et Slide-seq, sous réserve de compatibilité avec les layouts de fichiers standards.
- **Architecture sur disque** : les grands jeux spatiaux utilisent BPCells pour conserver les matrices de comptages sur disque, tandis que des représentations légères de type sketch/métadonnées permettent une analyse interactive en RAM.
- **Exécution asynchrone** : les opérations lourdes passent par un pool de démons `mirai` (timeouts, vérifications de santé, cache par jeu de données, bouton de réinitialisation des démons), avec journaux de tâches afin d'éviter le blocage de l'interface.
> Les tâches asynchrones restent attachées à la session R locale. Ne fermez pas RStudio ni R pendant leur exécution. Les calculs sont volontairement plafonnés pour limiter la saturation RAM et la création excessive de processus sur stations de travail.
- **Analyse spatiale** : QC spatial, analyse de gènes spatialement variables de type Moran et clustering spatial léger tenant compte du voisinage. Cette approche s'inspire des principes de BANKSY mais n'est pas une implémentation complète ni interchangeable du paquet BANKSY original.
- **Déconvolution** : RCTD, transfert de labels depuis une référence et approches de type LDA. La référence est **partagée** (préparée une fois dans Import > Spatial, réutilisée par RCTD et Label Transfer), avec garde-fous de validation.
- **Multi-échantillons et niches** : intégration par sketch respectueuse de la mémoire, avec correction de batch Harmony optionnelle, et analyse de niches fondée sur la composition des voisinages locaux.
- **Backend disque** : BPCells est le backend sur disque ; seul le sketch RAM est garanti portable dans une session sauvegardée.
- **Visualisations avancées** : superpositions histologiques, vues spatiales/embeddings liées, sélection ROI au lasso, exploration de marqueurs de ROI et export de sous-ensembles.
- *Note scientifique* : la précision de la déconvolution dépend fortement de la qualité de la référence, de la compatibilité entre plateformes et du contexte tissulaire. Ces méthodes sont destinées à l’exploration scientifique et ne sont pas validées pour la décision clinique.

## Architecture

TranscriptoShiny repose sur une architecture Shiny modulaire. La logique analytique réutilisable est séparée de l'interface, les modules orchestrent les workflows, et les résultats exportables conservent la provenance nécessaire à leur interprétation.
Le développement suit une approche testée et orientée reproductibilité : les résultats analytiques clés sont validés par des tests automatisés avant d'être exposés dans l'interface.

```text
TranscriptoShiny/
├── app.R / global.R
├── config/          # defaults.R, thresholds.R (single source of truth)
├── i18n/            # translation.json (fr/en)
├── R/
│   ├── core/        # state, validation, provenance, jobs, caching, io/pathway helpers
│   ├── sc/          # velocity, communication, abundance (Milo/scCODA/design/cross), trajectory, pipeline, plotting, bpcells, export
│   ├── bulk/        # import engine, report engine, helpers
│   ├── spatial/     # async (mirai), io, deconv prep/tasks, multi, niche, plotting, report, reference, stats, export
│   ├── reports/     # collector, validator, render, bundle
│   └── plotting/    # shared palettes
├── modules/         # import/, sc/, bulk/, bulk_de/, spatial/ (+ deconv sub-modules)
├── reports/         # Rmd templates (sc, bulk, spatial + child)
├── scripts/         # renv bootstrap and development/benchmark scripts
└── renv/            # lock + activate
```

L'état inter-modules passe par le `reactiveValues` partagé (`global_data`) ; les calculs lourds spatiaux/async passent par le pool de démons `mirai`. Les modules lisent leurs paramètres dans `config/` et les seuils déclarés — aucune valeur magique en dur.

## Installation

### 1. Cloner le dépôt

```bash
git clone https://github.com/Bio-MG/ShinyApp---TranscriptoShiny.git
cd ShinyApp---TranscriptoShiny
```

### 2. Installer les dépendances (recommandé : renv)

Le projet utilise **renv** pour restaurer un environnement R reproductible. Travaillez depuis la racine du dépôt cloné afin que `.Rprofile` puisse activer `renv`.
Dans R ou RStudio :
```r
setwd("path/to/ShinyApp---TranscriptoShiny")
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::restore()
```
Si la restauration échoue ou sur une nouvelle machine, lancez le script bootstrap depuis la racine du dépôt :
```r
source("scripts/renv_bootstrap.R")
```
> Important : lancez toujours l'application depuis la racine du projet. `app.R` vérifie explicitement que la librairie `renv` du projet est active et échoue avec une erreur claire sinon.

*Installation manuelle (déconseillée, développeurs uniquement) : ce n'est pas la voie supportée pour reproduire l'environnement de référence — préférez `renv::restore()`.*

```r
# Noyau CRAN minimal
install.packages(c("shiny", "bslib", "shinyjs", "shinyWidgets", "shinycssloaders",
                   "shiny.i18n", "bsicons", "DT", "plotly", "ggplot2", "dplyr",
                   "patchwork", "viridis", "future", "mirai", "igraph", "Matrix",
                   "RANN", "circlize", "rmarkdown", "zip", "fs", "scattermore"))

# Bioconductor
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install(c("Seurat", "SeuratObject", "SingleR", "celldex",
                       "SingleCellExperiment", "DESeq2", "edgeR", "limma",
                       "ComplexHeatmap", "slingshot"))
```

*Note : les dépendances spatiales, AnnData, RCTD et de déconvolution s'installent séparément ; `renv::restore()` reste l'installation complète supportée.*

### Dépendances optionnelles par fonctionnalité

| Fonctionnalité | Dépendance principale | Si absente |
| :--- | :--- | :--- |
| Matrices spatiales sur disque | BPCells | Pas de backend disque (tout en RAM) |
| Déconvolution RCTD | spacexr | RCTD indisponible |
| Lecture `.h5ad` robuste | schard | Repli vers SeuratDisk ou échec |
| Visium HD (parquet) | arrow | Import HD incomplet (alternative légère : nanoparquet) |
| Inférence de lignées | slingshot | Trajectoire limitée au pseudotemps graphe |
| scCODA | Environnement Python + dépendances scCODA | scCODA indisponible (erreur guidée) |
| Vues spatiales WebGL | leafgl | Rendu de grands nuages de points dégradé |

*Note : les workflows spatiaux peuvent exiger des packages supplémentaires tels que `BPCells`, `mirai`, `RANN` et `spacexr`. scCODA requiert un environnement **Python 3.13** dédié (`python_env_sccoda/` ou variable `TS_SCCODA_PYTHON`) — sans lui, une erreur classée avec guidance est levée, sans repli silencieux. Le PDF par domaine requiert TinyTeX ; le rapport consolidé 4F génère un HTML autonome sans pandoc.*

### 3. Lancer l’application

```r
shiny::runApp()
```

Vous pouvez aussi ouvrir `app.R` dans RStudio puis cliquer sur **Run App**.

### Environnement recommandé

| Composant | Recommandation |
| :--- | :--- |
| **Version de R** | 4.4.2 (version unique supportée) |
| **IDE** | RStudio (recommandé pour le développement et le débogage) — ouvrir `SHINYAPP test.Rproj` pour activer renv |
| **RAM (usage courant)** | 16 Go minimum |
| **RAM (grands jeux de données)** | 32 Go recommandés pour les grands jeux single-cell ou spatiaux |
| **Calcul** | L’exécution CPU-only est entièrement prise en charge ; aucun GPU n’est requis |
| **Stockage** | Un espace disque local rapide et suffisant est essentiel pour les fichiers temporaires, les rapports et les données spatiales BPCells |

## Workflows typiques

### Single-cell RNA-seq

1. Importez des données compatibles (dossiers 10x, `.rds`, `.h5`, `.h5ad` ou `.loom`).
2. Mappez facultativement les identifiants géniques vers des symboles standards.
3. Lancez le pipeline (automatique 1 clic ou guidé) : QC, normalisation, PCA, clustering et UMAP/t-SNE.
4. Annotez les types cellulaires avec SingleR.
5. Explorez les marqueurs, les corrélations géniques et les enrichissements de voies.
6. Exportez un rapport paramétré ou un script R reproductible.

### Bulk RNA-seq

1. Importez des matrices de comptages bruts de type entier et les métadonnées correspondantes. *(N’utilisez pas de valeurs TPM/FPKM pré-normalisées pour l’expression différentielle.)*
2. Mappez facultativement les identifiants géniques.
3. Appliquez filtrage et VST, puis examinez la PCA et le QC de corrélation entre échantillons.
4. Définissez le design expérimental, examinez les alertes relatives aux covariables et spécifiez les contrastes.
5. Lancez l’expression différentielle (DESeq2, edgeR ou limma-voom).
6. Explorez les résultats avec volcano/MA plots, heatmaps et comparaisons Venn/UpSet multi-contrastes.
7. Réalisez l’analyse de voies et exportez le rapport HTML/PDF multi-contrastes ou le script R.

### Transcriptomique spatiale

1. Importez des données spatiales (Visium, Visium HD, Xenium, CosMx ou Slide-seq).
2. Examinez les métriques de QC spatial et appliquez les filtres de spots/cellules.
3. Lancez le clustering spatial tenant compte du voisinage et/ou l’analyse de gènes spatialement variables de type Moran.
4. Préparez et validez facultativement un jeu de données single-cell de référence.
5. Lancez la déconvolution (RCTD, transfert de labels ou LDA).
6. Visualisez les résultats avec superpositions histologiques, vues liées et sélection de ROI au lasso.
7. Réalisez facultativement une intégration multi-échantillons par sketch ou une analyse de composition des niches.

## Métadonnées expérimentales (condition / réplicat)

Les analyses A-vs-B du domaine Single-Cell — pseudobulk DE, Milo, scCODA, et tous
les plots par condition — exigent que **chaque échantillon porte une CONDITION et
un RÉPLICAT**. Une CONDITION est une propriété de l'*échantillon* (ex. « contrôle »
vs « traité »), jamais de la cellule ; le RÉPLICAT distingue les échantillons
biologiques d'un même groupe et fait de l'échantillon l'unité de réplication.

Le panneau **« 0.5 Métadonnées — condition / réplicat »** de l'onglet Single-Cell
remplit ce contrat :

1. **Table design éditable** — un échantillon par ligne (détecté depuis
   `orig.ident`) ; les colonnes condition / replicate sont éditables directement
   dans la table.
2. **« Déduire depuis les noms »** — parse automatique des conventions
   `A_1` / `A-1` / `A.1` / `B2` (condition en préfixe) ou `1a` / `2b`
   (condition en suffixe). Le découpage « collé » ne s'applique qu'aux codes de
   condition d'**une seule lettre** : un nom comme `patient1` est déclaré
   indéductible (condition = nom, réplicat vide à compléter) plutôt que de
   fabriquer un réplicat.
3. **Jointure CSV** — « …ou joindre un CSV de design » : un fichier
   `sample,condition[,replicate]` rempli la table ; tout échantillon manquant
   est signalé (aucune valeur fabriquée).
4. **Récapitulatif + application unique** — le récap liste les bloqueurs
   (condition constante, réplicat unique = pseudo-réplication…) ; le bouton
   « ✅ Appliquer le design à l'objet SC » commit l'ensemble en une fois, purge
   les résultats dépendants et alimente le récap pseudobulk (comptes
   pseudo-échantillons par condition, alerte plan saturé) ainsi que la bannière
   du Volcano.

Sans design appliqué, les analyses conditionnelles restent volontairement
inaccessibles (l'application ne devine jamais un plan expérimental). Avec des
réplicats biologiques (ex. 3 vs 3), préférez le **Pseudobulk** à la comparaison
au niveau cellule pour éviter la pseudo-réplication.

## Travailler avec de grands jeux de données

TranscriptoShiny intègre des garde-fous spécifiques pour gérer les limites de mémoire et de calcul sur des stations de travail :

- **Utilisez les workflows de sketch** : pour les grands jeux single-cell ou spatiaux, activez les options de sketch Seurat v5 ou l’intégration spatiale par sketch afin d’éviter le chargement des matrices complètes en RAM.
- **Respectez les limites du stockage sur disque** : ne forcez pas les matrices de comptages spatiales complètes en mémoire ; utilisez les tâches asynchrones fournies et adossées à BPCells.
- **Exploitez les aperçus** : utilisez les visualisations d’aperçu/sous-échantillonnées proposées par l’interface et réservez les exports pleine fidélité au rapport final.
- **Gérez le stockage temporaire** : assurez-vous que le `tempdir()` de R ou l’emplacement de cache configuré dispose d’un espace libre suffisant, en particulier pour les imports volumineux `.h5` ou `.h5ad` et les artefacts BPCells.
- **Échelonnez les opérations lourdes** : sur une station avec 32 Go de RAM, exécutez une seule analyse lourde à la fois, par exemple une déconvolution spatiale ou une recherche de marqueurs à grande échelle.
- **Utilisez un stockage local rapide** : conservez les données brutes, les rapports générés et les répertoires BPCells sur des disques locaux rapides, par exemple NVMe SSD, plutôt que sur des volumes réseau.

### Barre latérale système

- Afficher l'usage mémoire et l'état des objets chargés.
- Déclencher un nettoyage mémoire explicite.
- Ajuster la limite mémoire par tâche parallèle.
- Ajuster la taille maximale d'upload.
- Vérifier si les objets SC/Bulk/Spatial sont chargés et si le pipeline principal a tourné.
- Sauvegarder/restaurer une session de travail.
- Changer la langue de l'interface entre français et anglais.

> Sur une machine 32 Go, n'allouez pas toute la RAM physique à une seule tâche.

### Sauvegarde et restauration de session

La barre latérale permet de sauvegarder/charger une session `.rds`. Les objets SC, Bulk et le sketch spatial sont sérialisés. Pour les données spatiales adossées à BPCells, les matrices complètes restent liées aux artefacts sur disque. Après restauration sur une autre machine ou après déplacement/nettoyage des fichiers, le sketch spatial peut rester visualisable mais le clustering/la déconvolution peuvent exiger une ré-importation. La référence scRNA-seq partagée (RCTD/transfert de labels) peut aussi nécessiter une ré-importation si son artefact local est manquant. Une session `.rds` n'est ni une archive universelle des données brutes ni un substitut à un stockage organisé des fichiers sources.

## Reproductibilité et utilisation scientifique

TranscriptoShiny favorise la recherche reproductible grâce à l’export de rapports HTML/PDF paramétrés et de scripts R spécifiques à chaque domaine, qui récapitulent les étapes analytiques effectuées dans l’interface.

Le **rapport consolidé (4F)** va plus loin : les rapports sont des **compilateurs d'état + provenance**, jamais des ré-exécuteurs d'analyses. Le compilateur agrège les résultats des domaines **sans ré-exécution**, produit un HTML autonome et un bundle d'export (manifeste, tables de résultats fidèles, script R reproductible, sessionInfo). Aucune donnée brute n'est embarquée par défaut. La provenance est obligatoire pour toute section du rapport consolidé — le validateur rejette la provenance incomplète. Le contrat consolidé couvre **11 domaines de résultats** (dont pseudobulk et corrélation génique) ; le rapport de domaine Single-Cell restitue en option les tables canoniques vitesse ARN, communication cellulaire et abondance différentielle (tables pures, aucun recalcul).

**Environnement gelé :** `renv` est la voie supportée pour figer l'environnement (voir `RENV_SETUP.md` + `scripts/renv_bootstrap.R`). Les paquets GitHub-only (BPCells, spacexr, schard, …) s'installent via `renv::install()`.

La reproductibilité et la validité scientifique dépendent également :

- Des fichiers d’entrée exacts et de leur formatage.
- Des versions précises de R, Bioconductor et des packages dépendants.
- De la version des bases d’annotation, par exemple `org.Hs.eg.db`, utilisée lors de l’analyse.
- Des paramètres explicitement choisis par l’utilisateur.

**Bonnes pratiques :**

- Enregistrez votre `sessionInfo()` lors du partage ou de la publication de résultats.
- Conservez et partagez le fichier `renv.lock` fourni avec vos fichiers sources, métadonnées, paramètres et exports de provenance pour reconstruire l'environnement d'analyse.
- Considérez l’application comme une aide analytique. Tous les résultats nécessitent une revue biologique et statistique indépendante ; TranscriptoShiny ne remplace ni un design d’étude rigoureux, ni le jugement en contrôle qualité, ni l’expertise du domaine.

### Types d'exports

| Export | Finalité |
| :--- | :--- |
| Rapport de domaine | Résumer l'analyse Bulk/Single-Cell/Spatial effectuée dans ce module. |
| Rapport Single-Cell consolidé | Compiler les résultats et la provenance disponibles sans relancer les calculs. |
| Script R reproductible | Documenter les paramètres et les étapes exécutées dans l'interface. |
| Tables de résultats | Exporter les résultats filtrés ou complets par module. |
| Bundle d'export | Regrouper manifeste, tables, script et session info là où proposé. |

> Un rapport exporté documente l'état et les résultats au moment de sa génération ; il ne remplace ni la conservation des données sources, de `renv.lock`, des métadonnées et des artefacts sur disque nécessaires aux grands jeux spatiaux.

## Qualité logicielle

- **Tests** : suite `testthat` + `shinytest2` e2e (référence V1.1.0-rc : **1873 PASS / 0 FAIL / 0 ERROR**, e2e 13 PASS, ~14 min ; inclut un test fonctionnel du pipeline auto SC sur fixture minima).
- **Garde anti-duplication** : `tools/check_duplication.R` (0 erreur).
- **Contrats figés** : chaque contrat de résultat est couvert par un test de gel — toute modification = code + test + doc simultanément.
- **Gates release** : `scripts/verify_release_gates.R` (lock valide, i18n sans doublon, aucun chemin local dans les fichiers suivis).
- **Docs release** : `docs/release/` (matrice de durcissement 14 catégories, limitations connues, compatibilité, baseline de performance).

## Feuille de route

- [x] Localisation de l'interface en anglais et support de l'internationalisation (bilingue fr/en live).
- [x] RNA Velocity, communication cellule-cellule (import CellChat/CellPhoneDB), abondance différentielle (Milo + scCODA + vues croisées), rapport consolidé SC + provenance.
- [x] Option Slingshot pour l'inférence de lignées (quand le paquet est installé).
- [x] Gates de release, suite de tests, renv, packaging V1.0.0.
- [x] Refonte UX/UI V1.1 : pipeline auto 1 clic mutualisé (panneau 0 dans les trois domaines), SC en 5 sections parent avec DA nidifiée, conteneur Spatial (accordéon d'étapes + résultats à droite), étapes Bulk numérotées, saut « Aller au mapping des IDs », GEO « Source publique (GEO) » — zéro changement de comportement scientifique.
- [ ] Choix élargis d'intégration single-cell au-delà de Harmony.
- [ ] Détection dédiée des doublets au sein du pipeline de QC.
- [ ] Génération native de communication cellule-cellule à partir des données brutes (actuellement import-only).
- [ ] Extension continue des workflows spatiaux et des analyses fondées sur des références.

## Contribuer

Les contributions sont les bienvenues. Si vous souhaitez contribuer :

1. Ouvrez d’abord une issue afin de discuter de la motivation biologique ou technique du changement.
2. Pour les rapports de bug, fournissez un exemple minimal reproductible, votre système d’exploitation, votre version de R et le message d’erreur complet.
3. Évitez d’ajouter de grands fichiers de données à Git ; utilisez des données synthétiques ou fortement sous-échantillonnées pour les tests.
4. Soumettez une pull request avec des commits clairs et ciblés.

## Obtenir de l’aide

En cas de problème, ouvrez une GitHub Issue en incluant :

- Votre système d’exploitation et votre version de R.
- Les versions des packages clés, par exemple Seurat, DESeq2 et BPCells.
- Le format des données d’entrée et un exemple minimal reproductible, si le partage des données le permet.
- Le message d’erreur complet de la console ou une capture d’écran de l’erreur dans l’interface.


## Licence

Ce projet est distribué sous licence MIT. Consultez le fichier `LICENSE` du dépôt pour le texte complet.

---

# English

**Status:** `V1.1.0-rc` — local transcriptomics platform covering single-cell RNA-seq, bulk RNA-seq, and spatial transcriptomics, with advanced analyses available depending on installed dependencies (RNA velocity, imported cell–cell communication, Milo/scCODA differential abundance, spatial deconvolution, reproducible reports). V1.1 is a UX/UI wave (mutualized 1-click auto pipeline, panel grouping, Spatial container) — **zero scientific behavior change**.
> Some advanced analyses rely on optional dependencies, GitHub packages, or a dedicated Python environment. Check the Installation section before running a workflow.

## Why TranscriptoShiny?

TranscriptoShiny is designed for iterative transcriptomic data exploration by biologists and bioinformaticians. It combines guided user interfaces, proactive validation messages, interactive visualizations, and reproducible reporting, all while maintaining strict user control over local data.

The application prioritizes:
- **Local-first execution**: Data remains on your workstation, ensuring privacy and control.
- **Progressive guidance**: Intuitive workflows for non-experts, without hiding critical analytical parameters from advanced users.
- **Scalability**: Memory-aware strategies (e.g., Seurat v5 sketching, BPCells disk-backed matrices) to handle large single-cell and spatial datasets on standard hardware.
- **Scientific rigor**: Built-in experimental-design safeguards for bulk differential expression and transparent, exportable analytical steps.
- **Traceability**: Provenance produced at each analysis step and compiled at report time (never reconstructed after the fact), with frozen result contracts covered by freeze tests.

*Language*: Fully bilingual French / English interface (sidebar switch). Live switching via shiny.i18n; French strings are the translation keys.

## Supported workflows

TranscriptoShiny provides dedicated, modular environments for three primary transcriptomic domains:
1. **Single-cell RNA-seq**: From raw 10x or matrix imports to clustering, annotation, marker discovery, and pathway analysis — plus RNA velocity, cell-cell communication, differential abundance (Milo/scCODA), and the consolidated report.
2. **Bulk RNA-seq**: From raw count matrices and metadata to differential expression, multi-contrast comparison, and functional enrichment.
3. **Spatial transcriptomics**: From Visium/Xenium/CosMx/Slide-seq imports to spatial clustering, deconvolution, multi-sample integration, and niche analysis.

## Application navigation

| Tab / area | Role |
| :--- | :--- |
| Import Data | Load Single-Cell, Bulk RNA, or Spatial files, or from the **public source (GEO)** (last entry of the menu). ID-mapping reminder + native "Go to ID mapping" jump button in the Single-Cell/Bulk/GEO imports. |
| Analyse Single-Cell | Five parent sections: **Preparation** (mapping, pipelines), **Analysis** (annotation, visualization, markers, correlation, pathways), **Dynamics** (trajectory, pseudobulk, RNA velocity), **Cell abundance** (inner tabs: A. design — B. Milo/scCODA — C. cross-views), **Deliverables** (reports/exports). |
| Analyse Bulk RNA | Numbered steps 1→4: QC & filtering pipeline (with a compact "Bulk Pipeline Summary" view), ID mapping, differential expression & pathways, then "4. Deliverables — Report & R Script". |
| Analyse Spatial | Sidebar container: numbered accordion steps on the left (one step open at a time), results on the right; dataset and daemon status always visible in the sidebar. Spatial pipeline, QC, clustering, deconvolution, visualization, multi-sample integration, niches, report/export. |
| System sidebar | Language switch, memory usage, RAM cleanup, memory/upload limits, loaded objects status, session save/load, built-in help. |

**Mutualized pipeline paradigm:** in all three domains, the **1-click automatic pipeline** is the first accordion panel (0.), announced by a paradigm badge at the top of each sidebar ("auto available" / "guided mode" / "async auto"). The step-by-step guided mode remains fully available; both paradigms share the same computations and the same state.

> Note: the **"Public source (GEO)"** entry is a data source, not a fourth modality. It accepts compatible `series_matrix.txt` files as local import as well as accession-based fetching; it is not an offline-only viewer.

## Key capabilities

### Single-cell RNA-seq
- **Flexible import**: Supports 10x directories, `.rds`, `.h5`, `.h5ad`, and `.loom` formats, preserving `orig.ident` for multi-sample workflows.
- **Gene identifier mapping**: Optional, robust conversion of Ensembl/Entrez IDs to gene symbols prior to analysis.
> Gene ID mapping depends on species, annotation version, and input ID quality. Unresolved, ambiguous, or duplicated IDs must be checked before biological interpretation; always keep original IDs in your exports.
- **Standard pipeline**: QC, normalization, highly variable feature selection, PCA, neighbor graph construction, clustering, UMAP, and optional t-SNE. Available as a **1-click automatic pipeline** (panel 0: optional ID mapping, QC, normalization/PCA/clustering/UMAP, t-SNE, then optional steps SingleR annotation, markers, gene correlation, ORA on top markers, trajectory) or panel-by-panel in guided mode.
> The Single-Cell interface groups its panels into five parent sections (Preparation / Analysis / Dynamics / Cell abundance / Deliverables). RNA velocity, cell–cell communication, and differential abundance require specific prerequisites and remain outside the automatic pipeline.
- **Batch correction**: Harmony-based integration when multiple samples or batches are present.

#### Multi-sample single-cell

The Single-Cell workflow is natively multi-sample (e.g. Control vs Treatment, Day 0 vs Day 10 — each sample is handled as a distinct entity, like the Spatial module):

- **Batched import**: multiple 10x folders, `.rds`, or `.h5` files can be imported in one session (Options A and B of the Single-Cell import). Each import becomes a distinct sample (`orig.ident`); as soon as ≥ 2 samples are loaded, a summary overview (sample, cells, genes, condition/batch when detectable) provides immediate visual confirmation that samples are recognized as distinct entities.
- **Batch correction**: Harmony is automatically applied on sample identity (`orig.ident`) as soon as ≥ 2 samples are detected — select "Harmony" as the pipeline reduction method.
- **Downstream compatibility**: pseudobulk, Milo, and scCODA leverage sample identity to avoid pseudoreplication (the replication unit is the sample, never the cell — see the experimental-design validation in "Cell abundance").

- **Memory-aware scaling**: Seurat v5 sketch workflows (`SketchData` with LeverageScore, `ProjectData`), BPCells-aware handling, targeted feature scaling, and stratified subsampling for costly exploratory tasks.
- **Annotation & exploration**: Automatic cell-type annotation via SingleR (celldex references), marker discovery (`FindAllMarkers`), gene correlation, pathway analysis, and diverse visualizations (embeddings, feature plots, violins, dot plots, heatmaps, ridge/stacked views).
- **RNA velocity**: strict import of pre-prepared velocity data (spliced/unspliced matrices and compatible results/vectors) aligned to the Seurat object. The app performs multi-state consistency checks, provides phase-portrait and vector visualizations, and exports PNG/PDF/CSV. It visualizes validated results without silently re-computing a velocity model.
- **Cell–cell communication (panel 8b)**: import-only of external results (CellChat table / object, CellPhoneDB means+p-values). Canonical 12-field table, exact identity matching, QC, exploratory views (dotplot, pathway heatmap, circular network), descriptive centrality, filters with provenance. No de-novo generation from raw counts (remains a future parking item).
- **Differential abundance (panels 8c–8f)**:
> Prerequisite: differential abundance analyses require metadata identifying the biological replication unit (e.g. sample_id, donor_id, patient_id) and an experimental condition. A cell cluster alone is not a biological replicate.
  - Experimental-design validation that **blocks pseudoreplication** (cells are never treated as biological replicates; hard floors on replicates per condition, cells per sample, etc.).
  - Milo (neighbourhood DA, seed recorded, graphSpatialFDR).
  - scCODA (sample-level compositional DA via explicit Python environment; pure-R convergence diagnostics on ESS / R-hat / divergences).
  - Cross-views (descriptive concordance categories between Milo & scCODA; no composite p-value).
- **Trajectory**: exploratory kNN-graph pseudotime **plus optional Slingshot lineage inference** (when the package is available). Hard cell-count ceiling.
- **Pseudobulk** utilities and a **consolidated Single-Cell report** (compiler of canonical state + provenance only — never re-executes analyses; HTML autonomous, export bundle with manifest, faithful tables, reproducible R script, sessionInfo).
- Provenance is recorded for every analysis step that feeds a report; sections lacking provenance are rejected by the validator.
- *Scientific note*: the default trajectory remains the lightweight graph pseudotime; Slingshot is offered as an optional engine when installed. Built-in doublet detection and cell-cycle regression are not yet integrated.

### Bulk RNA-seq
> For DESeq2, edgeR, and limma-voom, provide raw gene-level counts, ideally integer or integer-like, from a quantification tool compatible with count-based analysis. Do not use TPM, FPKM, CPM, log-counts, or batch-corrected matrices as input for differential expression.

- **Smart import**: Handles merged raw count matrices or per-sample count files, with automated metadata alignment and gene duplicate resolution.
- **GEO support**: Offline parsing of GEO `series_matrix.txt` files without requiring network access or `GEOquery`.
- **Exploratory QC**: Filtering, variance-stabilizing transformation (VST), PCA, scree plots, and sample-correlation heatmaps.
- **Differential expression**: Workflows powered by DESeq2, edgeR, and limma-voom.
> DESeq2, edgeR, and limma-voom are provided as distinct engines. Comparing them is useful for exploring robustness, but results must be interpreted with a clearly documented design, filters, normalization factors, and contrasts.
- **Design safeguards**: Proactive checks for confounding covariates, missing covariate values that invalidate the model, and covariates with only one observed level.
- **Contrast management**: Standard, user-defined, and pairwise contrasts, alongside multi-method comparison and rank-consensus exploration.
- **Visualization & enrichment**: Volcano plots, MA plots, heatmaps, multi-contrast Venn/UpSet comparisons, and ORA/GSEA pathway analysis.
- *Scientific note*: Pathway analysis results should be interpreted in the context of the chosen background universe and identifier mapping. Multi-method rank consensus is an exploratory aid, not a formal meta-analysis.

### Spatial transcriptomics
- **Broad import support**: Visium, Visium HD (supported layouts), Xenium, CosMx, and Slide-seq, subject to standard file-layout compatibility.
- **Disk-backed architecture**: Large spatial assays utilize BPCells to store count matrices on disk, while lightweight sketch/metadata representations support interactive analysis in RAM.
- **Asynchronous execution**: Heavy operations run via a `mirai` daemon pool (timeouts, health checks, per-dataset cache, daemon reset button), with task logs to prevent UI blocking.
> Asynchronous tasks remain attached to the local R session. Do not close RStudio or R while they are running. Computations are intentionally capped to limit RAM saturation and excessive process creation on workstations.
- **Spatial analysis**: Spatial QC, Moran-style spatially variable feature analysis, and lightweight neighborhood-aware spatial clustering. This approach is inspired by BANKSY principles but is not a full or interchangeable implementation of the original BANKSY package.
- **Deconvolution**: RCTD, reference label transfer, and LDA-style approaches. The reference is **shared** (prepared once in Import > Spatial, reused by RCTD and Label Transfer), with validation safeguards.
- **Multi-sample & niches**: Memory-aware sketch integration (with optional Harmony batch correction) and niche analysis based on local neighborhood composition.
- **Disk backend**: BPCells is the on-disk backend; only the RAM sketch is guaranteed portable in a saved session.
- **Advanced visualization**: Histology overlays, linked spatial/embedding views, lasso ROI selection, ROI marker exploration, and subset export.
- *Scientific note*: Deconvolution accuracy depends heavily on reference quality, platform compatibility, and tissue context. These methods are for research exploration and are not validated for clinical decision-making.

## Architecture

TranscriptoShiny uses a modular Shiny architecture. Reusable analytical logic is separated from the interface, modules orchestrate workflows, and exportable results retain the provenance needed for interpretation.
Development follows a tested, reproducibility-oriented approach: key analytical results are validated by automated tests before being exposed in the UI.

```text
TranscriptoShiny/
├── app.R / global.R
├── config/          # defaults.R, thresholds.R (single source of truth)
├── i18n/            # translation.json (fr/en)
├── R/
│   ├── core/        # state, validation, provenance, jobs, caching, io/pathway helpers
│   ├── sc/          # velocity, communication, abundance (Milo/scCODA/design/cross), trajectory, pipeline, plotting, bpcells, export
│   ├── bulk/        # import engine, report engine, helpers
│   ├── spatial/     # async (mirai), io, deconv prep/tasks, multi, niche, plotting, report, reference, stats, export
│   ├── reports/     # collector, validator, render, bundle
│   └── plotting/    # shared palettes
├── modules/         # import/, sc/, bulk/, bulk_de/, spatial/ (+ deconv sub-modules)
├── reports/         # Rmd templates (sc, bulk, spatial + child)
├── scripts/         # renv bootstrap and development/benchmark scripts
└── renv/            # lock + activate
```

Cross-module state flows through the shared `reactiveValues` (`global_data`); heavy spatial/async work runs on the `mirai` daemon pool. Modules read their parameters from `config/` and the declared thresholds — no hard-coded magic numbers.

## Installation

### 1. Clone the repository
```bash
git clone https://github.com/Bio-MG/ShinyApp---TranscriptoShiny.git
cd ShinyApp---TranscriptoShiny
```

### 2. Install dependencies (recommended: renv)
The project uses **renv** to restore a reproducible R environment. Work from the cloned repository root so that `.Rprofile` can activate `renv`.
In R or RStudio:
```r
setwd("path/to/ShinyApp---TranscriptoShiny")
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::restore()
```
If restore fails or on a new machine, run the bootstrap script from the repository root:
```r
source("scripts/renv_bootstrap.R")
```
> Important: always launch the app from the project root. `app.R` explicitly checks that the project `renv` library is active and fails with a clear error otherwise.

*Manual install (discouraged, developers only): this is not the supported way to reproduce the reference environment — prefer `renv::restore()`.*

```r
# Minimal CRAN core
install.packages(c("shiny", "bslib", "shinyjs", "shinyWidgets", "shinycssloaders",
                   "shiny.i18n", "bsicons", "DT", "plotly", "ggplot2", "dplyr",
                   "patchwork", "viridis", "future", "mirai", "igraph", "Matrix",
                   "RANN", "circlize", "rmarkdown", "zip", "fs", "scattermore"))

# Bioconductor
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install(c("Seurat", "SeuratObject", "SingleR", "celldex",
                       "SingleCellExperiment", "DESeq2", "edgeR", "limma",
                       "ComplexHeatmap", "slingshot"))
```

*Note: spatial, AnnData, RCTD and deconvolution dependencies must be installed separately; `renv::restore()` remains the supported full install.*

### Optional dependencies by feature

| Feature | Main dependency | Consequence if missing |
| :--- | :--- | :--- |
| Disk-backed spatial matrices | BPCells | No on-disk backend (everything in RAM) |
| RCTD deconvolution | spacexr | RCTD unavailable |
| Robust `.h5ad` reading | schard | Fallback to SeuratDisk or failure |
| Visium HD (parquet) | arrow | Incomplete HD import (lightweight alternative: nanoparquet) |
| Lineage inference | slingshot | Trajectory limited to graph pseudotime |
| scCODA | Python env + scCODA deps | scCODA unavailable (guided error) |
| Spatial WebGL views | leafgl | Degraded large point-cloud rendering |
*Note: Spatial workflows may require additional packages such as `BPCells`, `mirai`, `RANN`, and `spacexr`. scCODA requires a dedicated **Python 3.13** environment (`python_env_sccoda/` or the `TS_SCCODA_PYTHON` variable) — without it, a classed error with guidance is raised, with no silent fallback. Per-domain PDF requires TinyTeX; the consolidated 4F report produces standalone HTML without pandoc.*

### 3. Launch the application
```r
shiny::runApp()
```
Alternatively, open `app.R` in RStudio and click **Run App**.

### Recommended environment
| Component | Recommendation |
| :--- | :--- |
| **R Version** | 4.4.2 (single supported version) |
| **IDE** | RStudio (recommended for development and debugging) — open `SHINYAPP test.Rproj` to activate renv |
| **RAM (Routine)** | 16 GB minimum |
| **RAM (Large Data)** | 32 GB recommended for large single-cell or spatial datasets |
| **Compute** | CPU-only execution is fully supported; GPU is not required |
| **Storage** | Sufficient fast local disk space is critical for temporary files, reports, and BPCells-backed spatial data |

## Typical workflows

### Single-cell RNA-seq
1. Import compatible data (10x directories, `.rds`, `.h5`, `.h5ad`, or `.loom`).
2. (Optional) Map gene identifiers to standard symbols.
3. Run the pipeline (1-click automatic or guided): QC, normalization, PCA, clustering, and UMAP/t-SNE.
4. Annotate cell types using SingleR.
5. Explore markers, gene correlations, and pathway enrichments.
6. Export a parameterized report or a reproducible R script.

### Bulk RNA-seq
1. Import raw integer-like count matrices and corresponding metadata. *(Note: Do not use pre-normalized TPM/FPKM values for differential expression).*
2. (Optional) Map gene identifiers.
3. Apply filtering and variance-stabilizing transformation (VST); review PCA and sample-correlation QC.
4. Define the experimental design, checking for covariate warnings, and specify contrasts.
5. Run differential expression (DESeq2, edgeR, or limma-voom).
6. Explore results via volcano/MA plots, heatmaps, and multi-contrast Venn/UpSet comparisons.
7. Perform pathway analysis and export the multi-contrast HTML/PDF report or R script.

### Spatial transcriptomics
1. Import spatial data (Visium, Visium HD, Xenium, CosMx, or Slide-seq).
2. Review spatial QC metrics and apply spot/cell filters.
3. Run spatial clustering (neighborhood-aware) and/or Moran-style spatially variable feature analysis.
4. (Optional) Prepare and validate a single-cell reference dataset.
5. Run deconvolution (RCTD, label transfer, or LDA).
6. Visualize results with histology overlays, linked views, and lasso ROI selection.
7. (Optional) Perform multi-sample sketch integration or niche composition analysis.

## Working with large datasets

TranscriptoShiny includes specific safeguards to manage memory and compute limits on workstation-scale hardware:
- **Use sketch workflows**: For large single-cell or spatial datasets, enable the Seurat v5 sketching options or spatial sketch integration to avoid loading full-resolution matrices into RAM.
- **Respect disk-backed boundaries**: Do not attempt to force full-resolution spatial count matrices into memory; rely on the provided BPCells-backed asynchronous tasks.
- **Leverage previews**: Utilize preview/subsampled visualizations where offered in the UI, reserving full-fidelity exports for final reporting.
- **Manage temporary storage**: Ensure that your R `tempdir()` or configured cache location has adequate free disk space, especially for large `.h5` or `.h5ad` uploads and BPCells artifacts.
- **Pace heavy operations**: On a 32 GB workstation, run one heavy asynchronous analysis (e.g., spatial deconvolution or large-scale marker discovery) at a time.
- **Use fast local storage**: Keep raw input data, generated reports, and BPCells directories on fast local drives (e.g., NVMe SSD) rather than network-mounted volumes.

### System sidebar

- Display memory usage and loaded object status.
- Trigger explicit memory cleanup.
- Adjust memory limit per parallel task.
- Adjust maximum upload size.
- Check whether SC/Bulk/Spatial objects are loaded and whether the main pipeline has run.
- Save/restore a working session.
- Switch interface language between French and English.

> On a 32 GB machine, do not allocate all physical RAM to a single task.

### Session save and restore

The sidebar allows saving/loading a `.rds` session. SC, Bulk, and the spatial sketch are serialized. For BPCells-backed spatial data, full matrices remain linked to on-disk artifacts. After restoring on another machine or after moving/cleaning files, the spatial sketch may remain viewable but clustering/deconvolution may require re-import. The shared scRNA-seq reference (RCTD/label transfer) may also need re-import if its local artifact is missing. A `.rds` session is neither a universal archive of raw data nor a substitute for organized source file storage.

## Reproducibility and scientific use

TranscriptoShiny supports reproducible research by exporting parameterized HTML/PDF reports and domain-specific R scripts that recapitulate the analytical steps performed in the UI.

The **consolidated report (4F)** goes further: reports are **compilers of state + provenance**, never re-runners of analyses. The compiler aggregates cross-domain results **without re-execution**, producing standalone HTML and an export bundle (manifest, faithful result tables, reproducible R script, sessionInfo). No raw data is embedded by default. Provenance is mandatory for any section that appears in a consolidated report — the validator rejects incomplete provenance. The consolidated contract covers **11 result domains** (including pseudobulk and gene correlation); the Single-Cell domain report optionally renders the canonical RNA velocity, cell–cell communication, and differential abundance tables (pure tables, no recomputation).

**Frozen environment:** `renv` is the supported way to freeze the environment (see `RENV_SETUP.md` + `scripts/renv_bootstrap.R`). GitHub-only packages (BPCells, spacexr, schard, …) must be installed via `renv::install()`.

However, true reproducibility and scientific validity also depend on:
- The exact input files and their formatting.
- The specific versions of R, Bioconductor, and dependent packages used.
- The version of annotation databases (e.g., `org.Hs.eg.db`) at the time of analysis.
- The parameters explicitly chosen by the user.

**Best practices**:
- Record your `sessionInfo()` when sharing or publishing results.
- Keep and share the provided `renv.lock` file together with your source files, metadata, parameters, and provenance exports to reconstruct the analytical environment.
- Treat the application as an analytical aid. All results require independent biological and statistical review; TranscriptoShiny does not replace rigorous study design, quality control judgment, or domain expertise.

### Export types

| Export | Purpose |
| :--- | :--- |
| Domain report | Summarize Bulk/Single-Cell/Spatial analysis performed in that module. |
| Consolidated Single-Cell report | Compile available results and provenance without re-running computations. |
| Reproducible R script | Document parameters and steps executed in the UI. |
| Result tables | Export filtered or complete results per module. |
| Export bundle | Group manifest, tables, script, and session info where offered. |

> An exported report documents the state and results at generation time; it does not replace conservation of source data, `renv.lock`, metadata, and on-disk artifacts needed for large spatial datasets.

## Software quality

- **Tests**: `testthat` suite + `shinytest2` e2e (V1.1.0-rc baseline: **1873 PASS / 0 FAIL / 0 ERROR**, e2e 13 PASS, ~14 min; includes a functional test of the SC auto pipeline on a minimal fixture).
- **Duplication gate**: `tools/check_duplication.R` (0 errors).
- **Frozen contracts**: each result contract is covered by a freeze test — any change = code + test + doc simultaneously.
- **Release gates**: `scripts/verify_release_gates.R` (valid lockfile, duplicate-free i18n, no local paths in tracked files).
- **Release docs**: `docs/release/` (14-category hardening matrix, known limitations, compatibility, performance baseline).

## Roadmap

- [x] English UI localization and internationalization support (live fr/en bilingual).
- [x] RNA velocity, cell-cell communication (CellChat/CellPhoneDB import), differential abundance (Milo + scCODA + cross-views), consolidated SC report + provenance.
- [x] Slingshot option for lineage inference (when the package is installed).
- [x] Release gates, test suite, renv, V1.0.0 packaging.
- [x] V1.1 UX/UI overhaul: mutualized 1-click auto pipeline (panel 0 in all three domains), SC grouped into 5 parent sections with nested DA, Spatial container (step accordion + results on the right), numbered Bulk steps, "Go to ID mapping" jump, GEO as "Public source (GEO)" — zero scientific behavior change.
- [ ] Expanded single-cell integration choices beyond Harmony.
- [ ] Dedicated doublet-detection support within the QC pipeline.
- [ ] Native cell-cell communication generation from raw data (currently import-only).
- [ ] Continued expansion of spatial workflows and reference-based analyses.

## Contributing

Contributions are welcome. If you wish to contribute:
1. Please open an issue first to discuss the biological or technical motivation for the change.
2. For bug reports, provide a minimal reproducible example, your OS, R version, and the complete error message.
3. Avoid committing large data files to Git; use synthetic or heavily subsampled data for testing.
4. Submit a pull request with clear, focused commits.

## Getting help

If you encounter issues, please open a GitHub Issue and include:
- Your operating system and R version.
- The versions of key packages (e.g., Seurat, DESeq2, BPCells).
- The input data format and a minimal reproducible example (if data sharing permits).
- The complete console error message or a screenshot of the UI error.


## License

This project is distributed under the MIT License. See the `LICENSE` file in the repository for full details.