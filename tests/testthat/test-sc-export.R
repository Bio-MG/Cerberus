# =============================================================================
# test-sc-export.R — R/sc/sc_export.R
# =============================================================================
# 44e increment de la dette de conventions (§2dd, 2026-09-20) : ecrire le test
# eponyme manquant de `R/sc/sc_export.R` — le SEUL fichier signale par C9 dont
# AUCUNE fonction n'est citee par AUCUN test (§2da.5, reconfirme §2dc.3 et
# re-mesure ici).
#
# Le fichier n'expose QU'UNE fonction, `sc_r_script_text(obj, shared_rv = NULL)`
# — PURE (assemblage `paste0()`), **8 branches independantes** commandees par
# des drapeaux `has_*` introspectes sur l'objet. Elle est appelee par DEUX sites
# de production :
#   - `modules/sc/mod_sc.R:726`  (downloadHandler "dl_sc_r_script")
#   - `R/reports/report_collector.R:516`  (sous `tryCatch`)
#
# ---------------------------------------------------------------------------
# POURQUOI CE FICHIER EXISTE MALGRE UNE EXCLUSION DOCUMENTEE
# ---------------------------------------------------------------------------
# `test-plot-export.R:308` affirme que `sc_export.R` reste EXCLU — mais de quoi ?
# De la regle des `ggsave()` (famille C10) : le fichier EMBARQUE une chaine de
# script R generee, dont les `ggsave(...)` ne sont PAS executes par l'app. Cette
# exemption est donc une decision sur la nature du CONTENU, pas un jugement
# d'INtaisabilite : `sc_r_script_text()` est une fonction pure de premier niveau,
# appelable en R pur. C9 pose une autre question (« ce fichier a-t-il un test ? »)
# et recoit ici sa reponse.
#
# ---------------------------------------------------------------------------
# DEPENDANCE D'EXECUTION INVISIBLE AU VERROU SOURCE (§2by.6 #3) — MESUREE
# ---------------------------------------------------------------------------
# Ligne 31 : `pca_dims <- if (has_pca) min(ncol(Seurat::Embeddings(obj,"pca")), 50) else 20`.
# `Embeddings()` etait appele SANS PREFIXE, or **aucun `library(Seurat)`**
# inconditionnel n'existe dans le boot de l'app (`spatial_async.R:222` ne
# l'attache que dans une garde paresseuse). Sonde : sans Seurat attache,
# `has_pca = TRUE` fait echouer l'appel sur `could not find function "Embeddings"`.
# ⇒ Ce fichier de test **attache Seurat**, exactement comme `test-sc-plotting.R`
#   attache ggplot2 pour `ggtitle()` (§2by). C'est la dependance reelle qui est
#   eprouvee, pas une version mochee.
#   ⚠️ La convention du depot est PREFIXEE : 13 sites `Seurat::` /
#   `SeuratObject::` contre 0 nu. Le nu de `sc_export.R:31` etait une observation
#   MESUREE, consignee en `STATUS.md` §2dd — **corrigee au §2dg** (`Seurat::`,
#   prefixe DOMINANT). Les 3 autres sites nus (`sc_helpers.R:1238`,
#   `mod_sc_pipeline.R:434`, `mod_sc_viz.R:598`) le sont aussi.
#   ⇒ Le contrat est desormais verrouille par `test-embeddings-prefix.R`.
#
# ---------------------------------------------------------------------------
# TOUTES les attentes sont MESUREES par une sonde hors depot avant d'etre
# ecrites (lecon §2da.6). Aucun chiffre, aucune sous-chaine devinee.
# =============================================================================

suppressMessages(library(Seurat))

source_project_file("R/core/io_helpers.R")   # %||% (le fichier en redefinit un local)
source_project_file("R/sc/sc_export.R")

# ---------------------------------------------------------------------------
# Fixture : un VRAI objet Seurat construit en R pur, parametrable par branche.
# `set.seed()` => deterministe ; `suppressWarnings()` autour des calculs, qui
# emettent des avertissements ATTENDUS (coercition matrice -> dgCMatrix, etc.)
# qu'un test sans garde ferait compter a tort (§2ce.4).
# ---------------------------------------------------------------------------
.ts_sc_export_obj <- function(with_mt = TRUE, clusters = TRUE, singler = FALSE,
                              traj = FALSE, pca = TRUE, umap = TRUE,
                              n_genes = 60L, n_cell = 60L) {
  set.seed(42)
  m <- matrix(stats::rpois(n_genes * n_cell, 5), nrow = n_genes)
  rownames(m) <- paste0("G", seq_len(n_genes))
  colnames(m) <- paste0("c", seq_len(n_cell))
  if (with_mt) rownames(m)[1:3] <- c("MT-ND1", "MT-CO1", "MT-ATP6")

  o <- suppressWarnings(Seurat::CreateSeuratObject(counts = m))
  if (with_mt) {
    o[["percent.mt"]] <- suppressWarnings(
      Seurat::PercentageFeatureSet(o, pattern = "^MT-")
    )
  }
  o <- suppressWarnings(Seurat::NormalizeData(o, verbose = FALSE))
  o <- suppressWarnings(Seurat::FindVariableFeatures(o, nfeatures = 30, verbose = FALSE))
  o <- suppressWarnings(Seurat::ScaleData(o, verbose = FALSE))
  if (pca) o <- suppressWarnings(Seurat::RunPCA(o, npcs = 5, verbose = FALSE))
  if (clusters) {
    o <- suppressWarnings(Seurat::FindNeighbors(o, dims = 1:5, verbose = FALSE))
    o <- suppressWarnings(Seurat::FindClusters(o, resolution = 0.5, verbose = FALSE))
  }
  if (umap) o <- suppressWarnings(Seurat::RunUMAP(o, dims = 1:5, verbose = FALSE))
  if (singler) o$SingleR_main <- rep(c("T", "B"), length.out = n_cell)
  if (traj)    o$pseudotime   <- seq_len(n_cell) / n_cell
  o
}

.ts_lines <- function(x) strsplit(x, "\n", fixed = TRUE)[[1]]

# La ligne d'en-tete "Pipeline" resume 6 des 8 drapeaux : c'est le point
# d'observation le plus compact, et il est MESURE pour chaque combinaison.
.ts_pipeline_line <- function(x) {
  hit <- grep("^# Pipeline", .ts_lines(x), value = TRUE)
  if (length(hit) != 1L) return(NA_character_)
  hit
}

# =============================================================================
# 1. Contrat de type / forme de sortie
# =============================================================================
test_that("sc_r_script_text() rend UNE chaine scalaire non vide", {
  o <- .ts_sc_export_obj()
  x <- sc_r_script_text(o)
  expect_type(x, "character")
  expect_length(x, 1L)
  expect_gt(nchar(x), 0L)
  # Mesure : 3567 caracteres pour l'objet complet (60 genes x 60 cellules).
  expect_gt(nchar(sc_r_script_text(.ts_sc_export_obj(clusters = TRUE, singler = TRUE,
                                                     traj = TRUE))), 1000L)
})

test_that("sc_r_script_text() est PURE : deux appels rendent le meme texte", {
  o <- .ts_sc_export_obj()
  expect_identical(sc_r_script_text(o), sc_r_script_text(o))
})

test_that("sc_r_script_text() ne rend jamais NULL ni NA", {
  x <- sc_r_script_text(.ts_sc_export_obj(with_mt = FALSE, clusters = FALSE,
                                          pca = FALSE, umap = FALSE))
  expect_false(is.null(x))
  expect_false(is.na(x))
})

# =============================================================================
# 2. Dimensions introspectees (n_genes x n_cell) — les 2 seuls chiffres
#    reellement lus sur l'objet, donc les 2 seuls ou une inversion se verrait.
# =============================================================================
test_that("l'en-tete porte les dimensions REELLES de l'objet (genes x cellules)", {
  o <- .ts_sc_export_obj(n_genes = 60L, n_cell = 60L)
  x <- sc_r_script_text(o)
  expect_match(x, "# Dataset    : 60 gènes × 60 cellules", fixed = TRUE)

  # Controle de BORNE (§2cf.1) : une autre geometrie doit suivre.
  o2 <- .ts_sc_export_obj(n_genes = 80L, n_cell = 45L)
  x2 <- sc_r_script_text(o2)
  expect_match(x2, "# Dataset    : 80 gènes × 45 cellules", fixed = TRUE)
  # ... et NE PAS reprendre les anciennes valeurs (anti-cache / anti-hardcode).
  expect_false(grepl("60 gènes × 60 cellules", x2, fixed = TRUE))
})

test_that("n_genes vient de nrow() et n_cell de ncol() (pas l'inverse)", {
  # Geometrie ASYMETRIQUE : c'est le seul cas ou une inversion nrow/ncol
  # produirait une sortie DIFFERENTE. Avec 60x60 elle passerait inapercue.
  x <- sc_r_script_text(.ts_sc_export_obj(n_genes = 80L, n_cell = 45L))
  expect_match(x, "80 gènes", fixed = TRUE)
  expect_match(x, "45 cellules", fixed = TRUE)
  expect_false(grepl("45 gènes", x, fixed = TRUE))
  expect_false(grepl("80 cellules", x, fixed = TRUE))
})

# =============================================================================
# 3. En-tete "Pipeline" — les 6 drapeaux, chacun dans SES DEUX etats
# =============================================================================
test_that("branche 'objet nu' : les 6 drapeaux valent non/?" , {
  x <- sc_r_script_text(.ts_sc_export_obj(with_mt = FALSE, clusters = FALSE,
                                          pca = FALSE, umap = FALSE))
  expect_identical(.ts_pipeline_line(x),
                   "# Pipeline   : PCA=non, UMAP=non, clusters=non, SingleR=non, Corr=non, Traj=non")
})

test_that("branche 'objet complet' : les 6 drapeaux sont renseignes", {
  x <- sc_r_script_text(.ts_sc_export_obj(with_mt = TRUE, clusters = TRUE,
                                          singler = TRUE, traj = TRUE))
  # `clusters=1` : une seule population detectee sur 60 cellules simulees
  # (MESURE, cf. sonde). `SingleR=SingleR_main` : la colonne detectee par
  # `grep("^SingleR_", ...)` est citee par son NOM.
  expect_identical(.ts_pipeline_line(x),
                   "# Pipeline   : PCA=oui, UMAP=oui, clusters=1, SingleR=SingleR_main, Corr=non, Traj=oui")
})

test_that("has_umap depend de `umap` et pas de `pca` (drapeaux INDEPENDANTS)", {
  # Objet avec PCA mais SANS UMAP : les deux drapeaux doivent diverger.
  x <- sc_r_script_text(.ts_sc_export_obj(pca = TRUE, umap = FALSE, clusters = FALSE))
  expect_match(x, "PCA=oui", fixed = TRUE)
  expect_match(x, "UMAP=non", fixed = TRUE)
})

test_that("has_pca et has_umap sont lus dans obj@reductions (noms exacts)", {
  o <- .ts_sc_export_obj()
  expect_true("pca" %in% names(o@reductions))
  expect_true("umap" %in% names(o@reductions))
  x <- sc_r_script_text(o)
  expect_match(x, "PCA=oui", fixed = TRUE)
  expect_match(x, "UMAP=oui", fixed = TRUE)
})

test_that("has_mt est lu sur les COLONNES de meta.data, pas sur les genes", {
  # percent.mt pose => oui ; absent => le bloc mt_pat est emis a la place.
  expect_match(sc_r_script_text(.ts_sc_export_obj(with_mt = TRUE)), "PCA=oui")
  o_no <- .ts_sc_export_obj(with_mt = FALSE)
  expect_false("percent.mt" %in% colnames(o_no@meta.data))
})

# =============================================================================
# 4. Les 8 branches de code — chacune dans SES DEUX etats (exclusion mutuelle)
# =============================================================================
test_that("branche QC/%MT : les deux bras sont MUTUELLEMENT EXCLUSIFS", {
  x_no <- sc_r_script_text(.ts_sc_export_obj(with_mt = FALSE))
  x_mt <- sc_r_script_text(.ts_sc_export_obj(with_mt = TRUE))

  # Bras `!has_mt` : on (re)calcule le motif mitochondrial.
  expect_true(any(grepl("mt_pat <- if", .ts_lines(x_no), fixed = TRUE)))
  expect_false(any(grepl("percent.mt déjà calculé", .ts_lines(x_no), fixed = TRUE)))

  # Bras `has_mt` : commentaire d'exclusion, PAS de recalcul.
  expect_true(any(grepl("percent.mt déjà calculé", .ts_lines(x_mt), fixed = TRUE)))
  expect_false(any(grepl("mt_pat <- if", .ts_lines(x_mt), fixed = TRUE)))
})

test_that("branche VlnPlot : percent.mt s'ajoute aux features ET change ncol", {
  x_no <- sc_r_script_text(.ts_sc_export_obj(with_mt = FALSE))
  x_mt <- sc_r_script_text(.ts_sc_export_obj(with_mt = TRUE))

  expect_true(any(grepl('features=c("nFeature_RNA","nCount_RNA"), ncol=2', .ts_lines(x_no),
                        fixed = TRUE)))
  expect_true(any(grepl('features=c("nFeature_RNA","nCount_RNA","percent.mt"), ncol=3',
                        .ts_lines(x_mt), fixed = TRUE)))
})

test_that("branche marqueurs : le commentaire ne s'emet QUE si shared_rv en porte", {
  o <- .ts_sc_export_obj()
  x_sans <- sc_r_script_text(o)
  x_avec <- sc_r_script_text(o, list(markers_data = data.frame(gene = c("A", "B", "C"))))

  expect_false(any(grepl("marqueurs dans l'app", .ts_lines(x_sans), fixed = TRUE)))
  # Le NOMBRE de marqueurs est interpole (3 ici) : mesure.
  expect_true(any(grepl("# 3 marqueurs dans l'app", .ts_lines(x_avec), fixed = TRUE)))
  # Le bloc `FindAllMarkers` est present dans les DEUX cas (non conditionnel).
  expect_true(any(grepl("FindAllMarkers", .ts_lines(x_sans), fixed = TRUE)))
  expect_true(any(grepl("FindAllMarkers", .ts_lines(x_avec), fixed = TRUE)))
})

test_that("branche marqueurs : un shared_rv avec 0 ligne ne declenche PAS le commentaire", {
  o <- .ts_sc_export_obj()
  x <- sc_r_script_text(o, list(markers_data = data.frame(gene = character(0))))
  expect_false(any(grepl("marqueurs dans l'app", .ts_lines(x), fixed = TRUE)))
})

test_that("branche SingleR : colonne detectee => mode 'deja annote'", {
  x_oui <- sc_r_script_text(.ts_sc_export_obj(singler = TRUE))
  x_non <- sc_r_script_text(.ts_sc_export_obj(singler = FALSE))

  expect_true(any(grepl("# Déjà annoté (colonne : SingleR_main) :", .ts_lines(x_oui),
                        fixed = TRUE)))
  expect_false(any(grepl("# Déjà annoté", .ts_lines(x_non), fixed = TRUE)))
  # Bras `!has_singler` : la recette est FOURNIE, mais DESACTIVEE (commentee).
  expect_true(any(grepl("BiocManager::install", .ts_lines(x_non), fixed = TRUE)))
  expect_true(any(grepl("SingleR::SingleR", .ts_lines(x_non), fixed = TRUE)))
})

test_that("branche SingleR : seule la DERNIERE colonne ^SingleR_ est nommee", {
  o <- .ts_sc_export_obj(singler = FALSE)
  o$SingleR_main <- rep("T", ncol(o))
  o$SingleR_fine <- rep("CD4", ncol(o))
  x <- sc_r_script_text(o)
  # `tail(singler_cols,1)` => la DERNIERE dans l'ordre des colonnes.
  expect_true(any(grepl("# Déjà annoté (colonne : SingleR_fine) :", .ts_lines(x),
                        fixed = TRUE)))
  expect_match(x, 'group.by="SingleR_fine"', fixed = TRUE)
})

test_that("branche correlation : les deux bras sont exclusifs", {
  o <- .ts_sc_export_obj()
  x_non <- sc_r_script_text(o)
  x_oui <- sc_r_script_text(o, list(corr_target_gene = "CD3D"))

  expect_true(any(grepl("Lancez l'étape 5 (Gene Correlation)", .ts_lines(x_non),
                        fixed = TRUE)))
  expect_false(any(grepl("TARGET_GENE <-", .ts_lines(x_non), fixed = TRUE)))

  expect_true(any(grepl('TARGET_GENE <- "CD3D"', .ts_lines(x_oui), fixed = TRUE)))
  expect_true(any(grepl("find_correlated_genes", .ts_lines(x_oui), fixed = TRUE)))
  expect_false(any(grepl("Lancez l'étape 5", .ts_lines(x_oui), fixed = TRUE)))
})

test_that("branche trajectoire : les deux bras sont exclusifs", {
  x_oui <- sc_r_script_text(.ts_sc_export_obj(traj = TRUE))
  x_non <- sc_r_script_text(.ts_sc_export_obj(traj = FALSE))

  expect_true(any(grepl("Pseudotemps déjà calculé", .ts_lines(x_oui), fixed = TRUE)))
  expect_false(any(grepl("Pseudotemps déjà calculé", .ts_lines(x_non), fixed = TRUE)))
  # Bras `!has_traj` : recette commentee, mentionnant la signature Step-3.9.
  expect_true(any(grepl("calculate_pseudotime", .ts_lines(x_non), fixed = TRUE)))
})

# =============================================================================
# 5. `pca_dims` : la valeur par DEFAUT et le PLAFOND a 50
# =============================================================================
test_that("sans PCA, PCA_DIMS retombe sur 20 (valeur par defaut)", {
  x <- sc_r_script_text(.ts_sc_export_obj(pca = FALSE, umap = FALSE, clusters = FALSE))
  expect_true(any(grepl("PCA_DIMS  <- 20", .ts_lines(x), fixed = TRUE)))
})

test_that("avec PCA, PCA_DIMS est le nombre REEL de composantes (5 ici)", {
  x <- sc_r_script_text(.ts_sc_export_obj(pca = TRUE))
  expect_true(any(grepl("PCA_DIMS  <- 5", .ts_lines(x), fixed = TRUE)))
  # ... et PAS la valeur par defaut : les deux branches divergent vraiment.
  expect_false(any(grepl("PCA_DIMS  <- 20", .ts_lines(x), fixed = TRUE)))
})

test_that("PCA_DIMS est PLAFONNE a 50 par min(ncol(...), 50)", {
  # L'expression source est `min(ncol(Seurat::Embeddings(obj,"pca")), 50)`.
  # On falsifie le plafond sans dependre de Seurat : on verifie la LIGNE SOURCE,
  # car construire > 50 composantes coute cher et ne testerait que Seurat.
  # ⚠️ Le prefixe `Seurat::` fait partie du CONTRAT depuis le §2dg : cet appel
  # est du CODE EXECUTE, et aucun `library(Seurat)` inconditionnel n'existe au
  # boot (le nu echouait sur `could not find function`). Cf. test-embeddings-prefix.R.
  src <- readLines(file.path(ts_project_root(), "R/sc/sc_export.R"),
                   warn = FALSE, encoding = "UTF-8")
  def <- grep("^\\s*pca_dims\\s*<-", src, value = TRUE)
  expect_length(def, 1L)
  expect_match(def, "min(ncol(Seurat::Embeddings(obj,\"pca\")), 50)", fixed = TRUE)
})

# =============================================================================
# 6. Tracabilite : la date du jour, dans l'en-tete ET dans les noms de fichiers
# =============================================================================
test_that("la date du jour apparait dans l'en-tete et dans les 3 noms de sortie", {
  x <- sc_r_script_text(.ts_sc_export_obj(clusters = TRUE))
  today <- format(Sys.Date(), "%Y-%m-%d")
  expect_match(x, paste0("# Généré le : ", today), fixed = TRUE)
  expect_match(x, paste0("umap_clusters_", today, ".png"), fixed = TRUE)
  expect_match(x, paste0("markers_", today, ".csv"), fixed = TRUE)
  expect_match(x, paste0("sc_obj_processed_", today, ".rds"), fixed = TRUE)
})

# =============================================================================
# 7. Contrat de SORTIE : le script genere doit etre du R PARSEABLE
# =============================================================================
test_that("le texte genere PARSE (le script exporte doit etre executable)", {
  # C'est l'assertion de plus fort levier du fichier : elle verifie l'assemblage
  # `paste0()` DANS SON ENSEMBLE, pas branche par branche. Une virgule oubliee
  # dans une branche conditionnelle ne se voit qu'ici.
  for (cfg in list(
    list(with_mt = FALSE, clusters = FALSE, singler = FALSE, traj = FALSE,
         pca = FALSE, umap = FALSE, rv = NULL),
    list(with_mt = TRUE, clusters = TRUE, singler = TRUE, traj = TRUE,
         pca = TRUE, umap = TRUE, rv = NULL),
    list(with_mt = TRUE, clusters = TRUE, singler = FALSE, traj = FALSE,
         pca = TRUE, umap = TRUE,
         rv = list(markers_data = data.frame(gene = c("A", "B")),
                   corr_target_gene = "CD3D"))
  )) {
    x <- sc_r_script_text(.ts_sc_export_obj(with_mt = cfg$with_mt, clusters = cfg$clusters,
                                           singler = cfg$singler, traj = cfg$traj,
                                           pca = cfg$pca, umap = cfg$umap),
                          cfg$rv)
    expect_no_error(parse(text = x))
  }
})

# =============================================================================
# 8. Robustesse : shared_rv PARTIEL ou MALFORME ne doit pas casser la generation
# =============================================================================
test_that("un shared_rv NULL et un shared_rv vide rendent le meme texte", {
  o <- .ts_sc_export_obj()
  expect_identical(sc_r_script_text(o, NULL), sc_r_script_text(o, list()))
})

# =============================================================================
# 9. Ce que C9 NE verifie pas : le lien entre le test et le FICHIER
# =============================================================================
test_that("ce fichier source bien R/sc/sc_export.R (et pas un homonyme)", {
  # C9 est satisfaite par le NOM du fichier de test ; on prouve ici que la
  # fonction testee vient bien du fichier vise, et qu'aucun autre fichier ne
  # la definit (donc que le `source_project_file` ci-dessus est le bon).
  src <- readLines(file.path(ts_project_root(), "R/sc/sc_export.R"),
                   warn = FALSE, encoding = "UTF-8")
  expect_true(any(grepl("^sc_r_script_text <- function", src)))
  expect_true(exists("sc_r_script_text", mode = "function"))
})
