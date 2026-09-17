# =============================================================================
# test-sc-plotting.R — R/sc/sc_plotting.R
# =============================================================================
# 14ᵉ incrément de la dette de conventions : classer les `stop()` du fichier
# avec `errorCondition(<msg>, class = "sc_plotting_error")`.
#
# Portée : les 17 sites C10 de `R/sc/sc_plotting.R`. Le fichier n'expose
# QU'UNE fonction publique (`build_sc_viz_plot`), un **dispatcheur sur `type`** —
# pure (C2 interdit Shiny dans `R/`), donc appelable en R pur ⇒ la **classe est
# observable**, sans `testServer()`.
#
# ⚠️ Ce fichier est exigé par **C9** (aucun test éponyme n'existait) : il paie
# donc **deux fois** — C9 −1 et C10 −17.
#
# 🟢 **16 sites sur 17 sont joignables** — le meilleur ratio du chantier
# (§2bq 44 %, §2bw 50 %, §2bx 71 %). Seul le **201** ne l'est pas : il exige
# que `FindMarkers()` **réussisse** et rende **0 ligne**, ce qu'un objet
# dégénéré ne produit pas.
#
# ⚠️ ANTI-TRONCATURE (§2bn, §2bx.3) : **5** sites sont multi-arguments
# (63, 194, 200, 278, 284) ⇒ `paste0()` obligatoire. Les assertions portent sur
# le message **ENTIER**, pas sur un préfixe — une assertion-préfixe passerait
# même tronqué. Cas **200** : la queue du message est **interne à Seurat** donc
# volatile ; on assère le préfixe **et** une longueur strictement supérieure à
# celle du préfixe nu, ce qui **suffit** à détecter la troncature.
# =============================================================================

source_project_file("R/core/io_helpers.R")     # %||%
source_project_file("R/plotting/palettes.R")   # sc_discrete_scale / sc_continuous_scale
source_project_file("R/plotting/theme.R")      # ts_theme
source_project_file("R/sc/sc_plotting.R")

skip_if_not_installed("Seurat")
# ⚠️ suppressWarnings() aussi : sinon « package … was built under R version
# 4.4.3 » (bruit d'environnement) compterait comme un WARN du test.
suppressWarnings(suppressPackageStartupMessages(library(Seurat)))

.viz_catch <- function(expr) {
  tryCatch({ expr; NULL },
           error = function(e) list(msg = conditionMessage(e), class = class(e)))
}

# Objet Seurat minimal : 3 gènes x n cellules, `seurat_clusters` + `orig.ident`.
# ⚠️ dgCMatrix directe : CreateSeuratObject() prévient sinon (« coercing to
# dgCMatrix ») à chaque appel et polluerait la sortie.
.viz_obj <- function(n = 6L, clusters = c("0", "1"), idents = "A") {
  skip_if_not_installed("Matrix")
  cnt <- Matrix::Matrix(matrix(1L, nrow = 3L, ncol = n,
                               dimnames = list(c("G1", "G2", "G3"),
                                               paste0("c", seq_len(n)))),
                        sparse = TRUE)
  md <- data.frame(row.names = paste0("c", seq_len(n)),
                   seurat_clusters = factor(rep(clusters, length.out = n)),
                   orig.ident      = rep(idents, length.out = n))
  SeuratObject::CreateSeuratObject(counts = cnt, meta.data = md)
}

.viz_expect <- function(e, msg) {
  expect_identical(e$msg, msg)
  expect_true("sc_plotting_error" %in% e$class)
}

# ---------------------------------------------------------------------------
# Verrou source : le fichier ne doit plus contribuer UN SEUL signalement C10.
# La détection est celle de la garde elle-même (jamais réimplémentée).
# ---------------------------------------------------------------------------
test_that("sc_plotting.R ne contribue aucun signalement C10 (verrou source)", {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools/check_conventions.R"), envir = e)
  before <- length(e$.REPORT$warns)
  e$check_c10_error_style(file.path(ts_project_root(), "R/sc/sc_plotting.R"))
  w <- e$.REPORT$warns
  n <- length(w) - before
  if (n > 0L) {
    lignes <- vapply(w[(before + 1L):length(w)], function(x) x$line, integer(1))
    cat("Sites C10 restants dans sc_plotting.R :",
        paste(sort(lignes), collapse = ", "), "\n")
  }
  expect_equal(n, 0L)
})

# ---------------------------------------------------------------------------
# « Aucun gène valide » — 6 branches partagent le même garde (73, 96, 111,
# 123, 131, 150) : un type différent, le même message entier.
# ---------------------------------------------------------------------------
test_that("branches à gènes : aucun gène valide (73, 96, 111, 123, 131, 150)", {
  obj <- .viz_obj()
  for (tp in c("feature", "violin", "stacked_violin", "ridge", "dot", "heatmap")) {
    e <- .viz_catch(build_sc_viz_plot(obj, list(type = tp, feat_sel = "NOPE")))
    .viz_expect(e, "Aucun gène valide")
  }
})

test_that("heatmap_hier : aucun gène valide sélectionné (258)", {
  obj <- .viz_obj()
  e <- .viz_catch(build_sc_viz_plot(obj, list(type = "heatmap_hier", feat_sel = "NOPE")))
  .viz_expect(e, "Aucun gène valide sélectionné pour la heatmap")
})

test_that("correlation_matrix : au moins 2 gènes requis (172)", {
  obj <- .viz_obj()
  e <- .viz_catch(build_sc_viz_plot(obj, list(type = "correlation_matrix",
                                              feat_sel = "G1")))   # 1 seul gène
  .viz_expect(e, "Au moins 2 gènes requis")
})

# ---------------------------------------------------------------------------
# Réductions — 63 et 278 : même message, deux branches ⚠️ MULTI-ARGUMENTS
# (le nom de la réduction est un 2ᵉ argument) ⇒ la queue « umap » est
# exactement ce que la troncature supprimerait.
# ---------------------------------------------------------------------------
test_that("dim / density_2d : réduction non calculée, message ENTIER (63, 278)", {
  obj <- .viz_obj()   # aucune réduction calculée ⇒ "umap" absent
  e <- .viz_catch(build_sc_viz_plot(obj, list(type = "dim")))
  .viz_expect(e, "Réduction non calculée : umap")

  e <- .viz_catch(build_sc_viz_plot(obj, list(type = "density_2d",
                                              density_gene = "G1")))
  .viz_expect(e, "Réduction non calculée : umap")
})

test_that("density_2d : aucun gène sélectionné (276)", {
  obj <- .viz_obj()
  e <- .viz_catch(build_sc_viz_plot(obj, list(type = "density_2d")))
  .viz_expect(e, "Aucun gène sélectionné pour la densité")
})

# ---------------------------------------------------------------------------
# multi_sample (180)
# ---------------------------------------------------------------------------
test_that("multi_sample : au moins 2 échantillons requis (180)", {
  obj1 <- .viz_obj(n = 3L, clusters = "0")   # une seule orig.ident
  e <- .viz_catch(build_sc_viz_plot(obj1, list(type = "multi_sample")))
  .viz_expect(e, "Au moins 2 échantillons requis")
})

# ---------------------------------------------------------------------------
# Volcano — 189, 194, 200 ⚠️ 194 et 200 sont MULTI-ARGUMENTS
# ---------------------------------------------------------------------------
test_that("volcano : colonne de groupe introuvable (189)", {
  obj <- .viz_obj()
  e <- .viz_catch(build_sc_viz_plot(obj, list(type = "volcano", group_by = "nope")))
  .viz_expect(e, "Colonne de groupe introuvable")
})

test_that("volcano : groupe 1 invalide, message ENTIER (194)", {
  obj <- .viz_obj()
  e <- .viz_catch(build_sc_viz_plot(obj, list(type = "volcano",
                                              group_by = "seurat_clusters",
                                              volcano_group1 = "XX")))
  .viz_expect(e, "Groupe 1 invalide: XX")
})

test_that("volcano : échec de FindMarkers, message NON TRONQUÉ (200)", {
  obj <- .viz_obj()
  # ⚠️ La queue du message est INTERNE À SEURAT (volatile d'une version à
  # l'autre) ⇒ on n'assère pas sa valeur. En revanche la LONGUEUR, elle,
  # trahit la troncature : sans `paste0()`, `errorCondition()` ne garderait
  # que « FindMarkers: » (13 caractères), et rien de plus.
  e <- suppressWarnings(.viz_catch(
    build_sc_viz_plot(obj, list(type = "volcano", group_by = "seurat_clusters",
                                volcano_group1 = "0"))))
  expect_match(e$msg, "^FindMarkers: ")
  expect_gt(nchar(e$msg), nchar("FindMarkers: "))
  expect_true("sc_plotting_error" %in% e$class)
})

# ---------------------------------------------------------------------------
# 284 — le défaut du dispatcheur ⚠️ MULTI-ARGUMENTS
# ---------------------------------------------------------------------------
test_that("type inconnu : non supporté, message ENTIER (284)", {
  obj <- .viz_obj()
  e <- .viz_catch(build_sc_viz_plot(obj, list(type = "bogus")))
  .viz_expect(e, "Type de visualisation non supporté: bogus")
})

# ---------------------------------------------------------------------------
# Témoin nominal : la conversion ne doit pas casser le chemin heureux.
# ---------------------------------------------------------------------------
test_that("build_sc_viz_plot rend un ggplot sur le chemin nominal (dim)", {
  skip_if_not_installed("ggplot2")
  # `sc_plotting.R` appelle `ggtitle()`/`theme_*()` SANS préfixe (comme l'app,
  # qui attache ggplot2 dans global.R) ⇒ l'attacher ici aussi.
  suppressWarnings(suppressPackageStartupMessages(library(ggplot2)))
  obj <- .viz_obj(n = 8L)
  # une réduction « umap » minimale, sinon la garde 63 légitimement l'erreur
  emb <- matrix(rnorm(16), nrow = 8L,
                dimnames = list(paste0("c", 1:8), c("umap_1", "umap_2")))
  obj[["umap"]] <- suppressWarnings(
    SeuratObject::CreateDimReducObject(embeddings = emb, key = "umap_"))
  p <- build_sc_viz_plot(obj, list(type = "dim", group_by = "seurat_clusters"))
  expect_s3_class(p, "ggplot")
})
