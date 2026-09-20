# =============================================================================
# test-embeddings-prefix.R — `Embeddings()` doit etre PREFIXE (45e+ increment)
# =============================================================================
# POURQUOI CE FICHIER EXISTE
# -----------------------------------------------------------------------------
# `Embeddings()` est re-exporte par Seurat ET par SeuratObject. Dans CE depot,
# **aucun `library(Seurat)` inconditionnel** n'existe au boot de l'app :
#   - `R/spatial/spatial_async.R:222` ne l'attache que dans une garde PARESSEUSE
#     (`if (requireNamespace(...)) library(Seurat)`), donc APRES coup et selon
#     le chemin ;
#   - `R/sc/sc_export.R:47` et `R/spatial/spatial_export.R:555` sont DANS des
#     chaines de caractères (scripts R generes) — ils n'attachent rien dans le
#     processus de l'app.
# => Un appel NU `Embeddings(...)` dans du CODE EXECUTE echoue a l'execution
#    sur `could not find function "Embeddings"`, et ce, quelle que soit la
#    disponibilite du paquet : c'est une dependance d'EXECUTION invisible au
#    verrou source (§2by.6 #3, deja consigne pour `sc_export.R:31`).
#
# MESURE (avant ce lot) : 4 sites NUS —
#   R/sc/sc_export.R:31 · R/sc/sc_helpers.R:1238
#   modules/sc/mod_sc_pipeline.R:434 · modules/sc/mod_sc_viz.R:598
# CONVENTION DU DEPOT : `Seurat::Embeddings(` 13 sites  vs
#                       `SeuratObject::Embeddings(` 6 sites
# => `Seurat::` est le prefixe DOMINANT ; c'est celui qu'on applique.
#
# ⚠️ CE FICHIER N'ATTACHE **PAS** Seurat. C'est deliberé : attacher Seurat
# masquerait exactement le defaut qu'on mesure (un test ne doit pas fournir la
# dependance dont l'absence EST le bug). On lit donc le SOURCE, comme le fait
# deja `test-sc-export.R` pour le plafond a 50.
# =============================================================================

# Chemins de CODE EXECUTE. On EXCLUT explicitement :
#   - `R/sc/sc_export.R`  : la ligne 31 est du code, mais les lignes 58-88 sont
#     dans la chaine generee (elles DOIVENT rester nues : le script ecrit
#     `library(Seurat)` ligne 47). On traite ce fichier a part, ligne par ligne.
#   - tout commentaire (`#'`, `#`).
.ts_ep_files <- function() {
  root <- ts_project_root()
  f <- c(list.files(file.path(root, "R"), pattern = "\\.R$",
                    recursive = TRUE, full.names = TRUE),
         list.files(file.path(root, "modules"), pattern = "\\.R$",
                    recursive = TRUE, full.names = TRUE))
  sort(normalizePath(f, winslash = "/", mustWork = FALSE))
}

# Une ligne est du CODE si elle n'est pas un commentaire pur.
.ts_ep_is_code <- function(lines) {
  !grepl("^\\s*#", lines)
}

# Retire chaines et commentaires pour ne compter que les tokens REELLEMENT du code.
# (On ne cherche PAS a etre un parseur : on cherche les appels NUS `Embeddings(`.)
.ts_ep_bare_calls <- function(lines) {
  code <- lines[.ts_ep_is_code(lines)]
  # un appel NU = `Embeddings(` NON precede de `::` (aucun `:` juste avant)
  hits <- grepl("(?<![:[:alnum:]_.])Embeddings\\s*\\(", code, perl = TRUE)
  data.frame(line = code[hits], stringsAsFactors = FALSE)
}

# =============================================================================
# 1. Le contrat : ZERO appel NU dans le code execute de R/ et modules/
# =============================================================================
test_that("aucun appel NU a Embeddings() dans R/ ni modules/ (code execute)", {
  bad <- list()
  for (p in .ts_ep_files()) {
    # sc_export.R : on n'inspecte QUE la partie code (avant le `paste0(`)
    lines <- readLines(p, warn = FALSE, encoding = "UTF-8")
    if (grepl("sc_export\\.R$", p)) {
      first_paste <- min(grep("^\\s*paste0\\(", lines))
      if (is.finite(first_paste)) lines <- lines[seq_len(first_paste - 1L)]
    }
    # spatial_export.R : le `library(Seurat)` de la ligne 555 est dans une
    # fonction dont le corps EST execute — mais les appels nus y sont couverts
    # par ce `library()`. On le laisse donc de cote lui aussi, comme documente.
    if (grepl("spatial_export\\.R$", p)) next
    b <- .ts_ep_bare_calls(lines)
    if (nrow(b)) bad[[p]] <- b$line
  }
  expect_length(bad, 0L)
  # Temoin de non-vacuite : la sonde DOIT trouver des appels PREFIXES, sinon
  # elle ne prouve rien (une regex muette rendrait ce test vert a tort).
  all_lines <- unlist(lapply(.ts_ep_files(), function(p)
    readLines(p, warn = FALSE, encoding = "UTF-8")))
  expect_true(any(grepl("Seurat(Object)?::Embeddings\\s*\\(", all_lines)))
})

# =============================================================================
# 2. Les 4 sites MESURES portent desormais le prefixe DOMINANT `Seurat::`
# =============================================================================
test_that("les 4 sites mesures sont prefixes en Seurat::", {
  root <- ts_project_root()
  sites <- c(
    "R/sc/sc_export.R"                  = "min(ncol(Seurat::Embeddings(obj,\"pca\")), 50)",
    "R/sc/sc_helpers.R"                 = "nrow(Seurat::Embeddings(obj[[cand]]))",
    "modules/sc/mod_sc_pipeline.R"      = "t(Seurat::Embeddings(obj,\"pca\")",
    "modules/sc/mod_sc_viz.R"           = "ncol(Seurat::Embeddings(obj, r))"
  )
  for (rel in names(sites)) {
    src <- readLines(file.path(root, rel), warn = FALSE, encoding = "UTF-8")
    expect_true(any(grepl(sites[[rel]], src, fixed = TRUE)),
                info = paste("prefixe manquant dans", rel))
  }
})

# =============================================================================
# 3. BORNE : le script GENERE doit garder ses appels NUS (il attache Seurat)
# =============================================================================
# C'est la borne qui empeche un « correctif » trop large : prefixer les lignes
# 58-88 de sc_export.R serait FAUX (ce serait du texte, pas du code) et ferait
# echouer les tests de contenu du script genere.
test_that("sc_export.R : le script GENERE garde ses appels NUS (library(Seurat) ligne 47)", {
  src <- readLines(file.path(ts_project_root(), "R/sc/sc_export.R"),
                   warn = FALSE, encoding = "UTF-8")
  # le script genere attache Seurat...
  expect_true(any(grepl("^library\\(Seurat\\)", src)))
  # ...donc `RunPCA`/`FindClusters`/`Idents` y restent NUS (dans le paste0)
  first_paste <- min(grep("^\\s*paste0\\(", src))
  body <- src[seq.int(first_paste, length(src))]
  expect_true(any(grepl("RunPCA\\(", body)))
  expect_false(any(grepl("Seurat::RunPCA\\(", body)))
})

# =============================================================================
# 4. La sonde DISCRIMINE : elle attrape bien un appel nu (auto-test)
# =============================================================================
test_that("la sonde .ts_ep_bare_calls() attrape un nu et epargne un prefixe", {
  probe <- c(
    "  x <- Embeddings(obj, \"pca\")",              # NU  -> doit matcher
    "  y <- Seurat::Embeddings(obj, \"pca\")",      # SE  -> non
    "  z <- SeuratObject::Embeddings(obj, \"pca\")",# SO  -> non
    "# w <- Embeddings(obj)  (commentaire, ignore)",
    "  a <- myEmbeddings(obj)"                      # autre nom -> non
  )
  b <- .ts_ep_bare_calls(probe)
  expect_length(b$line, 1L)
  expect_match(b$line, "^\\s*x <-", fixed = FALSE)
})
