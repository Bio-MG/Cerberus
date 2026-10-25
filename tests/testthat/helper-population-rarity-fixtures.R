# =============================================================================
# helper-population-rarity-fixtures.R — fixtures deterministes (CCC 9, Q1)
# =============================================================================
# Auto-source par testthat avant les test-*.R. Comme les autres helpers, ce
# fichier est source AVANT helper-source.R (ordre alphabetique p < s) — on
# source helper-source.R soi-meme si besoin. Noms prefixes .pr_* pour eviter
# toute collision.
#
# Sourcing minimal et VOLONTAIREMENT leger : le calcul ne depend que de
# io_helpers (%||%), provenance (new_provenance_entry) et sc_velocity
# (velocity_object_fingerprint, empreinte v2 REUTILISEE). AUCUN graphe, AUCUN
# miloR, AUCUNE dependance Seurat : la question est un simple decompte de
# metadonnees — c'est precisement le point de la regle 3 (cf. contrat §1.2).
#
# Aucune donnee biologique reelle : tailles de populations synthetiques et
# deterministes.
# =============================================================================

if (!exists("source_project_file", envir = globalenv(), mode = "function")) {
  .pr_root <- getwd()
  for (.i in 1:8) {
    if (file.exists(file.path(.pr_root, "app.R"))) break
    .parent <- dirname(.pr_root)
    if (identical(.parent, .pr_root)) break
    .pr_root <- .parent
  }
  sys.source(file.path(.pr_root, "tests", "testthat", "helper-source.R"),
             envir = globalenv())
}

source_project_file("tools/pkg_skeleton/R/io_helpers.R")   # %||%
source_project_file("tools/pkg_skeleton/R/provenance.R")
source_project_file("R/sc/sc_velocity.R")    # velocity_object_fingerprint
source_project_file("R/sc/sc_population_rarity.R")

# ── Metadonnees synthetiques deterministes ──────────────────────────────────
# counts        : nom de population -> nb de cellules (tailles CONNUES)
# na_labels     : nb de cellules sans etiquette (ajoutees en fin, exclues du
#                 decompte mais comptabilisees)
# empty_levels  : niveaux de facteur SANS cellule (exclus et comptabilises)
# sample_cycle  : nb d'echantillons distincts, attribues par cyclage
#                 deterministe (s1, s2, ...) — sert a tester n_samples_present.
#
# Defaut : A=60, B=20, C=8, D=3 (total 91 >= plancher 50). Avec le seuil
# declare 10 (regle absolue), C et D sont rares -> n_rare = 2, et il reste des
# populations NON rares : le cas nominal est donc discriminant.
.pr_meta <- function(counts = c(A = 60L, B = 20L, C = 8L, D = 3L),
                     na_labels = 0L,
                     empty_levels = character(0),
                     sample_cycle = 4L) {
  labels <- c(rep(names(counts), times = as.integer(counts)),
              rep(NA_character_, as.integer(na_labels)))
  meta <- data.frame(
    cell_type = factor(labels, levels = c(names(counts), empty_levels)),
    stringsAsFactors = FALSE
  )
  meta$sample_id <- rep(paste0("s", seq_len(as.integer(sample_cycle))),
                        length.out = nrow(meta))
  rownames(meta) <- paste0("cell_", seq_len(nrow(meta)))
  meta
}

# Stub a dimnames : seule l'identite (velocity_object_fingerprint) est extraite.
# Aucun objet Seurat n'est necessaire — voir l'en-tete.
.pr_stub_obj <- function(meta = .pr_meta()) {
  matrix(0, nrow = 5, ncol = nrow(meta),
         dimnames = list(paste0("gene", 1:5), rownames(meta)))
}

# ── Orchestration miroir du module ──────────────────────────────────────────
# Defauts = valeurs DECLAREES explicitement (le module lit les selectInput, les
# tests fixent les memes valeurs). Aucun seuil par defaut n'existe dans le
# domaine : il est fourni ici, comme le ferait l'utilisateur.
.pr_run <- function(meta = .pr_meta(),
                    identity_column = "cell_type",
                    rule_type = "absolute_n_cells",
                    threshold = 10L,
                    ...) {
  compute_population_rarity(
    meta            = meta,
    identity_column = identity_column,
    rule_type       = rule_type,
    threshold       = threshold,
    ...
  )
}
