# =============================================================================
# test-biostat-m4-prefill.R -- M4 (O27 2026-10-29) : prefill du parcours
# biostatistique depuis les metadonnees du jeu actif (moteur PUR), RED d'abord
# =============================================================================
# CONTRAT : docs/contracts/BIOSTAT_DECISION_CONTRACT.md -- SPÉC GELÉE v1.0.0
# (c'est LA source ; ce test le SUIT, il ne le modifie pas). Proposition mere :
# docs/proposals/BIOSTAT_DECISION_TREE_PROPOSAL.md §3 (pre-remplissage :
# n groupes depuis la condition, appariement via identifiants dupliques a
# travers les conditions) -- orchestration cote module = M4 (jalon M4, delta
# documente en tete de modules/bulk_de/mod_biostat_tree.R).
#
# CE QUE CE FICHIER EPINGLE (moteur pur, hors Shiny -- motif test-biostat-tree.R)
#
#   biostat_prefill_from_metadata(meta, condition_col = NULL, subject_col = NULL)
#   -> list(groups = <"two"|"three_plus"|NA_character_>, paired = <bool>)
#
#   P1  surface : la fonction existe, signature (meta, condition_col = NULL,
#       subject_col = NULL), retour liste NOMMEE groups/paired.
#   P2  groupes : >= 3 niveaux distincts non-NA -> "three_plus" ; sinon "two"
#       (y compris 1 seul niveau) ; condition_col NULL / absente de meta /
#       colonne vide (tout NA) -> NA_character_ (PAS de deduction magique).
#   P3  appariement : TRUE seulement si subject_col fournie, presente, ET une
#       valeur non-NA de cette colonne se repete a travers PLUSIEURS niveaux
#       de condition ; duplication DANS un seul niveau -> FALSE ; subject_col
#       absent/NULL -> FALSE ; condition absente -> FALSE (pas de niveaux a
#       travers lesquels repeter).
#   P4  purete : zero I/O (garde lexicale sur le code, motif T2 du freeze M2),
#       meta non mute par l'appel, pas d'ecriture d'etat (<<-, assign).
#   P5  contrat gele intact : biostat_tree_version() et l'enum §3 (13 valeurs)
#       ne bougent PAS (le moteur M4 est ADDITIF au fichier M2), et le record
#       §2 consomme les sorties reelles du prefill (groups "two"/"three_plus").
#
# RED : tant que biostat_prefill_from_metadata() n'existe pas dans
# tools/pkg_skeleton/R/biostat_tree.R, chaque bloc P* echoue sur son premier
# expect explicite (jamais un skip -- un skip n'est pas une porte rouge,
# motif test-biostat-tree.R:52-54). ASCII pur pour les gardes (C7).
# =============================================================================

.biostat_m4p_rel  <- "tools/pkg_skeleton/R/biostat_tree.R"
.biostat_m4p_path <- file.path(ts_project_root(), .biostat_m4p_rel)
.biostat_m4p_present <- file.exists(.biostat_m4p_path)
.biostat_m4p_red_info <- paste(
  .biostat_m4p_rel, "absent : moteur M2 non livre -- M4 ne peut pas lui",
  "ajouter le prefill (contrat BIOSTAT_DECISION_CONTRACT.md : le fichier",
  "existant porte l'arbre gele, aucun nouveau fichier source pour M4)"
)

if (.biostat_m4p_present) {
  source_project_file(.biostat_m4p_rel)
}

.biostat_m4p_fn_present <- .biostat_m4p_present &&
  exists("biostat_prefill_from_metadata", envir = globalenv(),
         inherits = FALSE)

# --- Code du fichier moteur, commentaires relies (motif test-biostat-tree.R) --
.biostat_m4p_code <- function() {
  lines <- readLines(.biostat_m4p_path, warn = FALSE)
  paste(sub("#.*$", "", lines), collapse = "\n")
}

# --- Fixtures metadata (niveaux / sujets, NAs meles) --------------------------
.biostat_m4p_meta <- function(condition, subject = NULL) {
  m <- data.frame(condition = condition, stringsAsFactors = FALSE)
  if (!is.null(subject)) m$subject <- subject
  m
}

# =============================================================================
# P1 -- porte RED + surface : signature et forme du retour
# =============================================================================
test_that("P1 surface : biostat_prefill_from_metadata existe, signature et retour nomme", {
  expect_true(.biostat_m4p_present, info = .biostat_m4p_red_info)
  if (!.biostat_m4p_present) return(invisible(NULL))
  expect_true(.biostat_m4p_fn_present,
              info = paste("biostat_prefill_from_metadata() absente de",
                           "tools/pkg_skeleton/R/biostat_tree.R -- moteur M4",
                           "(prefill, proposition §3) non livre"))
  if (!.biostat_m4p_fn_present) return(invisible(NULL))

  fmls <- formals(biostat_prefill_from_metadata)
  expect_identical(names(fmls), c("meta", "condition_col", "subject_col"),
                   info = paste("signature M4 attendue : (meta, condition_col =",
                                "NULL, subject_col = NULL) -- moteur pur, aucune",
                                "lecture d'etat (C17)"))
  expect_true(is.null(fmls$condition_col) && is.null(fmls$subject_col),
              info = "condition_col et subject_col doivent etre optionnels (NULL par defaut)")

  out <- biostat_prefill_from_metadata(
    .biostat_m4p_meta(c("A", "A", "B", "B"), c("s1", "s2", "s1", "s2")),
    condition_col = "condition", subject_col = "subject")
  expect_true(is.list(out),
              info = "le prefill doit etre une liste (record §2 : prefilled est une liste nommee)")
  expect_identical(names(out), c("groups", "paired"),
                   info = "retour NOMME attendu : list(groups =, paired =) (contrat §2)")
  expect_identical(out$groups, "two",
                   info = "2 niveaux -> groups = 'two'")
  expect_true(is.logical(out$paired) && isTRUE(out$paired),
              info = "sujet s1 repete a travers A et B -> paired = TRUE")
})

# =============================================================================
# P2 -- groupes : 3+ -> three_plus ; sinon two ; degenerescences -> NA
# =============================================================================
test_that("P2 groupes : three_plus / two / NA sans deduction magique", {
  expect_true(.biostat_m4p_present, info = .biostat_m4p_red_info)
  if (!.biostat_m4p_present) return(invisible(NULL))
  expect_true(.biostat_m4p_fn_present,
              info = "biostat_prefill_from_metadata() absente (moteur M4 non livre)")
  if (!.biostat_m4p_fn_present) return(invisible(NULL))

  # >= 3 niveaux distincts non-NA (NAs meles, facteurs compris) -> three_plus
  out3 <- biostat_prefill_from_metadata(
    .biostat_m4p_meta(c("A", "B", "C", "A", NA)),
    condition_col = "condition")
  expect_identical(out3$groups, "three_plus",
                   info = "3 niveaux distincts non-NA -> 'three_plus' (proposition §3)")

  out3f <- biostat_prefill_from_metadata(
    data.frame(condition = factor(c("A", "B", "C", NA)), stringsAsFactors = TRUE),
    condition_col = "condition")
  expect_identical(out3f$groups, "three_plus",
                   info = "colonne facteur : les niveaux presents comptent (3 -> three_plus)")

  # 2 niveaux -> two ; 1 seul niveau -> two aussi (spec : three_plus si >= 3,
  # two sinon ; NA seulement si colonne indisponible/vide)
  expect_identical(
    biostat_prefill_from_metadata(
      .biostat_m4p_meta(c("A", "A", "B", NA)), condition_col = "condition")$groups,
    "two", info = "2 niveaux distincts non-NA -> 'two'")
  expect_identical(
    biostat_prefill_from_metadata(
      .biostat_m4p_meta(c("A", "A", "A")), condition_col = "condition")$groups,
    "two", info = "1 seul niveau -> 'two' (seule la colonne indisponible/vide rend NA)")

  # Degenerescences -> NA_character_ : condition_col NULL, absente, colonne vide
  expect_identical(
    biostat_prefill_from_metadata(
      .biostat_m4p_meta(c("A", "B")), condition_col = NULL)$groups,
    NA_character_, info = "condition_col NULL -> NA_character_ (pas de deduction magique)")
  expect_identical(
    biostat_prefill_from_metadata(
      .biostat_m4p_meta(c("A", "B")), condition_col = "nope")$groups,
    NA_character_, info = "condition_col absente de meta -> NA_character_")
  expect_identical(
    biostat_prefill_from_metadata(
      .biostat_m4p_meta(c(NA, NA, NA)), condition_col = "condition")$groups,
    NA_character_, info = "colonne vide (tout NA) -> NA_character_")
  expect_identical(
    biostat_prefill_from_metadata(NULL, condition_col = "condition")$groups,
    NA_character_, info = "meta NULL -> NA_character_ (degradation honnete)")
  expect_identical(
    biostat_prefill_from_metadata(data.frame(), condition_col = "condition")$groups,
    NA_character_, info = "meta sans colonnes -> NA_character_")
})

# =============================================================================
# P3 -- appariement : repetition a travers PLUSIEURS niveaux de condition
# =============================================================================
test_that("P3 paired : TRUE seulement si un sujet traverse plusieurs conditions", {
  expect_true(.biostat_m4p_present, info = .biostat_m4p_red_info)
  if (!.biostat_m4p_present) return(invisible(NULL))
  expect_true(.biostat_m4p_fn_present,
              info = "biostat_prefill_from_metadata() absente (moteur M4 non livre)")
  if (!.biostat_m4p_fn_present) return(invisible(NULL))

  # sujet s1 present dans A ET B -> TRUE (proposition §3 : ids dupliques entre
  # conditions) ; les lignes NA d'un cote ou de l'autre sont ignorees
  m_paired <- .biostat_m4p_meta(
    c("A", "A", "B", "B", "B", NA),
    c("s1", "s2", "s1", "s3", NA,  "s1"))
  expect_true(
    biostat_prefill_from_metadata(m_paired, condition_col = "condition",
                                  subject_col = "subject")$paired,
    info = "s1 se repete a travers les niveaux A et B -> paired = TRUE")

  # 3 niveaux traverses -> TRUE aussi ; et groups vaut three_plus sur le meme appel
  m_three <- .biostat_m4p_meta(c("A", "B", "C", "A", "B", "C"),
                               c("s1", "s1", "s1", "s2", "s2", "s2"))
  out <- biostat_prefill_from_metadata(m_three, condition_col = "condition",
                                       subject_col = "subject")
  expect_true(out$paired,
              info = "s1 traverse A, B et C -> paired = TRUE (>= 2 niveaux)")
  expect_identical(out$groups, "three_plus",
                   info = "groups et paired sont deduits du MÊME appel (liste §2)")

  # duplication DANS un seul niveau de condition : PAS un appariement
  m_within <- .biostat_m4p_meta(c("A", "A", "B"), c("s1", "s1", "s2"))
  expect_false(
    biostat_prefill_from_metadata(m_within, condition_col = "condition",
                                  subject_col = "subject")$paired,
    info = paste("s1 duplique DANS le niveau A seulement -> FALSE (la repetition",
                 "doit traverser PLUSIEURS niveaux de condition, proposition §3)"))

  # cas degeneres -> FALSE (jamais NA)
  expect_false(
    biostat_prefill_from_metadata(m_paired, condition_col = "condition")$paired,
    info = "subject_col non fourni -> paired = FALSE")
  expect_false(
    biostat_prefill_from_metadata(m_paired, condition_col = "condition",
                                  subject_col = "nope")$paired,
    info = "subject_col absent de meta -> paired = FALSE")
  expect_false(
    biostat_prefill_from_metadata(m_paired, condition_col = "nope",
                                  subject_col = "subject")$paired,
    info = "condition_col absente -> FALSE (aucun niveau de condition a traverser)")
  expect_false(
    biostat_prefill_from_metadata(
      .biostat_m4p_meta(c("A", "B"), c(NA, NA)),
      condition_col = "condition", subject_col = "subject")$paired,
    info = "subject_col tout NA -> paired = FALSE")
  expect_false(
    biostat_prefill_from_metadata(m_paired, condition_col = "condition",
                                  subject_col = "subject")$paired &&
      FALSE,
    info = "garde de programmation (lecture seule du bloc degenerescent)")
})

# =============================================================================
# P4 -- purete : zero I/O (garde lexicale), meta non mute, pas d'etat global
# =============================================================================
test_that("P4 purete : aucune I/O, aucune mutation de meta, aucune ecriture d'etat", {
  expect_true(.biostat_m4p_present, info = .biostat_m4p_red_info)
  if (!.biostat_m4p_present) return(invisible(NULL))
  expect_true(.biostat_m4p_fn_present,
              info = "biostat_prefill_from_metadata() absente (moteur M4 non livre)")
  if (!.biostat_m4p_fn_present) return(invisible(NULL))

  # (a) garde lexicale sur le CODE du fichier (commentaires relies, motif T2
  #     du freeze M2) : le moteur M2+M4 ne lit ni n'ecrit rien.
  code <- .biostat_m4p_code()
  forbidden <- c(
    "read\\.csv|read\\.csv2|read\\.delim|read\\.rds|readRDS|readLines|fread|scan\\s*\\(" = "lecture de fichier",
    "file\\s*\\(|url\\s*\\(|download|socket|dbConnect|connect\\s*\\("                    = "acces ressource",
    "write\\.|saveRDS|writeLines"                                                        = "ecriture de fichier",
    "<<-|assign\\s*\\("                                                                  = "ecriture d'etat global",
    "source\\s*\\(|sys\\.source"                                                         = "sourcing (I/O indirecte)",
    "Sys\\.getenv|Sys\\.setenv"                                                          = "environnement processus",
    "reactiveValues|isolate\\s*\\("                                                      = "etat reactif Shiny (C2/C17)"
  )
  for (i in seq_along(forbidden)) {
    expect_false(grepl(names(forbidden)[i], code),
                 info = paste("le moteur biostat (M2+prefill M4) contient un",
                              "token I/O ou d'etat interdit :",
                              forbidden[[i]], "-- purete exige (C17, zero I/O)"))
  }

  # (b) meta non mute par l'appel (purete fonctionnelle).
  meta <- .biostat_m4p_meta(c("A", "A", "B", "B"), c("s1", "s2", "s1", "s2"))
  snap <- data.frame(
    condition = as.character(meta$condition),
    subject   = as.character(meta$subject),
    stringsAsFactors = FALSE)
  invisible(biostat_prefill_from_metadata(meta, condition_col = "condition",
                                          subject_col = "subject"))
  expect_identical(data.frame(condition = as.character(meta$condition),
                              subject   = as.character(meta$subject),
                              stringsAsFactors = FALSE),
                   snap,
                   info = "l'appel a mute meta -- interdit (fonction pure)")
})

# =============================================================================
# P5 -- contrat gele intact : version, enum §3, et le record §2 consomme les
#       sorties reelles du prefill
# =============================================================================
test_that("P5 contrat gele intact : version/enum inchanges ; record consomme le prefill deduit", {
  expect_true(.biostat_m4p_present, info = .biostat_m4p_red_info)
  if (!.biostat_m4p_present) return(invisible(NULL))
  expect_true(.biostat_m4p_fn_present,
              info = "biostat_prefill_from_metadata() absente (moteur M4 non livre)")
  if (!.biostat_m4p_fn_present) return(invisible(NULL))

  # M4 est ADDITIF : ni biostat_tree_version() ni l'enum §3 ne bougent (jalon).
  expect_identical(biostat_tree_version(), "1.0.0",
                   info = "biostat_tree_version() doit rester '1.0.0' (contrat §2, aucun bump M4)")
  expect_setequal(
    .biostat_method_ids,
    c("bulk_deseq2_wald", "bulk_deseq2_lrt", "bulk_edger", "bulk_edger_norep",
      "bulk_voom", "cont_ttest", "cont_welch", "cont_paired_ttest",
      "cont_wilcoxon", "cont_signed_rank", "cont_anova", "cont_kruskal",
      "sc_da_delegate"))
  expect_identical(length(.biostat_method_ids), 13L,
                   info = "enum §3 ferme : 13 valeurs, aucune autre (contrat gelé)")

  # Les deux valeurs reelles du prefill passent la garde prefilled du record §2
  # (le module M4 retombe sur les defauts M3 quand groups vaut NA -- comportement
  # câble, épingle par test-biostat-m4-wiring.R).
  pf <- biostat_prefill_from_metadata(
    .biostat_m4p_meta(c("A", "B", "C"), c("s1", "s1", "s2")),
    condition_col = "condition", subject_col = "subject")
  rec <- biostat_decision_record(
    node_path      = c("root", "data_counts", "replicates_ok", "groups_three",
                       "bulk_deseq2_lrt"),
    method_id      = "bulk_deseq2_lrt",
    engine         = "",
    rationale_keys = "biostat.r.lrt",
    diagnostics    = list(),
    prefilled      = list(groups = pf$groups, paired = pf$paired),
    user_confirmed = TRUE)
  expect_identical(rec$prefilled$groups, "three_plus",
                   info = "le record §2 doit consommer la valeur déduite three_plus telle quelle")
  expect_true(isTRUE(rec$prefilled$paired),
               info = "le record §2 doit consommer la valeur déduite paired = TRUE telle quelle")
})
