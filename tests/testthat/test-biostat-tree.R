# =============================================================================
# test-biostat-tree.R -- M2 (O27 2026-10-29) : logique pure de l'arbre de
# decision biostatistique, RED d'abord (contract-first, STATUS.md §7)
# =============================================================================
# CONTRAT : docs/contracts/BIOSTAT_DECISION_CONTRACT.md -- SPÉC GELÉE v1.0.0
# (c'est LA source ; ce test le SUIT, il ne le modifie pas). Proposition mere :
# docs/proposals/BIOSTAT_DECISION_TREE_PROPOSAL.md (§12 : 4 decisions tranchees
# par O27 -- GO / orientation seule / alpha 0.05 / UI accordeon Bulk DE).
#
# POURQUOI CE FICHIER
#
# Le contrat (section « Statut ») est sans effet executant tant que
# tools/pkg_skeleton/R/biostat_tree.R n'existe pas. Ce freeze test leve le
# voile : il fait EXISTER l'effet executant des M2 en epinglant la surface
# publique, l'orientation seule, l'alpha, la descente et la provenance.
#
# Ce qui est epingle (T1..T6, contre le contrat gele) :
#   T1  surface publique gele : les fonctions documentees au contrat existent
#       -- l'entree de l'arbre (biostat_build_tree(), proposition §1.2) et les
#       accesseurs de recommandation (biostat_resolve(), contrat §6 ;
#       biostat_decision_record(), contrat §2 ; biostat_tree_version(),
#       contrat §1/§2). Enumerees depuis le contrat + la fiche, pas inventees.
#   T2  orientation seule : AUCUNE feuille ne lance de test statistique --
#       garde lexicale sur le CODE (commentaires relies) : pas d'appel
#       de:: / wilcox.test( / t.test( / edgeR / DESeq2. Les feuilles cont_*
#       rendent l'ORIENTATION (texte/structure gelees), jamais un lancement
#       (O27 decision §12.2). Les chaines "engine" DECLAREES par
#       l'utilisateur (contrat §2) ne sont pas des appels : seules les formes
#       d'appel (::, f() ) sont cherchees.
#   T3  alpha epingle : TS_BIOSTAT_ALPHA_DEFAULT (0.05, O27 §12.3) est la
#       seule source du seuil -- aucun litteral 0.05 dans l'arbre, aucune
#       re-declaration de la constante (bloc gele de thresholds.R:103-105).
#   T4  descente deterministe : pour les entrees du contrat, l'arbre rend la
#       recommandation attendue (enum §3 ferme : les 13 method_id atteints ;
#       node_path §2 : "root" en tete, feuille = method_id, sans doublon
#       consecutif) + la provenance 4F (les 4 champs d'identite geles du
#       contrat §1 : analysis_id / method / parameters / dataset).
#   T5  entrees invalides : refus structure honnete -- erreurs TYPEES du
#       vocabulaire gele du contrat §6 (biostat_tree_missing_answer,
#       biostat_tree_unknown_method), jamais une exception brute non classee.
#   T6  pin C8 : ce test CITE le contrat (motif « BIOSTAT_DECISION_CONTRACT »
#       present dans le fichier) -- garde C8 de tools/check_conventions.R:502
#       (token "biostat_decision" cherche dans les blobs de tests) ; dette
#       O26/M1 « C8 x2 dont BIOSTAT » (DECISIONS.md 2026-10-29) : 2 -> 1.
#
# Charge : le fichier de logique pure est source via source_project_file()
# (helper-source.R, motif helper-sccoda-fixtures.R) ; thresholds.R (bloc
# TS_BIOSTAT_*) est deja source par le bootstrap de test. Provenance.R est
# source pour le canal 4F (new_provenance_entry(), contrat §1) -- canal
# EXISTANT, aucune API nouvelle (proposition §6).
#
# RED : tant que biostat_tree.R n'existe pas, chaque bloc T* echoue sur son
# premier expect_true explicite (jamais un skip -- un skip n'est pas une
# porte rouge). ASCII pur : un garde ne doit pas dependre de la locale (C7).
# =============================================================================

.biostat_tree_rel   <- "tools/pkg_skeleton/R/biostat_tree.R"
.biostat_tree_path  <- file.path(ts_project_root(), .biostat_tree_rel)
.biostat_tree_present <- file.exists(.biostat_tree_path)
.biostat_red_info <- paste(
  .biostat_tree_rel, "absent : M2 (logique pure, O27 2026-10-29) non livre --",
  "contrat BIOSTAT_DECISION_CONTRACT.md : sans effet executant tant que le",
  "fichier n'existe pas (section Statut)"
)

if (.biostat_tree_present) {
  source_project_file(.biostat_tree_rel)
}

# Canal 4F EXISTANT (contrat §1 : new_provenance_entry() + provenance_append())
.biostat_prov_rel <- "tools/pkg_skeleton/R/provenance.R"
if (file.exists(file.path(ts_project_root(), .biostat_prov_rel))) {
  source_project_file(.biostat_prov_rel)
}

# --- Code du fichier de logique pure, commentaires relies --------------------
# Les gardes T2/T3 jugent le CODE, pas les commentaires : on coupe chaque
# ligne au premier "#". Approximation assumee (une "#" dans une chaine R
# tronquerait la ligne) : aucune cle i18n ni chaine du contrat n'en porte ;
# tout doute se leve en lisant le fichier.
.biostat_tree_code <- function() {
  lines <- readLines(.biostat_tree_path, warn = FALSE)
  paste(sub("#.*$", "", lines), collapse = "\n")
}

# --- Enum §3 ferme (13 valeurs, aucune autre -- contrat gele) ---------------
.biostat_enum_13 <- c(
  "bulk_deseq2_wald", "bulk_deseq2_lrt", "bulk_edger", "bulk_edger_norep",
  "bulk_voom", "cont_ttest", "cont_welch", "cont_paired_ttest",
  "cont_wilcoxon", "cont_signed_rank", "cont_anova", "cont_kruskal",
  "sc_da_delegate"
)

# --- Parcours generiques de l'arbre (champs declaratifs, proposition §1.2) ---
# Le contrat §2 fige les champs du RECORD, pas la representation interne de
# l'arbre ; le parcours reste donc AGNOSTIQUE (liste imbriquee OU data.tree
# Node) : acces de champ par [[ ]] sous tryCatch, un champ absent est IGNORE,
# jamais fatal. NB mesure 2026-11-01 : data.tree est dans renv.lock (ligne
# 17784) mais PAS installe dans la bibliotheque active -- l'implementation
# M2 doit pouvoir construire l'arbre sans lui (zero dependance nouvelle).
.biostat_node_field <- function(nd, field) {
  if (is.null(nd)) return(NULL)
  tryCatch(nd[[field]], error = function(e) NULL)
}

.biostat_kids <- function(nd) {
  kids <- .biostat_node_field(nd, "children")
  if (is.null(kids)) list() else kids
}

# Toutes les feuilles (noeuds portant un method_id non vide), avec la carte
# de reponses qui y mene : on ne part d'aucun vocabulaire de reponse DEVINE,
# la carte est DERIVEE de l'arbre lui-meme (champ `answers` nomme, §1.2).
.biostat_walk <- function(nd, answers = list(), out = list()) {
  mid <- .biostat_node_field(nd, "method_id")
  if (!is.null(mid) && length(mid) == 1L && !is.na(mid) && nzchar(mid)) {
    out[[length(out) + 1L]] <- list(leaf = nd, answers = answers)
    return(out)
  }
  ans <- .biostat_node_field(nd, "answers")
  nid <- .biostat_node_field(nd, "id")
  if (is.null(nid) || is.na(nid)) nid <- .biostat_node_field(nd, "name")
  kids <- .biostat_kids(nd)
  if (is.null(ans) || !length(ans) || is.null(names(ans)) || !length(kids)) {
    return(out) # noeud porte (gate) sans reponse nommee : hors portee RED
  }
  for (nm in names(ans)) {
    target <- ans[[nm]]
    child <- NULL
    if (is.list(target) && (!is.null(.biostat_node_field(target, "children")) ||
                            !is.null(.biostat_node_field(target, "method_id")) ||
                            !is.null(.biostat_node_field(target, "answers")))) {
      child <- target # la branche EST le noeud enfant
    } else {
      for (k in kids) {
        kid <- .biostat_node_field(k, "id")
        if (is.null(kid) || is.na(kid)) kid <- .biostat_node_field(k, "name")
        if (identical(kid, target) || identical(.biostat_node_field(k, "name"), target)) {
          child <- k
          break
        }
      }
    }
    if (is.null(child)) next
    key <- if (is.null(nid) || is.na(nid)) paste0("node", length(answers) + 1L) else nid
    out <- .biostat_walk(child, c(answers, setNames(list(nm), key)), out)
  }
  out
}

# node_path d'un resultat de resolve() : soit le champ fige du contrat §2,
# soit la remontee parentale du noeud feuille (data.tree).
.biostat_node_path_of <- function(res) {
  np <- .biostat_node_field(res, "node_path")
  if (is.null(np)) {
    np <- character(0)
    cur <- res
    while (!is.null(cur)) {
      nm <- .biostat_node_field(cur, "name")
      np <- c(if (is.null(nm)) NA_character_ else nm, np)
      cur <- .biostat_node_field(cur, "parent")
    }
  }
  as.character(np)
}

# =============================================================================
# T0 -- porte RED explicite (le fichier existe ; jamais un skip)
# =============================================================================
test_that("T0 RED : le fichier M2 tools/pkg_skeleton/R/biostat_tree.R existe", {
  expect_true(.biostat_tree_present, info = .biostat_red_info)
})

# =============================================================================
# T1 -- surface publique gele (contrat §1/§2/§6 + proposition §1.2)
# =============================================================================
test_that("T1 surface publique gele : entree de l'arbre + accesseurs de recommandation", {
  expect_true(.biostat_tree_present, info = .biostat_red_info)
  if (!.biostat_tree_present) return(invisible(NULL))

  surface <- c(
    biostat_tree_version     = "contrat §1 (method = decision_tree_v ++ version) et §2",
    biostat_build_tree       = "entree de l'arbre -- proposition §1.2 (logique pure)",
    biostat_resolve          = "descente -- contrat §6 (erreur typée si reponse manquante)",
    biostat_decision_record  = "record de recommandation -- contrat §2 (champs geles)"
  )
  for (i in seq_along(surface)) {
    expect_true(
      exists(names(surface)[i], envir = globalenv(), inherits = FALSE),
      info = paste("fonction documentee absente :", names(surface)[i],
                   "--", unname(surface)[i])
    )
  }
  expect_identical(
    biostat_tree_version(), "1.0.0",
    info = "contrat §2 : version egale a biostat_tree_version(), '1.0.0' a la creation"
  )
})

# =============================================================================
# T2 -- orientation seule (O27 §12.2) : aucune feuille ne lance de test
# =============================================================================
test_that("T2 orientation seule : aucun appel de test statistique dans l'arbre", {
  expect_true(.biostat_tree_present, info = .biostat_red_info)
  if (!.biostat_tree_present) return(invisible(NULL))

  code <- .biostat_tree_code()
  # Formes d'APPEL uniquement (contrat §2 : l'engine des feuilles bulk_* est
  # DECLARE par l'utilisateur -- une chaine "DESeq2" n'est pas un appel).
  forbidden <- c(
    "de::"                = "alias de differential expression (de::)",
    "wilcox\\.test\\s*\\(" = "wilcox.test()",
    "t\\.test\\s*\\("     = "t.test()",
    "edgeR\\s*(::|\\()"   = "edgeR (paquet ou appel)",
    "DESeq2\\s*(::|\\()"  = "DESeq2 (paquet ou appel)"
  )
  for (i in seq_along(forbidden)) {
    expect_false(
      grepl(names(forbidden)[i], code),
      info = paste("l'arbre lance un test statistique :", forbidden[i],
                   "-- O27 §12.2 : orientation seule, les feuilles cont_*",
                   "rendent l'orientation, jamais un lancement")
    )
  }
})

# =============================================================================
# T3 -- alpha epingle : TS_BIOSTAT_ALPHA_DEFAULT seule source du seuil
# =============================================================================
test_that("T3 alpha epingle : pas de litteral 0.05 ni de re-declaration dans l'arbre", {
  expect_true(.biostat_tree_present, info = .biostat_red_info)
  if (!.biostat_tree_present) return(invisible(NULL))

  code <- .biostat_tree_code()
  expect_false(
    grepl("0\\.05", code),
    info = paste("litteral 0.05 dans biostat_tree.R : le seuil doit venir",
                 "EXCLUSIVEMENT de TS_BIOSTAT_ALPHA_DEFAULT (contrat §4,", "thresholds.R:103 -- bloc gele)")
  )
  expect_false(
    grepl("TS_BIOSTAT_ALPHA_DEFAULT\\s*<-", code),
    info = paste("re-declaration de TS_BIOSTAT_ALPHA_DEFAULT dans l'arbre :",
                 "le bloc TS_BIOSTAT_* de thresholds.R est gele (M1)")
  )
  # La constante O27 est bien celle du bloc gele (filet : le garde porte sur
  # la VALEUR declaree, decision §12.3 = 0.05).
  expect_true(
    exists("TS_BIOSTAT_ALPHA_DEFAULT", envir = globalenv(), inherits = FALSE),
    info = "TS_BIOSTAT_ALPHA_DEFAULT introuvable (thresholds.R non source ?)"
  )
  expect_identical(TS_BIOSTAT_ALPHA_DEFAULT, 0.05,
                   info = "O27 §12.3 : TS_BIOSTAT_ALPHA_DEFAULT = 0.05")
})

# =============================================================================
# T4 -- descente deterministe + provenance 4F (contrat §1/§2/§3)
# =============================================================================
test_that("T4 descente deterministe : recommandation attendue + provenance 4F", {
  expect_true(.biostat_tree_present, info = .biostat_red_info)
  if (!.biostat_tree_present) return(invisible(NULL))

  tree <- biostat_build_tree()

  # (a) enum §3 FERME : les 13 method_id du contrat, aucune autre feuille.
  .collect_mids <- function(nd, out = character(0)) {
    mid <- .biostat_node_field(nd, "method_id")
    if (!is.null(mid) && length(mid) == 1L && !is.na(mid) && nzchar(mid)) {
      out <- c(out, mid)
    }
    for (k in .biostat_kids(nd)) out <- .collect_mids(k, out)
    out
  }
  # AJUSTEMENT MINEUR (2026-11-01, passage VERT) : le testthat verrouille du
  # depot n'accepte pas info= sur expect_setequal() — le message vit ici, le
  # verdict imprime les deux vecteurs.
  # Enum §3 FERME : exactement les 13 valeurs, aucune autre (contrat gele).
  expect_setequal(.collect_mids(tree), .biostat_enum_13)

  # (b) descente deterministe : chaque feuille est rejoints par resolve()
  # avec la carte de reponses DERIVEE de l'arbre ; deux appels, un verdict.
  paths <- .biostat_walk(tree)
  expect_true(
    length(paths) >= 1L,
    info = paste("aucune feuille joignable par le parcours de l'arbre :",
                 "biostat_build_tree() doit exposer des noeuds question a",
                 "champ answers nomme (proposition §1.2)")
  )
  for (p in paths) {
    res  <- biostat_resolve(tree, p$answers)
    res2 <- biostat_resolve(tree, p$answers)
    expect_identical(
      res, res2,
      info = paste("biostat_resolve() non deterministe pour la feuille",
                   p$leaf$method_id, "-- l'arbre est un parcours pur (pas",
                   "d'aleatoire, contrat §1 : seed non applicable)")
    )
    mid <- .biostat_node_field(res, "method_id")
    expect_identical(
      mid, p$leaf$method_id,
      info = paste("resolve() ne retombe pas sur la feuille",
                   p$leaf$method_id)
    )
    np <- .biostat_node_path_of(res)
    expect_true(
      length(np) >= 2L,
      info = paste("contrat §2 : node_path de longueur >= 2 (feuille",
                   p$leaf$method_id, ")")
    )
    expect_identical(
      np[[1]], "root",
      info = "contrat §2 : le premier element de node_path est TOUJOURS 'root'"
    )
    expect_identical(
      np[[length(np)]], mid,
      info = paste("contrat §2 : le dernier element de node_path est la",
                   "feuille method_id (feuille", p$leaf$method_id, ")")
    )
    expect_false(
      any(np[-1L] == np[-length(np)]),
      info = paste("contrat §2 : node_path sans doublon consecutif (voie",
                   paste(np, collapse = " > "), ")")
    )
  }

  # (c) record de recommandation (contrat §2 -- champs geles), sur la voie
  # bulk documentee de la proposition §5 (>= 3 groupes, DESeq2 declare).
  idx_lrt <- which(vapply(paths, function(p)
    identical(p$leaf$method_id, "bulk_deseq2_lrt"), logical(1)))
  expect_true(
    length(idx_lrt) >= 1L,
    info = paste("feuille bulk_deseq2_lrt non joignable : la voie contractuelle",
                 "de la proposition §5 (comptages, replicats OK, >= 3 groupes)",
                 "doit exister dans l'arbre")
  )
  leaf_lrt <- paths[[idx_lrt[[1L]]]]
  np_lrt <- .biostat_node_path_of(biostat_resolve(tree, leaf_lrt$answers))
  rec <- biostat_decision_record(
    node_path      = np_lrt,
    method_id      = "bulk_deseq2_lrt",
    engine         = "DESeq2", # declare par l'utilisateur (contrat §2)
    rationale_keys = c("biostat.r.lrt"),
    prefilled      = list(groups = "three_plus", paired = FALSE),
    user_confirmed = TRUE
  )
  expect_identical(rec$version, "1.0.0", info = "contrat §2 : version '1.0.0'")
  expect_identical(
    rec$node_path[[1]], "root",
    info = "contrat §2 : racine 'root' dans le record"
  )
  expect_true(
    rec$method_id %in% .biostat_enum_13,
    info = "contrat §2 : method_id dans l'enum ferme §3"
  )
  expect_identical(
    rec$engine, "DESeq2",
    info = "contrat §2 : engine = moteur declare par l'utilisateur pour bulk_*"
  )
  expect_true(
    all(grepl("^biostat\\.r\\.", rec$rationale_keys)),
    info = paste("contrat §2/§5-I7 : rationale_keys = cles i18n biostat.r.*,",
                 "jamais de texte brut (l'existence des cles dans",
                 "translation.json releve du freeze §7, hors portee RED M2)")
  )
  expect_identical(
    rec$diagnostics, list(),
    info = paste("contrat §2/§5-I4 : diagnostics VIDE pour toute feuille",
                 "bulk_* et sc_da_delegate (hypothese binomiale negative)")
  )
  expect_true(
    identical(names(rec$prefilled), c("groups", "paired")) &&
      rec$prefilled$groups %in% c("two", "three_plus") &&
      is.logical(rec$prefilled$paired),
    info = "contrat §2 : prefilled = list(groups, paired) avec valeurs §3"
  )
  expect_true(is.logical(rec$user_confirmed), info = "contrat §2 : user_confirmed booleen")
  # AJUSTEMENT MINEUR (2026-11-01, passage VERT) : le testthat verrouille du
  # depot n'accepte pas info= sur expect_s3_class() — forme expect_true().
  expect_true(inherits(rec$timestamp, "POSIXct"),
              info = "contrat §2 : timestamp POSIXct")

  # engine vide pour toute feuille NON bulk (contrat §2 : « chaine vide sinon »)
  rec_sc <- biostat_decision_record(
    node_path      = c("root", "compositions", "sc_da_delegate"),
    method_id      = "sc_da_delegate",
    rationale_keys = c("biostat.r.sc_delegate"),
    prefilled      = list(groups = "two", paired = FALSE),
    user_confirmed = FALSE
  )
  expect_identical(
    rec_sc$engine, "",
    info = "contrat §2 : engine = chaine vide pour les feuilles non bulk_*"
  )

  # (d) provenance 4F -- les 4 champs d'identite geles du contrat §1
  # (analysis_id / method / parameters / dataset), via le canal EXISTANT
  # new_provenance_entry() -- aucune API nouvelle (proposition §6).
  ds <- data.frame( # fixture deterministe (jeu bulk factice, 6 echantillons)
    condition = factor(rep(c("A", "B", "C"), each = 2L)),
    row.names = sprintf("s%d", 1:6)
  )
  entry <- new_provenance_entry(
    analysis_id = "biostat_decision",
    method      = paste0("decision_tree_v", biostat_tree_version()),
    parameters  = list(
      node_path   = rec$node_path,
      method_id   = rec$method_id,
      engine      = rec$engine,
      rationale   = rec$rationale_keys,
      diagnostics = rec$diagnostics,
      prefilled   = rec$prefilled
    ),
    dataset = ds
  )
  expect_identical(
    entry$analysis_id, "biostat_decision",
    info = "contrat §1 : analysis_id constant 'biostat_decision'"
  )
  expect_identical(
    entry$method, "decision_tree_v1.0.0",
    info = "contrat §1 : method = 'decision_tree_v' ++ biostat_tree_version()"
  )
  # AJUSTEMENT MINEUR (2026-11-01, passage VERT) : pas de info= sur
  # expect_setequal (testthat verrouille) — contrat §1 : parameters = liste
  # plate aux 6 cles geles.
  expect_setequal(
    names(entry$parameters),
    c("node_path", "method_id", "engine", "rationale", "diagnostics", "prefilled")
  )
  expect_true(
    (!is.na(entry$dataset_hash)) && all(!is.na(entry$dataset_dims)),
    info = paste("contrat §1 : dataset = identite + hash du jeu actif (via",
                 ".provenance_dataset_identity())")
  )
})

# =============================================================================
# T5 -- entrees invalides : refus structure honnete (contrat §6, erreurs typees)
# =============================================================================
test_that("T5 entrees invalides : erreurs typees du vocabulaire gele §6", {
  expect_true(.biostat_tree_present, info = .biostat_red_info)
  if (!.biostat_tree_present) return(invisible(NULL))

  tree <- biostat_build_tree()
  paths <- .biostat_walk(tree)
  expect_true(length(paths) >= 1L, info = .biostat_red_info)

  # reponse manquante -> biostat_tree_missing_answer (contrat §6, ligne 1) :
  # on retire UNE reponse a une carte valide -- le refus est TYPE, jamais une
  # exception brute non classee.
  ans <- paths[[1]]$answers
  expect_true(length(ans) >= 1L, info = "carte de reponses vide dans le parcours")
  expect_error(
    biostat_resolve(tree, ans[-1L]),
    class = "biostat_tree_missing_answer",
    info = paste("contrat §6 : reponse manquante dans biostat_resolve() ->",
                 "erreur typee biostat_tree_missing_answer (refus structure,",
                 "pas d'exception brute)")
  )

  # method_id hors enum §3 -> biostat_tree_unknown_method (contrat §6, ligne 2)
  expect_error(
    biostat_decision_record(method_id = "cont_bogus"),
    class = "biostat_tree_unknown_method",
    info = paste("contrat §6 : method_id hors enum §3 -> erreur typee",
                 "biostat_tree_unknown_method (enum ferme)")
  )
})

# =============================================================================
# T6 -- pin C8 : ce test CITE le contrat (garde tools/check_conventions.R:502)
# =============================================================================
test_that("T6 pin C8 : le fichier de test cite BIOSTAT_DECISION_CONTRACT", {
  self <- paste(
    readLines(file.path(ts_project_root(), "tests", "testthat",
                        "test-biostat-tree.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_true(
    grepl("BIOSTAT_DECISION_CONTRACT", self, fixed = TRUE),
    info = paste("le test doit CITER le contrat BIOSTAT_DECISION_CONTRACT.md",
                 "-- garde C8 de tools/check_conventions.R (token",
                 "'biostat_decision' cherche dans les blobs de tests) ; dette",
                 "O26/M1 'C8 x2 dont BIOSTAT' : 2 -> 1 avertissements",
                 "(DECISIONS.md 2026-10-29)")
  )
})
