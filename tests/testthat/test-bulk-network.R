# =============================================================================
# test-bulk-network.R — interactome local + conversion d'identifiants (NEW-3)
# =============================================================================
# Couvre N-1 (UniProt -> SYMBOL : taux tracé, plancher DÉCLARÉ) et N-2
# (chargement + mémoïsation du réseau Reactome dédupliqué).
#
# ⚠️ Les tests qui construisent le réseau restent HORS LIGNE (reactome.db local,
# aucun accès réseau) mais coûtent ~10,6 s MESURÉS. Ils sont donc regroupés : le
# réseau n'est construit qu'une fois, la mémoïsation servant les autres tests.
# `skip_if_not_installed` protège un clone sans reactome.db — mais attention :
# reactome.db EST installé et au lockfile, donc ce skip ne doit PAS se
# déclencher ici (un skip qui masque une vraie erreur est un piège connu).
# =============================================================================
source_project_file("R/bulk/bulk_network.R")

# Réseau construit une seule fois pour tout le fichier (≈9 s), puis mémoïsé.
.network_fixture <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) cache <<- load_bulk_network()
    cache
  }
})

# Noms top-level non préfixés d'un point (doivent tous être dans l'API publique)
.network_top_level <- function() {
  exprs <- parse(file.path(ts_project_root(), "R/bulk/bulk_network.R"),
                 keep.source = FALSE)
  nms <- character(0)
  for (e in exprs) {
    if (is.call(e) && identical(deparse(e[[1L]]), "<-") && is.symbol(e[[2L]])) {
      nms <- c(nms, as.character(e[[2L]]))
    }
  }
  unique(nms)
}

test_that("surface publique figée, triée, sans doublon", {
  pub <- bulk_network_public_api()
  expect_setequal(pub, c(
    "assert_bulk_network_object", "assert_bulk_network_result",
    "build_bulk_network_table_export", "bulk_network_contract_fields",
    "bulk_network_error_state", "bulk_network_map_ids",
    "bulk_network_memo_clear", "bulk_network_node_roles",
    "bulk_network_pcsf_params", "bulk_network_pcsf_params_default",
    "bulk_network_public_api", "bulk_network_source_available",
    "bulk_network_species_supported",
    "bulk_network_species_unavailable_reason", "bulk_network_validity_states",
    "load_bulk_network", "plot_bulk_network", "run_bulk_network_pcsf"
  ))
  expect_false(anyDuplicated(pub) > 0L)
  expect_identical(pub, sort(pub))
  # Tout top-level NON pointé est public : une fonction ajoutée sans être
  # déclarée ici ferait échouer ce test (c'est le but).
  defined <- .network_top_level()
  expect_setequal(defined[!startsWith(defined, ".")], pub)
  # Les helpers internes restent préfixés d'un point.
  expect_true(all(c(".bulk_network_stop", ".bulk_network_config",
                    ".bulk_network_memo", ".bulk_network_spanning",
                    ".bulk_network_key_universe",
                    ".bulk_network_max_join_hops") %in% defined))
})

test_that("champs contractuels, états et rôles figés", {
  expect_setequal(bulk_network_contract_fields(), c(
    "type", "status", "nodes", "edges", "prizes", "node_role", "species",
    "source_db", "source_version", "id_type", "map_rate", "parameters",
    "qc", "warnings", "provenance", "analysis_id", "timestamp_utc"
  ))
  expect_setequal(bulk_network_validity_states(),
                  c("valid", "valid_with_warnings"))
  expect_setequal(bulk_network_node_roles(), c("terminal", "relay"))
})

# ── Espèce : la correction de prémisse est GELÉE ici --------------------------
test_that("seul hsapiens est disponible hors ligne, et le motif est explicite", {
  expect_identical(bulk_network_species_supported(), "hsapiens")
  # hsapiens n'a pas de motif d'indisponibilité
  expect_true(is.na(bulk_network_species_unavailable_reason("hsapiens")))
  # mmusculus : le motif doit nommer la VRAIE cause — la base de VOIES, pas
  # l'annotation de gènes. C'est la correction de la prémisse de §2bd.3.
  mm <- bulk_network_species_unavailable_reason("mmusculus")
  expect_true(is.character(mm) && nzchar(mm))
  expect_match(mm, "mmuReactome.db", fixed = TRUE)
  expect_match(mm, "HORS LIGNE", fixed = TRUE)
  expect_match(mm, "org.Mm.eg.db", fixed = TRUE)  # présent, mais insuffisant
  expect_true(bulk_network_source_available("hsapiens"))
  expect_false(bulk_network_source_available("mmusculus"))
  # entrées dégénérées : FALSE, jamais une erreur
  expect_false(bulk_network_source_available(NA_character_))
  expect_false(bulk_network_source_available(character(0)))
})

test_that("les erreurs sont classées et portent leur état", {
  e1 <- tryCatch(load_bulk_network("mmusculus"), error = function(e) e)
  expect_s3_class(e1, "bulk_network_error")
  expect_identical(bulk_network_error_state(e1), "source_unavailable")
  expect_match(conditionMessage(e1), "mmuReactome.db", fixed = TRUE)

  e2 <- tryCatch(load_bulk_network("hsapiens", source = "kegg"),
                 error = function(e) e)
  expect_identical(bulk_network_error_state(e2), "invalid_input")
  expect_match(conditionMessage(e2), "kegg", fixed = TRUE)

  e3 <- tryCatch(bulk_network_map_ids(42), error = function(e) e)
  expect_identical(bulk_network_error_state(e3), "invalid_input")
  e4 <- tryCatch(bulk_network_map_ids(character(0)), error = function(e) e)
  expect_identical(bulk_network_error_state(e4), "invalid_input")
  e5 <- tryCatch(bulk_network_map_ids("P05231", species = "mmusculus"),
                 error = function(e) e)
  expect_identical(bulk_network_error_state(e5), "source_unavailable")

  # une erreur NON classée rend NA, jamais une erreur
  expect_true(is.na(bulk_network_error_state(simpleError("autre"))))
})

# ── N-2 : mémoïsation (le réseau est construit ici, une seule fois) -----------
test_that("mémoïsation : pas de seconde construction, et pas de durée mensongère", {
  bulk_network_memo_clear()
  first <- load_bulk_network()
  expect_false(first$from_memo)
  expect_true(is.numeric(first$build_seconds))
  expect_true(first$build_seconds > 0)
  expect_true(is.finite(first$mean_degree))

  second <- load_bulk_network()
  expect_true(second$from_memo)
  # NA, pas la durée de la 1re construction : un chiffre faux vaut moins qu'un
  # NA honnête (défaut trouvé en exécutant le 2026-09-16).
  expect_true(is.na(second$build_seconds))
  expect_identical(second$edges, first$edges)
  expect_identical(second$nodes, first$nodes)
  expect_identical(second$n_edges, first$n_edges)

  bulk_network_memo_clear()
  expect_length(ls(envir = .bulk_network_memo), 0L)
})

# ── N-2 : cohérence du réseau ------------------------------------------------
test_that("le réseau est cohérent, dédupliqué et non orienté", {
  skip_if_not_installed("reactome.db")
  skip_if_not_installed("graphite")
  net <- .network_fixture()
  assert_bulk_network_object(net, context = "test")

  expect_identical(colnames(net$edges), c("from", "to"))
  expect_false(anyNA(net$edges$from) || anyNA(net$edges$to))
  # aucun auto-boucle, aucun préfixe « UNIPROT: » (les identifiants sont NUES)
  expect_false(any(net$edges$from == net$edges$to))
  expect_false(any(grepl(":", net$nodes, fixed = TRUE)))
  expect_identical(net$id_type, "UNIPROT")

  # compteurs cohérents avec la table (pas des chiffres recopiés)
  expect_identical(net$n_edges, nrow(net$edges))
  expect_identical(net$n_nodes, length(net$nodes))
  expect_identical(net$nodes, sort(unique(c(net$edges$from, net$edges$to))))
  expect_equal(net$mean_degree, 2 * net$n_edges / net$n_nodes)
  expect_equal(net$density,
               2 * net$n_edges / (net$n_nodes * (net$n_nodes - 1)))

  # DÉDUPLICATION : aucune paire non orientée ne revient deux fois
  key <- paste(pmin(net$edges$from, net$edges$to),
               pmax(net$edges$from, net$edges$to), sep = "\r")
  expect_false(anyDuplicated(key) > 0L)

  # provenance de la source
  expect_identical(net$source_db, "reactome")
  expect_identical(net$source_version,
                   as.character(utils::packageVersion("reactome.db")))
  expect_true(net$n_pathways > 0)
  expect_true(net$n_raw_edges >= net$n_edges)
  expect_true(net$n_dropped_non_protein >= 0)

  # ORDRE DE GRANDEUR mesuré le 2026-09-16 (le réseau ne doit pas se vider en
  # silence si `graphite` change de forme). Bornes LARGES : on gèle l'ordre de
  # grandeur, pas un chiffre exact, qui bougerait à chaque version de Reactome.
  expect_true(net$n_nodes > 5000)
  expect_true(net$n_edges > 100000)
})

# ── N-1 : conversion d'identifiants ------------------------------------------
test_that("la conversion UniProt -> SYMBOL trace son taux et ses ambiguïtés", {
  skip_if_not_installed("org.Hs.eg.db")
  net <- .network_fixture()
  set.seed(11)
  ids <- sample(net$nodes, 400)
  m <- suppressWarnings(bulk_network_map_ids(ids))

  expect_identical(m$n_input, length(ids))
  expect_identical(names(m$symbol), ids)        # alignement strict
  expect_identical(m$n_mapped, sum(!is.na(m$symbol)))
  expect_identical(m$n_unmapped, m$n_input - m$n_mapped)
  expect_equal(m$map_rate, m$n_mapped / m$n_input)
  expect_identical(m$annotation_db, "org.Hs.eg.db")
  # les non-convertis sont listés, et disjoints des convertis
  expect_length(m$unmapped_ids, m$n_unmapped)
  expect_length(intersect(m$unmapped_ids, names(m$symbol)[!is.na(m$symbol)]), 0L)
  # partition de l'entrée : dans l'espace + hors espace = total, et tout ce qui
  # est converti est forcément dans l'espace
  expect_identical(m$n_in_space + m$n_out_of_space, m$n_input)
  expect_true(m$n_mapped <= m$n_in_space)
  # ordre de grandeur mesuré : ~95 % sur le réseau entier
  expect_true(m$map_rate > 0.80)
  expect_false(m$below_floor)
})

test_that("le plancher de correspondance est DÉCLARÉ et se déclenche (cas négatif)", {
  skip_if_not_installed("org.Hs.eg.db")
  # Le plancher est lu dans config/thresholds.R (auto-sourcé par le helper).
  expect_true(exists("TS_BULK_NETWORK_MIN_MAP_RATE", inherits = TRUE))
  expect_identical(.bulk_network_config("TS_BULK_NETWORK_MIN_MAP_RATE", NA_real_),
                   TS_BULK_NETWORK_MIN_MAP_RATE)

  # CAS NÉGATIF RÉEL : 60 identifiants qui ne sont pas des accessions UniProt.
  # ⚠️ Ce cas PLANTAIT avant le correctif : `AnnotationDbi::mapIds()` échoue
  # (« None of the keys entered are valid keys for 'UNIPROT' ») quand AUCUNE clé
  # n'est dans l'espace, au lieu de rendre NA. Un plancher de correspondance qui
  # lève une erreur d'AnnotationDbi au lieu de rendre un taux de 0 % ne remplit
  # pas son rôle : il cache la cause au lieu de la nommer.
  bogus <- sprintf("ZZZZZ%04d", 1:60)
  m <- suppressWarnings(bulk_network_map_ids(bogus))
  expect_identical(m$n_mapped, 0L)
  expect_identical(m$map_rate, 0)
  expect_true(m$below_floor)          # le garde-fou MORD
  expect_true(all(is.na(m$symbol)))   # alignement conservé, valeurs NA
  expect_identical(names(m$symbol), bogus)
  # et la CAUSE est nommée, pas devinée : les 60 sont hors espace de clés
  expect_identical(m$n_out_of_space, 60L)
  expect_identical(m$n_in_space, 0L)
  expect_identical(m$n_unmapped, 60L)

  # et le seuil est bien celui de la config, pas une constante en dur
  old <- get("TS_BULK_NETWORK_MIN_MAP_RATE", inherits = TRUE)
  assign("TS_BULK_NETWORK_MIN_MAP_RATE", 1.5, envir = globalenv())
  on.exit(assign("TS_BULK_NETWORK_MIN_MAP_RATE", old, envir = globalenv()),
          add = TRUE)
  net <- .network_fixture()
  set.seed(12)
  m2 <- suppressWarnings(bulk_network_map_ids(sample(net$nodes, 50)))
  expect_true(m2$below_floor)         # 1.5 est inatteignable -> doit mordre
  expect_identical(m2$min_map_rate, 1.5)
})

test_that("les clés hors espace sont distinguées d'un simple échec de symbole", {
  skip_if_not_installed("org.Hs.eg.db")
  # Mélange : 2 accessions réelles + 3 chaînes hors espace. Le résultat doit
  # rester ALIGNÉ sur l'entrée (NA en place), et compter la cause.
  mix <- c("P05231", "P01579", "ZZZZZ0001", "ZZZZZ0002", "ZZZZZ0003")
  r <- suppressWarnings(bulk_network_map_ids(mix))
  expect_identical(names(r$symbol), mix)
  expect_identical(unname(r$symbol[1:2]), c("IL6", "IFNG"))
  expect_true(all(is.na(r$symbol[3:5])))
  expect_identical(r$n_mapped, 2L)
  expect_identical(r$n_out_of_space, 3L)
  expect_identical(r$n_in_space, 2L)
  expect_identical(r$n_unmapped, 3L)
  expect_identical(r$map_rate, 2 / 5)

  # L'espace de clés est mémoïsé (116 570 clés, 0,14 s mesurés) : le recalculer
  # à chaque appel coûterait plus cher que la conversion elle-même.
  expect_true(any(grepl("^keyspace::", ls(envir = .bulk_network_memo))))
  bulk_network_memo_clear()
  expect_length(ls(envir = .bulk_network_memo), 0L)
})

test_that("sur le réseau réel, la perte de conversion est un TROU D'ESPACE DE CLÉS", {
  skip_if_not_installed("org.Hs.eg.db")
  # ⚠️ Constat MESURÉ le 2026-09-16, gelé ici parce qu'il change la lecture du
  # 4,6 % manquant : 507 nœuds hors espace de clés UNIPROT, 507 non convertis,
  # et AUCUN nœud « dans l'espace mais sans symbole ». Ce n'est donc pas un trou
  # d'annotation SYMBOL qu'on pourrait corriger, c'est une limite d'org.Hs.eg.db
  # sur des accessions Reactome — une conclusion qui doit rester visible.
  net <- .network_fixture()
  m <- suppressWarnings(bulk_network_map_ids(net$nodes))
  expect_identical(m$n_input, net$n_nodes)
  expect_identical(m$n_unmapped, m$n_out_of_space)
  expect_identical(m$n_in_space - m$n_mapped, 0L)
  expect_true(m$n_out_of_space > 0L)
  # ordre de grandeur : ~95 % converti (le chiffre exact bougera avec la version
  # de reactome.db, la proportion non)
  expect_true(m$map_rate > 0.90 && m$map_rate < 0.99)
  expect_false(m$below_floor)
})

test_that("assert_bulk_network_object refuse un objet non canonique", {
  expect_true(isTRUE(assert_bulk_network_object(.network_fixture())))
  e <- tryCatch(assert_bulk_network_object(list()), error = function(e) e)
  expect_s3_class(e, "bulk_network_error")
  expect_identical(bulk_network_error_state(e), "invalid_input")
  e2 <- tryCatch(assert_bulk_network_object(NULL), error = function(e) e)
  expect_identical(bulk_network_error_state(e2), "invalid_input")
})

# =============================================================================
# N-3 — moteur PCSF heuristique
# =============================================================================

# Réseau SYNTHÉTIQUE à sous-réseau optimal CONNU PAR CONSTRUCTION (fixture §7).
#   A-B-C-D      chaîne de 4 (le chemin A..D fait 3 arêtes)
#   E-F-G        chaîne de 3, sans prime
#   X-Y          arête isolée, sans prime
# Primes sur A et D -> le seul sous-graphe reliant les deux est A-B-C-D.
.network_synthetic <- function() {
  e <- data.frame(
    from = c("A", "B", "C", "E", "F", "X"),
    to   = c("B", "C", "D", "F", "G", "Y"),
    stringsAsFactors = FALSE)
  nd <- sort(unique(c(e$from, e$to)))
  list(edges = e, nodes = nd, n_edges = nrow(e), n_nodes = length(nd),
       mean_degree = 2 * nrow(e) / length(nd),
       density = 2 * nrow(e) / (length(nd) * (length(nd) - 1)),
       n_pathways = 1L, n_raw_edges = nrow(e), n_dropped_non_protein = 0L,
       source_db = "reactome", source_version = "synthetique", id_type = "UNIPROT",
       build_seconds = NA_real_, from_memo = TRUE)
}

test_that("le moteur retrouve le sous-réseau optimal d'une fixture connue", {
  net <- .network_synthetic()
  # convert_ids = FALSE : la fixture est déjà en « accessions » (pas d'org.Hs.eg.db)
  r <- run_bulk_network_pcsf(c(A = 10, D = 10), network = net, convert_ids = FALSE)

  # LE résultat attendu par construction : la chaîne A-B-C-D, rien de plus.
  # Ni la chaîne E-F-G ni l'arête X-Y ne sont ajoutées (aucune prime à relier).
  expect_identical(sort(r$nodes$node), c("A", "B", "C", "D"))
  expect_identical(r$edges$from, c("A", "B", "C"))
  expect_identical(r$edges$to, c("B", "C", "D"))

  # Rôles : les primes sont terminales, les intermédiaires sont des RELAIS
  # (prédits, non mesurés — c'est la distinction que l'UI doit exposer).
  expect_identical(unname(r$node_role[["A"]]), "terminal")
  expect_identical(unname(r$node_role[["D"]]), "terminal")
  expect_identical(unname(r$node_role[["B"]]), "relay")
  expect_identical(unname(r$node_role[["C"]]), "relay")
  expect_identical(r$qc$n_trees, 1L)
  expect_identical(r$qc$n_relay, 2L)
  # score = 20 − ω·1 − β·3 − μ·2 = 20 − 10 − 3 − 2 = 5
  expect_equal(r$qc$score, 5)
  expect_identical(r$status, "valid")

  # structure en FORÊT : arêtes = nœuds − composantes (invariant fort)
  expect_identical(nrow(r$edges), nrow(r$nodes) - r$qc$n_trees)
  expect_true(isTRUE(assert_bulk_network_result(r, "synthetique")))
})

test_that("l'élagage retire les feuilles SANS prime", {
  net <- .network_synthetic()
  # Primes sur A et B : la queue C-D ne sert à rien et doit disparaître.
  r <- run_bulk_network_pcsf(c(A = 10, B = 10), network = net, convert_ids = FALSE)
  expect_identical(sort(r$nodes$node), c("A", "B"))
  expect_identical(nrow(r$edges), 1L)
  expect_identical(r$qc$n_relay, 0L)
  expect_identical(r$qc$n_trees, 1L)
})

test_that("le critère de rentabilité est DÉCLARÉ et gouverne réellement la fusion", {
  net <- .network_synthetic()
  # max_join_hops rend « ω = 10 » interprétable : d ≤ 5 arêtes.
  p <- bulk_network_pcsf_params()
  expect_identical(p$omega, 10)
  expect_identical(p$beta, 1)
  expect_identical(p$mu, 1)
  expect_identical(p$max_join_hops, 5)

  # ω = 1 est le cas DÉGÉNÉRÉ mesuré le 2026-09-16 : (ω+μ)/(β+μ) = 1, donc
  # aucune paire à distance ≥ 1 n'est rentable -> le moteur ne relie RIEN.
  # C'est précisément pourquoi les défauts ne peuvent pas être ω = 1.
  p1 <- bulk_network_pcsf_params(list(omega = 1))
  expect_identical(p1$max_join_hops, 0)
  r1 <- run_bulk_network_pcsf(c(A = 10, D = 10), network = net,
                              convert_ids = FALSE, params = list(omega = 1))
  expect_identical(r1$qc$n_trees, 2L)          # deux terminaux isolés
  expect_identical(nrow(r1$edges), 0L)
  expect_identical(r1$qc$n_relay, 0L)

  # et le seuil est bien CELUI DES PARAMÈTRES, pas une constante en dur :
  # ω = 4 -> (4+1)/2 = 2.5 -> d ≤ 2, insuffisant pour A..D (3 arêtes)
  r2 <- run_bulk_network_pcsf(c(A = 10, D = 10), network = net,
                              convert_ids = FALSE, params = list(omega = 4))
  expect_identical(r2$qc$n_trees, 2L)
  expect_identical(bulk_network_pcsf_params(list(omega = 4))$max_join_hops, 2)
  # ω = 5 -> (5+1)/2 = 3 -> d ≤ 2 (strict) ; ω = 6 -> d ≤ 3 -> la fusion passe
  expect_identical(bulk_network_pcsf_params(list(omega = 5))$max_join_hops, 2)
  expect_identical(bulk_network_pcsf_params(list(omega = 6))$max_join_hops, 3)
  r3 <- run_bulk_network_pcsf(c(A = 10, D = 10), network = net,
                              convert_ids = FALSE, params = list(omega = 6))
  expect_identical(r3$qc$n_trees, 1L)
  expect_identical(sort(r3$nodes$node), c("A", "B", "C", "D"))
})

test_that("les paramètres refusent les valeurs absurdes", {
  expect_error(bulk_network_pcsf_params(list(omega = -1)), class = "bulk_network_error")
  expect_error(bulk_network_pcsf_params(list(beta = NA_real_)), class = "bulk_network_error")
  expect_error(bulk_network_pcsf_params(list(mu = "x")), class = "bulk_network_error")
  expect_error(bulk_network_pcsf_params(list(max_nodes = 1)), class = "bulk_network_error")
  e <- tryCatch(bulk_network_pcsf_params(list(omega = -1)), error = function(e) e)
  expect_identical(bulk_network_error_state(e), "invalid_input")
})

test_that("le moteur est DÉTERMINISTE (aucun tirage) et le déclare", {
  net <- .network_synthetic()
  pri <- c(A = 10, D = 10, G = 7)
  r1 <- run_bulk_network_pcsf(pri, network = net, convert_ids = FALSE)
  r2 <- run_bulk_network_pcsf(pri, network = net, convert_ids = FALSE)
  expect_identical(r1$nodes, r2$nodes)
  expect_identical(r1$edges, r2$edges)
  expect_identical(r1$node_role, r2$node_role)
  # l'ordre des lignes ne doit pas dépendre d'un hachage
  expect_identical(r1$nodes$node, sort(r1$nodes$node))
  # aucune graine n'est revendiquée : un NA honnête vaut mieux qu'un chiffre faux
  expect_false(r1$qc$seed_used)
  expect_true(is.na(r1$provenance$seed))
  expect_match(r1$provenance$seed_reason, "déterministe", fixed = TRUE)
})

test_that("le seuil est DÉCLARÉ et filtre explicitement", {
  net <- .network_synthetic()
  # sans seuil : A(10) et B(-3) -> B écartée comme NON POSITIVE, pas comme
  # « sous le seuil » — les deux causes sont comptées séparément.
  r <- run_bulk_network_pcsf(c(A = 10, B = -3, C = 0, D = 10), network = net,
                             convert_ids = FALSE)
  expect_identical(r$qc$n_prizes_input, 4L)
  expect_identical(r$qc$n_prizes_non_positive, 2L)
  expect_identical(r$qc$n_prizes_below_threshold, 0L)
  expect_identical(r$qc$n_terminals, 2L)

  # avec seuil : D(10) passe, A(10) passe ; seuil 15 -> plus rien
  r2 <- run_bulk_network_pcsf(c(A = 10, D = 10, G = 20), network = net,
                              convert_ids = FALSE, threshold = 15)
  expect_identical(r2$qc$n_terminals, 1L)
  expect_identical(r2$qc$n_prizes_below_threshold, 2L)
  expect_true(is.null(r2$qc$threshold) || identical(r2$qc$threshold, 15))
  expect_identical(r2$qc$threshold, 15)

  # seuil qui ne laisse RIEN : refus classé, jamais un réseau vide silencieux
  e <- tryCatch(run_bulk_network_pcsf(c(A = 10, D = 10), network = net,
                                      convert_ids = FALSE, threshold = 99),
                error = function(e) e)
  expect_s3_class(e, "bulk_network_error")
  expect_identical(bulk_network_error_state(e), "invalid_input")
  expect_match(conditionMessage(e), "seuil", fixed = TRUE)

  # seuil non numérique
  e2 <- tryCatch(run_bulk_network_pcsf(c(A = 10), network = net,
                                       convert_ids = FALSE, threshold = "x"),
                 error = function(e) e)
  expect_identical(bulk_network_error_state(e2), "invalid_input")
})

test_that("recouvrement nul et primes illisibles sont refusés (jamais vides)", {
  net <- .network_synthetic()
  e <- tryCatch(run_bulk_network_pcsf(c(ZZZ = 5, YYY = 3), network = net,
                                      convert_ids = FALSE),
                error = function(e) e)
  expect_s3_class(e, "bulk_network_error")
  expect_identical(bulk_network_error_state(e), "no_overlap")
  expect_match(conditionMessage(e), "recouvrement NUL", fixed = TRUE)

  # table sans colonne de gène ou sans colonne de score
  e2 <- tryCatch(run_bulk_network_pcsf(data.frame(x = 1), network = net,
                                       convert_ids = FALSE),
                 error = function(e) e)
  expect_identical(bulk_network_error_state(e2), "invalid_input")
  e3 <- tryCatch(run_bulk_network_pcsf(data.frame(gene = "A"), network = net,
                                       convert_ids = FALSE),
                 error = function(e) e)
  expect_identical(bulk_network_error_state(e3), "invalid_input")
  # ni data.frame ni vecteur nommé
  e4 <- tryCatch(run_bulk_network_pcsf(c(1, 2, 3), network = net,
                                       convert_ids = FALSE),
                 error = function(e) e)
  expect_identical(bulk_network_error_state(e4), "invalid_input")
  e5 <- tryCatch(run_bulk_network_pcsf("A", network = net, convert_ids = FALSE),
                 error = function(e) e)
  expect_identical(bulk_network_error_state(e5), "invalid_input")
})

test_that("le plafond de primes REFUSE au lieu de geler la session", {
  net <- .network_synthetic()
  # le plafond vient de config/thresholds.R, il n'est pas recopié
  expect_true(exists("TS_BULK_NETWORK_MAX_NODES", inherits = TRUE))
  expect_identical(bulk_network_pcsf_params()$max_nodes,
                   TS_BULK_NETWORK_MAX_NODES)
  # 300 primes sur un réseau de 11 nœuds : le plafond mord AVANT le recouvrement
  big <- stats::setNames(rep(1, 300), sprintf("G%03d", 1:300))
  e <- tryCatch(run_bulk_network_pcsf(big, network = net, convert_ids = FALSE),
                error = function(e) e)
  expect_s3_class(e, "bulk_network_error")
  expect_identical(bulk_network_error_state(e), "too_many_prizes")
  expect_match(conditionMessage(e), "TS_BULK_NETWORK_MAX_NODES", fixed = TRUE)
})

test_that("assert_bulk_network_result refuse un résultat non canonique", {
  net <- .network_synthetic()
  r <- run_bulk_network_pcsf(c(A = 10, D = 10), network = net, convert_ids = FALSE)
  expect_true(isTRUE(assert_bulk_network_result(r)))

  # une liste vide n'est pas « invalide » mais INCOMPLÈTE : l'état
  # `contract_violation` est plus informatif (il nomme les champs manquants)
  # qu'un `invalid_input` générique.
  e <- tryCatch(assert_bulk_network_result(list()), error = function(e) e)
  expect_s3_class(e, "bulk_network_error")
  expect_identical(bulk_network_error_state(e), "contract_violation")
  e0 <- tryCatch(assert_bulk_network_result(NULL), error = function(e) e)
  expect_identical(bulk_network_error_state(e0), "invalid_input")
  # champ contractuel manquant -> contract_violation, état distinct
  broken <- r
  broken$provenance <- NULL
  e2 <- tryCatch(assert_bulk_network_result(broken), error = function(e) e)
  expect_identical(bulk_network_error_state(e2), "contract_violation")
  expect_match(conditionMessage(e2), "provenance", fixed = TRUE)
  # type inattendu
  wrong <- r
  wrong$type <- "autre"
  e3 <- tryCatch(assert_bulk_network_result(wrong), error = function(e) e)
  expect_identical(bulk_network_error_state(e3), "invalid_input")
  # rôle hors contrat
  bad <- r
  bad$nodes$role[1] <- "hub"
  e4 <- tryCatch(assert_bulk_network_result(bad), error = function(e) e)
  expect_identical(bulk_network_error_state(e4), "invalid_input")
})

test_that("le résultat respecte le contrat gelé, y compris sur le vrai réseau", {
  skip_if_not_installed("igraph")
  net <- .network_fixture()
  set.seed(7)
  acc <- sample(net$nodes, 40L)
  pri <- stats::setNames(round(stats::rexp(40L, rate = 0.15) + 1.3, 3), acc)
  r <- run_bulk_network_pcsf(pri, network = net, convert_ids = FALSE)

  expect_identical(names(r), bulk_network_contract_fields())
  expect_identical(r$type, "bulk_network_pcsf")
  expect_true(r$status %in% bulk_network_validity_states())
  expect_true(isTRUE(assert_bulk_network_result(r, "reel")))
  # rôles : exactement le vocabulaire du contrat, et chaque terminal porte une prime
  expect_true(all(r$nodes$role %in% bulk_network_node_roles()))
  expect_true(all(r$nodes$prize[r$nodes$role == "terminal"] > 0))
  expect_true(all(is.na(r$nodes$prize[r$nodes$role == "relay"])))
  # INVARIANT DE FORÊT : un sous-graphe sans cycle vérifie arêtes = nœuds − composantes.
  # C'est ce qui prouve que l'arbre couvrant fait son travail (les chemins les
  # plus courts se recouvrent et créeraient des cycles sans lui).
  expect_identical(nrow(r$edges), nrow(r$nodes) - r$qc$n_trees)
  expect_identical(r$qc$n_nodes, nrow(r$nodes))
  expect_identical(r$qc$n_edges, nrow(r$edges))
  expect_identical(r$qc$n_relay, sum(r$nodes$role == "relay"))
  # aucune arête ne référence un nœud absent du sous-graphe
  expect_true(all(c(r$edges$from, r$edges$to) %in% r$nodes$node))
  # provenance
  expect_identical(r$source_db, "reactome")
  expect_identical(r$id_type, "UNIPROT")
  expect_true(nzchar(r$timestamp_utc))
  expect_true(nzchar(r$analysis_id))
  expect_match(r$qc$algorithm, "HEURISTIQUE", fixed = TRUE)
})

test_that("le moteur tient le plafond déclaré en un temps borné (mesuré)", {
  skip_if_not_installed("igraph")
  net <- .network_fixture()
  # Mesuré le 2026-09-16 : 0,69 s pour 200 primes. La première implémentation
  # (un data.frame par paire de groupes) dépassait 10 min au même plafond — et
  # une variante intermédiaire BOUCLAIT à l'infini. Le seuil ci-dessous est
  # large (30 s) : il détecte une régression d'ordre de grandeur, pas une
  # fluctuation machine.
  set.seed(99)
  acc <- sample(net$nodes, TS_BULK_NETWORK_MAX_NODES)
  pri <- stats::setNames(round(stats::rexp(length(acc), rate = 0.15) + 1.3, 3), acc)
  t0 <- Sys.time()
  r <- run_bulk_network_pcsf(pri, network = net, convert_ids = FALSE)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_true(elapsed < 30)
  expect_true(isTRUE(assert_bulk_network_result(r, "plafond")))
  expect_identical(r$qc$n_terminals, as.integer(TS_BULK_NETWORK_MAX_NODES))
  # le sous-graphe ne peut pas dépasser les nœuds du réseau
  expect_true(r$qc$n_nodes <= net$n_nodes)
})

# =============================================================================
# N-4bis (2026-10-03) : build_bulk_network_table_export — degré sous-graphe
# =============================================================================
# 🆕 Régression mesurée (route d'export drive, Slice 2.3) : `tabulate(match(...))`
# sans `nb = nrow(nd)` dimensionne son vecteur au plus grand index PRÉSENT — un
# DERNIER nœud sans arête produisait un `degree` plus court que `nd`, et
# data.frame() levait "differing number of rows". Le téléchargement humain
# `dl_network` échouait exactement pareil. Le zéro est la valeur honnête.
test_that("build_bulk_network_table_export: un dernier noeud sans arete vaut degree 0", {
  res <- list(
    type = "bulk_network_pcsf", status = "valid",
    nodes = data.frame(node = c("a", "b", "c", "d"),
                       symbol = c("T", "E", "M", "A"),
                       role = c("terminal", "relay", "terminal", "relay"),
                       prize = c(2.5, 1, 3, 0.5), stringsAsFactors = FALSE),
    edges = data.frame(from = c("a", "a", "c"), to = c("b", "c", "b"),
                       stringsAsFactors = FALSE),
    prizes = NULL, node_role = NULL, species = "human", source_db = "string",
    source_version = "v12", id_type = "ensembl_gene", map_rate = 0.9,
    parameters = list(beta = 1), qc = list(), warnings = character(0),
    provenance = "fixture", analysis_id = "reg-degre", timestamp_utc = "t")
  tab <- build_bulk_network_table_export(res)
  expect_identical(nrow(tab), 4L)
  expect_identical(as.integer(tab$degree), c(2L, 2L, 2L, 0L),
                   info = "le noeud isole du bout vaut zero, pas un vecteur plus court")
})
