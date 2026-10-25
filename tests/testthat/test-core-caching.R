# =============================================================================
# test-core-caching.R — memoisation a portee contrainte R/core/caching.R (2D)
# =============================================================================
# Couvre : determinisme de cle, enforcement STRICT du perimetre (5 scopes),
# hit/miss, ecrasement, eviction FIFO, no-op NULL et clear().

source_project_file("tools/pkg_skeleton/R/caching.R")

test_that("cerberus_cache_key is deterministic for identical inputs", {
  k1 <- cerberus_cache_key("trajectory", matrix(1:6, 2), list(k = 5))
  k2 <- cerberus_cache_key("trajectory", matrix(1:6, 2), list(k = 5))
  expect_identical(k1, k2)
  expect_match(k1, "^trajectory:[0-9a-f]+$")
})

test_that("cerberus_cache_key differs when any ingredient changes", {
  base <- cerberus_cache_key("trajectory", matrix(1:6, 2), list(k = 5))
  expect_false(identical(base, cerberus_cache_key("trajectory", matrix(1:6, 2), list(k = 6))))
  expect_false(identical(base, cerberus_cache_key("trajectory", matrix(1:8, 2), list(k = 5))))
  expect_false(identical(base, cerberus_cache_key("markers", matrix(1:6, 2), list(k = 5))))
})

test_that("cerberus_cache_key refuses any scope outside the 5 allowed ones", {
  expect_error(cerberus_cache_key("anything_else", 1), "non autorisee")
  expect_error(cerberus_cache_key("deconv", 1), "non autorisee")  # nom exact requis
  for (scope in c("trajectory", "velocity", "markers", "pathways", "spatial_deconv")) {
    expect_match(cerberus_cache_key(scope, 1), paste0("^", scope, ":"), info = scope)
  }
})

test_that("cache get/set round-trips and misses return NULL", {
  on.exit(cerberus_cache_clear(), add = TRUE)
  key <- cerberus_cache_key("pathways", list(db = "KEGG", genes = letters[1:5]))
  expect_null(cerberus_cache_get(key))            # miss
  val <- data.frame(pathway = c("P1", "P2"), p = c(0.01, 0.2))
  expect_invisible(cerberus_cache_set(key, val))
  expect_identical(cerberus_cache_get(key), val)  # hit
})

test_that("cache set overwrites an existing key", {
  on.exit(cerberus_cache_clear(), add = TRUE)
  key <- cerberus_cache_key("markers", "v1")
  cerberus_cache_set(key, "first")
  cerberus_cache_set(key, "second")
  expect_identical(cerberus_cache_get(key), "second")
})

test_that("storing NULL is an explicit no-op (indistinguishable from a miss)", {
  on.exit(cerberus_cache_clear(), add = TRUE)
  key <- cerberus_cache_key("velocity", "x")
  expect_invisible(cerberus_cache_set(key, NULL))
  expect_null(cerberus_cache_get(key))
})

test_that("cache get/set refuse a forged key outside the allowed scopes", {
  expect_error(cerberus_cache_get("hack:abcdef"), "non autorisee")
  expect_error(cerberus_cache_set("hack:abcdef", 1), "non autorisee")
})

test_that("FIFO eviction caps the store at CERBERUS_CACHE_MAX_ENTRIES", {
  cerberus_cache_clear()
  on.exit(cerberus_cache_clear(), add = TRUE)
  keys <- vapply(seq_len(CERBERUS_CACHE_MAX_ENTRIES + 5L),
                 function(i) cerberus_cache_key("trajectory", paste0("item-", i)),
                 character(1))
  for (k in keys) cerberus_cache_set(k, paste0("val-", k))
  expect_null(cerberus_cache_get(keys[1]))        # le plus ancien a ete evicté
  expect_null(cerberus_cache_get(keys[5]))
  expect_identical(cerberus_cache_get(keys[length(keys)]),
                   paste0("val-", keys[length(keys)]))  # le plus recent reste
})

test_that("cerberus_cache_clear empties the whole cache", {
  key <- cerberus_cache_key("spatial_deconv", "ref")
  cerberus_cache_set(key, list(props = c(0.1, 0.9)))
  expect_true(cerberus_cache_clear())
  expect_null(cerberus_cache_get(key))
})
