# FREEZE — état de départ du prochain jalon (milestone-start)

Établi : **2026-09-24 15:2x** (Europe/Paris) — juste après la CLÔTURE du mandat B.
⚠️ Ce fichier est **descriptif** ; la preuve est le manifeste `manifest-milestone-start.txt`.

## Point de départ : l'état SÉPARÉ (accepté)

- `HEAD` = **`9cb1a06c3a14dfd2a9da7acba515868606c85b1c`**
- Les trois commits acceptés, **non amendés**, historique **linéaire** (reflog vérifié) :
  - `d211f8b` `feat(bulk): pathway state publisher` — 2026-09-24 14:26:30
  - `7e85967` `wip(sc): LIANA/scRNA communication workstream` — 2026-09-24 14:26:52
  - `9cb1a06` `fix(bulk_de): the DE job publishes 'error' when the DE raises, not 'invalid'` — 15:02:12
- Arbre de travail : **seul** ` M renv.lock` (non commité, **consigne** : ne pas le modifier).

## Empreinte de gel

- Fichier : `.workbuddy-ai/freeze/manifest-milestone-start.txt`
- Format : `sha256sum` (une ligne = `hash *./chemin`), **trié par chemin**.
- **437** fichiers.
- **Empreinte agrégée : `451a7a4ceb0d9ef1…`** (`sha256sum manifest-milestone-start.txt`).
- 🔑 **Stabilité prouvée** : deux relevés consécutifs **byte-identiques** (`cmp` OK).
- 🔑 **Égalité avec l'état VÉRIFIÉ** : cette empreinte est **identique** au `451a7a4c…` du run DE
  accepté ⇒ l'état séparé **EST** l'état qui a produit le BILAN accepté
  (`failed=0 passed=8091 error=0 skipped=9`) — pas seulement un état « équivalent ».

## Ensemble d'exclusion (identique aux manifestes précédents, validé par reproduction)

Exclus : `renv/`, `python_env_sccoda/`, `.git/`, `QC/`, `.Rproj.user/`, `node_modules/`,
`.workbuddy-ai/`, `tools/_drive/`, et le fichier `full_suite_results.txt`.
✅ **Validé** : le `find` ci-dessous reproduit **exactement** la liste de 437 chemins de
`manifest-de-before.txt` (diff vide) ⇒ l'ensemble d'exclusion n'a **pas** dérivé.

```bash
find . \
  -path ./renv -prune -o -path ./python_env_sccoda -prune -o -path ./.git -prune -o \
  -path ./QC -prune -o -path ./.Rproj.user -prune -o -path ./node_modules -prune -o \
  -path ./.workbuddy-ai -prune -o -path ./tools/_drive -prune -o \
  -type f ! -name full_suite_results.txt -print0 \
  | sort -z | xargs -0 sha256sum
```
⚠️ **Ne PAS** hacher fichier par fichier (`xargs -0 -n1 …` / boucle `while read`) : sur cet hôte la
pipeline est **tuée par SIGTERM**. Un **seul** appel `xargs -0 sha256sum` passe.

## Dette / angles morts reportés (NE PAS corriger dans le prochain jalon)

1. **Divergence** avertissement renv « packages recorded in the lockfile are not installed » vs
   garde d'hermétisme `0 erreur(s)` — **consignée, non expliquée**.
2. **14 dérives RÉELLES de version** (`future`, `bslib`, `igraph`, `xml2`, `sf`, `spatstat.*`,
   `bbotk`, `bit64`, `class`, `hexbin`, `mlr3learners`, `nnet`) — **consignées, non corrigées**
   (corriger exigerait `renv.lock`, **interdit**).
3. **`renv/` est HORS du manifeste de gel** ⇒ geler l'arbre **ne gèle pas la bibliothèque**.

## Règle opératoire du prochain jalon

1. **Re-hacher** ce manifeste **AVANT le premier run de test** ; exiger `451a7a4ceb0d9ef1…`.
   Un écart ⇒ **JETER** la mesure, ne pas la fausser.
2. Ne pas modifier `renv.lock` ; ne pas amender les trois commits ; ne pas réécrire l'historique.
3. Mesurer l'hermétisme (il **change sous vos pieds**), ne jamais recopier « 0 » ou « 11 ».
