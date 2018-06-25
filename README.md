# Bazasso

Ce projet a pour but de tester la pile Haskell/Yesod/Sqlite3 sur le répertoire
nationale des associations (France).

Cela permettra à terme de rédiger un retour d'expérience sur un développement
IRL utilisant ces technologies.

## Génération de la base de données

Bazasso utilise une base de données Sqlite3 en lecture seule.

Le projet requiert un fichier `associations.db` généré à partir des données
ouvertes des associatiions (RNA+JOAFE).

Pour générer cette base de données (comptez 2 Go), il faut utiliser le script
`creation-base-associations.bash` situé dans le sous-répertoire
`base-associations`.

Mais il faut auparavant placer les fichiers suivants dans le sous-répertoire
`base-associations/source`:

- `ASS*.taz` (récupérer tous les fichiers `.taz` possibles depuis 
   https://echanges.dila.gouv.fr/OPENDATA/ASSOCIATIONS/ )
- `EUCircos_Regions_departements_circonscriptions_communes_gps.csv.gz`
  (récupérer à http://www.nosdonnees.fr/wiki/index.php/Fichier:EUCircos_Regions_departements_circonscriptions_communes_gps.csv.gz )
- `rna_import_*.zip`, `rna_waldec_*.zip` (récupérer la dernière version à
  https://www.data.gouv.fr/fr/datasets/repertoire-national-des-associations/ )

La base de données `associations.db` doit être placée à la racine du projet.

## Exécution

Une fois compilé, le projet peut être lancé par `stack exec associations`.

## Compilation
### Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Build libraries: `stack build`

If you have trouble, refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for additional detail.

### Development

Start a development server with:

```
stack exec -- yesod devel
```

As your code changes, your site will be automatically recompiled and redeployed to localhost.

### Tests

```
stack test --flag associations:library-only --flag associations:dev
```

(Because `yesod devel` passes the `library-only` and `dev` flags, matching those flags means you don't need to recompile between tests and development, and it disables optimization to speed up your test compile times).
