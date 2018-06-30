# Bazasso

Ce projet a pour but de tester la pile Haskell/Yesod/Sqlite3 sur le répertoire
nationale des associations (France).

Cela permettra à terme de rédiger un retour d'expérience sur un développement
IRL utilisant ces technologies.

## Génération de la base de données

Bazasso utilise une base de données Sqlite3 en lecture seule.

Le projet requiert un fichier `associations.db` généré à partir des données
ouvertes des associatiions (RNA+JOAFE+Sirene).

### Récupération des données ouvertes

La récupération des données ouvertes se fait via le script
`telecharge-donnees-ouvertes.bash` situé dans le sous-répertoire
`base-associations`.

Pré-requis :

- wget
- html-xml-utils

Il doit être exécuté depuis le sous-répertoire `base-associations`

Comptez 3 Go de données à télécharger, le temps dépendant de votre connexion
Internet.

[Exemple de sortie](base-associations/help/telecharge-donnees-ouvertes.output)

Notes :

- Le script utilise `wget` pour télécharger, il est donc possible de configurer
  les variables d’environnement `http_proxy` et `https_proxy` si vous êtes
  derrière un proxy.
- Le script est conçu pour être lancé à intervalle régulier, les fichiers déjà
  téléchargé ne seront donc pas téléchargés à nouveau.
- Pour forcer le téléchargement de tous les fichiers, il faut vider le
  sous-répertoire `base-associations/source`.

### Génération de la base de données

La base de données finale fait environ 5 Go.

Il est possible de réduire la taille de la base de données en demandant à
Sqlite3 d’optimiser l’arbre de recherche des tables full text et de de supprimer
les espaces vides du fichier. Ces opérations prennent néanmoins beaucoup de
temps.

La génération se fait grâce au script `creation-base-associations.bash` situé
dans le sous-répertoire `base-associations`.

Pré-requis :

- Python 3
- zcat
- unzip
- sed
- [csvcut, utilitaire maison à télécharger](https://github.com/zigazou/csvcut)
- tidy
- 7zr (paquet p7zip)
- SQLite3
- iconv
- XSLTproc
- [XSLSproc, utilitaire maison à télécharger](https://github.com/zigazou/xslclearer)


Il doit être exécuté depuis le sous-répertoire `base-associations`

Comptez 15 minutes pour une machine équipée d’un i7-3770 et d’un disque SSD.

[Exemple de sortie](base-associations/help/creation-base-associations.output)

Le script génère un fichiers `associations.db` qui doit ensuite être placé à la racine du projet.

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
