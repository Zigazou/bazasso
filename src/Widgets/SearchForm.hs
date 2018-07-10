{- |
Module      :  SearchForm
Description :  Generates and handles search form using a keyword and a theme.
Copyright   :  (c) Frédéric BISSON
License     :  GPL-2
Maintainer  :  zigazou@free.fr

Generates and handles a search form requiring a keyword and an optional theme
to be selected.
-}
module Widgets.SearchForm
    ( SearchForm(searchString, searchTheme, searchDept, searchFields)
    , searchForm
    , SearchFields(TitleOnly, AllFields)
    , SearchDept(AllDepartment, OneDepartment)
    ) where

import           Import

import           Helpers.GeneralTheme (GeneralTheme (AllGeneralTheme),
                                       gtOptionList)

-- | Fields on which search is done
data SearchFields = TitleOnly
                  | AllFields
                  deriving (Eq, Show)

-- | Departement selection
data SearchDept = AllDepartment
                | OneDepartment Text Departement
                deriving (Eq, Show)

-- | A structure holding the results of a search form.
data SearchForm = SearchForm
    { searchString :: Text         -- ^ Keyword to search for
    , searchTheme  :: GeneralTheme -- ^ optional theme
    , searchDept   :: SearchDept   -- ^ optional department
    , searchFields :: SearchFields -- ^ Fields on which search is done
    }

-- | Settings for the keyword field.
textSettings :: FieldSettings master
textSettings = FieldSettings
    { fsLabel   = "Rechercher dans les titres des associations"
    , fsTooltip = Nothing
    , fsId      = Just "search-asso"
    , fsName    = Just "search-asso"
    , fsAttrs   = [ ("class", "form-control")
                  , ("placeholder", "Mots se trouvant dans le titre")
                  ]
    }

-- | Settings for the theme field.
selectSettings :: FieldSettings master
selectSettings = FieldSettings
    { fsLabel   = "Sélectionner le thème"
    , fsTooltip = Nothing
    , fsId      = Just "search-theme"
    , fsName    = Just "search-theme"
    , fsAttrs   = [ ("class", "large-select") ]
    }

-- | Settings for the department field.
deptSettings :: FieldSettings master
deptSettings = FieldSettings
    { fsLabel   = "Sélectionner le département"
    , fsTooltip = Nothing
    , fsId      = Just "search-dept"
    , fsName    = Just "search-dept"
    , fsAttrs   = [ ("class", "large-select") ]
    }

-- | Settings for the department field.
fieldsSettings :: FieldSettings master
fieldsSettings = FieldSettings
    { fsLabel   = "Sélectionner les champs"
    , fsTooltip = Nothing
    , fsId      = Just "search-fields"
    , fsName    = Just "search-fields"
    , fsAttrs   = [ ("class", "large-select") ]
    }

fieldsTypeList :: [ (Text, SearchFields) ]
fieldsTypeList = [ ("Dans le titre long uniquement", TitleOnly)
                 , ("Dans le titre long et l’objet", AllFields)
                 ]

getDepartmentList :: HandlerFor App [(Text, SearchDept)]
getDepartmentList = do
    departments <- runDB (selectList [] [])
    return $ ("Dans tous les départements" :: Text, AllDepartment)
           : (deptToKeyValue <$> departments)
    where deptToKeyValue (Entity deptId dept) =
            ( departementLibelle dept
            , OneDepartment (unDepartementKey deptId) dept
            )

-- | The `searchForm` function generates and handles a search form based on a
--   keyword and an optional theme.
searchForm :: Html -- ^ Html to be placed before our generated html
           -> MForm Handler (FormResult SearchForm, Widget)
searchForm extra = do
    -- Load department list from the database
    deptList <- lift getDepartmentList

    -- Handles form fields
    (searchRes, searchView) <- mreq textField textSettings Nothing
    (themeRes, themeView) <- mreq (selectFieldList gtOptionList)
                                  selectSettings (Just AllGeneralTheme)
    (deptRes, deptView) <- mreq (selectFieldList deptList)
                                deptSettings (Just AllDepartment)
    (fieldsRes, fieldsView) <- mreq (selectFieldList fieldsTypeList)
                                    fieldsSettings (Just TitleOnly)

    let search = SearchForm <$> searchRes <*> themeRes <*> deptRes <*> fieldsRes
    let widget = [whamlet|
        #{extra}
        <div .input-group>
            ^{fvInput searchView}
            <span .input-group-btn>
                <button .btn.btn-default type="submit">
                    <i .glyphicon.glyphicon-search>
        <div>
            ^{fvInput themeView}
        <div>
            ^{fvInput deptView}
        <div>
            ^{fvInput fieldsView}
        |]

    return (search, widget)
