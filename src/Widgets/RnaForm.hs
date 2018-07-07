{- |
Module      :  RnaForm
Description :  Generates and handles RNA form.
Copyright   :  (c) Frédéric BISSON
License     :  GPL-2
Maintainer  :  zigazou@free.fr

Generates and handles RNA form.
-}
module Widgets.RnaForm
    ( RnaForm(..)
    , rnaForm
    ) where

import           Import

-- | A structure holding the results of an RNA form.
data RnaForm = RnaForm Text

-- | Settings for the RNA field.
rnaSettings :: FieldSettings master
rnaSettings = FieldSettings
    { fsLabel   = "Saisir le numéro RNA de l’association"
    , fsTooltip = Nothing
    , fsId      = Just "rna-asso"
    , fsName    = Just "rna-asso"
    , fsAttrs   = [ ("class", "form-control")
                  , ("placeholder", "Numéro RNA de l’association")
                  ]
    }

-- | The `rnaForm` function generates and handles an RNA form.
rnaForm :: Html -- ^ Html to be placed before our generated html
        -> MForm Handler (FormResult RnaForm, Widget)
rnaForm extra = do
    (rnaRes, rnaView) <- mreq textField rnaSettings Nothing

    let rna = RnaForm <$> rnaRes
    let widget = [whamlet|
        #{extra}
        <div .input-group>
            ^{fvInput rnaView}
            <span .input-group-btn>
                <button .btn.btn-default type="submit">
                    <i .glyphicon.glyphicon-search>
        |]

    return (rna, widget)
