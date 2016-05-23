
-- | Some helper functions used internally by multiple modules.
-- | Not part of the official API, thus subject to change without affecting semver.

module Elm.Graphics.Internal
    ( createNode
    , removePaddingAndMargin
    , setStyle, removeStyle
    , addTransform, removeTransform
    , getDimensions, measure
    ) where


import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.HTML.Types (htmlDocumentToDocument, htmlElementToNode)
import DOM.HTML.Document (body)
import DOM.Node.Document (createElement)
import DOM.Node.Types (Element, Node, elementToNode)
import DOM.Node.Node (appendChild, removeChild, nextSibling, insertBefore, parentNode)
import Data.Nullable (toMaybe)
import Data.Maybe (Maybe(..))
import Data.List (List(..), (:))
import Data.Foldable (for_)
import Control.Monad.Eff (Eff)
import Control.Bind ((>=>))
import Prelude (bind, (>>=), (>>>), pure)


-- Sets the style named in the first param to the value of the second param
foreign import setStyle :: ∀ e. String -> String -> Element -> Eff (dom :: DOM | e) Element


-- Removes the style
foreign import removeStyle :: ∀ e. String -> Element -> Eff (dom :: DOM | e) Element


-- Dimensions
foreign import getDimensions :: ∀ e. Element -> Eff (dom :: DOM | e) {width :: Number, height :: Number}


createNode :: ∀ e. String -> Eff (dom :: DOM | e) Element
createNode elementType = do
    node <-
        window >>=
        document >>=
        htmlDocumentToDocument >>>
        createElement elementType

    removePaddingAndMargin node
    pure node


removePaddingAndMargin :: ∀ e. Element -> Eff (dom :: DOM | e) Element
removePaddingAndMargin =
    setStyle "padding" "0px" >=>
    setStyle "margin" "0px"


vendorTransforms :: List String
vendorTransforms =
    ( "transform"
    : "msTransform"
    : "MozTransform"
    : "webkitTransform"
    : "OTransform"
    : Nil
    )


addTransform :: ∀ e. String -> Element -> Eff (dom :: DOM | e) Element
addTransform transform node = do
    for_ vendorTransforms \t ->
        setStyle t transform node

    pure node


removeTransform :: ∀ e. Element -> Eff (dom :: DOM | e) Element
removeTransform node = do
    for_ vendorTransforms \t ->
        removeStyle t node

    pure node


-- Note that if the node is already in a document, you can just run getDimensions.
-- This is effectful, in the sense that the node will be removed from any parent
-- it currently has.
measure :: ∀ e. Node -> Eff (dom :: DOM | e) {width :: Number, height :: Number}
measure node = do
    doc <-
        window >>= document

    nullableBody <-
        body doc

    case toMaybe nullableBody of
        Just b -> do
            temp <-
                createElement "div" (htmlDocumentToDocument doc)

            setStyle "visibility" "hidden" temp
            setStyle "float" "left" temp

            oldSibling <- nextSibling node
            oldParent <- parentNode node

            appendChild node (elementToNode temp)

            let bodyDoc = htmlElementToNode b
            appendChild (elementToNode temp) bodyDoc

            dim <- getDimensions temp

            removeChild (elementToNode temp) bodyDoc

            -- Now, we should put it back ...
            case toMaybe oldParent of
                Just p ->
                    case toMaybe oldSibling of
                        Just s ->
                            insertBefore node s p

                        Nothing ->
                            appendChild node p

                Nothing ->
                    removeChild node (elementToNode temp)

            pure dim

        Nothing ->
            pure
                { width: 0.0
                , height: 0.0
                }
