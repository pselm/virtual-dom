
-- | > API to the core diffing algorithm. Can serve as a foundation for libraries
-- | > that expose more helper functions for HTML or SVG.

module Elm.VirtualDom
    ( module Virtual
    , Node
    , text, node
    , Property, property, attribute, attributeNS, mapProperty
    , style
    , on, onWithOptions, Options, defaultOptions, equalOptions
    , lazy, lazy_, lazy2, lazy2_, lazy3, lazy3_
    , keyedNode
    , program, programWithFlags
    , RenderedNode, render, update
    , RenderAt, replaceNode, appendChildTo
    ) where


import Control.Alt ((<|>))
import Control.Apply (lift2)
import Control.Comonad (extract)
import Control.Monad (unless, when)
import Control.Monad.Aff (forkAff)
import Control.Monad.Aff.AVar (AVar, makeEmptyVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef, writeRef)
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (runExceptT, throwError)
import Control.Monad.IO (IO, runIO')
import Control.Monad.IO.Effect (INFINITY)
import Control.Monad.IOSync (IOSync, runIOSync')
import Control.Monad.ST (pureST, newSTRef, writeSTRef, readSTRef)
import DOM (DOM)
import DOM.Event.Event (preventDefault, stopPropagation)
import DOM.Event.EventTarget (EventListener, addEventListener, eventListener)
import DOM.Event.Types (Event, EventType(EventType))
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.Node.Document (createTextNode, createElement, createElementNS)
import DOM.Node.Element (setAttribute)
import DOM.Node.Node (appendChild, nodeType, ownerDocument, parentNode, replaceChild, setTextContent)
import DOM.Node.NodeType (NodeType(..))
import DOM.Node.Types (Document, Element, elementToEventTarget, elementToNode, textToNode)
import DOM.Node.Types (Node) as DOM
import Data.Bifunctor (lmap)
import Data.Coyoneda (Coyoneda, coyoneda, unCoyoneda)
import Data.Either (Either(Right, Left), either)
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (class Foldable, for_, traverse_)
import Data.FoldableWithIndex (forWithIndex_, traverseWithIndex_)
import Data.Foreign (Foreign, ForeignError(..), readString, toForeign)
import Data.Lazy (Lazy, defer, force)
import Data.Leibniz (type (~), Leibniz(..))
import Data.List (List, fromFoldable)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe, maybe)
import Data.Newtype (wrap)
import Data.Nullable (Nullable)
import Data.StrMap (StrMap, foldM)
import Data.StrMap.ST (new, poke, peek)
import Data.StrMap.ST.Unsafe (unsafeFreeze)
import Data.Traversable (for)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(Tuple), fst)
import Data.Tuple.Nested ((/\), type (/\))
import Elm.Basics (Bool)
import Elm.Json.Decode (Decoder, Value, decodeValue)
import Elm.Platform (Cmd, Program, Sub)
import Elm.Result (Result(..))
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Prelude (class Eq, class Functor, Unit, bind, const, discard, eq, flip, id, map, pure, unit, void, (#), ($), (&&), (<#>), (<$>), (<<<), (<>), (==), (>>=), (||))
import Prelude (map) as Virtual
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (reallyUnsafeRefEq, unsafeRefEq)


-- Will suggest these for Data.Exists if they work
foreign import data Exists2 :: (Type -> Type -> Type) -> Type

mkExists2 :: ∀ f a b. f a b -> Exists2 f
mkExists2 = unsafeCoerce

runExists2 :: ∀ f r. (∀ a b. f a b -> r) -> Exists2 f -> r
runExists2 = unsafeCoerce


foreign import data Exists3 :: (Type -> Type -> Type -> Type) -> Type

mkExists3 :: ∀ f a b c. f a b c -> Exists3 f
mkExists3 = unsafeCoerce

runExists3 :: ∀ f r. (∀ a b c. f a b c -> r) -> Exists3 f -> r
runExists3 = unsafeCoerce


-- | > An immutable chunk of data representing a DOM node. This can be HTML or SVG.
data Node msg
    = KeyedNode (NodeRecord msg) (List (String /\ Node msg))
    | PlainNode (NodeRecord msg) (List (Node msg))
    | Tagger (Coyoneda Node msg)
    | Text String
    | Thunk (Lazy (Node msg)) (Thunk msg)
    | Thunk2 (Lazy (Node msg)) (Thunk2 msg)
    | Thunk3 (Lazy (Node msg)) (Thunk3 msg)


instance functorNode :: Functor Node where
    map func child = Tagger $ coyoneda func child


type NodeRecord msg =
    { tag :: String
    , namespace :: Maybe String
    , facts :: OrganizedFacts msg
    }


data EventNode msg parent
    = SubEventNode (msg -> parent) (EventNodeRef parent)
    | RootEventNode (msg -> IOSync Unit)


type EventNodeRef msg =
    Ref (Exists (EventNode msg))


runEventNode :: ∀ msg. msg -> EventNodeRef msg -> IOSync Unit
runEventNode msg eventNodeRef =
    liftEff (readRef eventNodeRef) >>=
        runExists
            case _ of
                RootEventNode func ->
                    func msg

                SubEventNode func parent ->
                    runEventNode (func msg) parent


-- Hmm. We're forgetting the parent type, of course, so this is a bit tricky.
-- Another way in which future iterations on the whole scheme will be valuable!
parentEventNode :: ∀ msg parentMsg. EventNodeRef msg -> IOSync (EventNodeRef parentMsg)
parentEventNode eventNodeRef =
    liftEff (readRef eventNodeRef) >>=
        runExists
            case _ of
                RootEventNode func ->
                    pure $ unsafeCoerce eventNodeRef

                SubEventNode func parent ->
                    pure $ unsafeCoerce parent


-- | A type which represents the result of rendering a virtual node into a DOM
-- | node. You originally produce this type by calling `render`. Then, you can
-- | update it by calling `update` with a new virtual node.
-- |
-- | This is not part of the Elm API, and you do not need to use it directly
-- | when setting up an ordinary Elm `program` -- it is handled for you in that
-- | case.
newtype RenderedNode msg =
    RenderedNode (Ref (RenderedNodeF msg))


-- This is wrapped in a `Ref` by `RenderedNode` because (a) producing one of
-- these is necessarily effectful anyway, since we're including references to
-- the DOM nodes that we create in doing so; and (b) using this to update the
-- DOM with a new virtual node is necessarily effectful with respect to the
-- data structure itself, since the updated DOM will no longer be in sync with
-- the **old** `RenderedNode` structures. So, we need the ability to actually
-- **mutate** references to a rendered node which are held externally. And,
-- that's what wrapping this in a `Ref` gives us.
--
-- It also makes the `update` process more efficient, since we don't have to
-- rebuild the structure as we go in order to make changes lower down ... the
-- children can actually be mutated without touching the parents, in cases
-- where the only change needed is deep in the structure.
data RenderedNodeF msg
    = RKeyedNode (NodeRecord msg) (RenderedNodeRecord msg) (List (String /\ RenderedNode msg))
    | RPlainNode (NodeRecord msg) (RenderedNodeRecord msg) (List (RenderedNode msg))
    | RTagger (Coyoneda RenderedNode msg) (EventNodeRef msg)
    | RText String DOM.Node (EventNodeRef msg)
    | RThunk (RenderedNode msg) (Thunk msg)
    | RThunk2 (RenderedNode msg) (Thunk2 msg)
    | RThunk3 (RenderedNode msg) (Thunk3 msg)


-- So, if we encounter a rendered node as we diff and we need to redraw, where
-- should we attach the results? That is, what existing node should we replace
-- when we redraw? For some kinds of rendered node, this is trivial, but for
-- others it requires a bit of computation.
redrawAt :: ∀ msg. RenderedNode msg -> IOSync (DOM.Node /\ EventNodeRef msg)
redrawAt (RenderedNode rNode) =
    (liftEff $ readRef rNode) >>=
        case _ of
            RKeyedNode _ rendered _ ->
                pure $ elementToNode rendered.element /\ rendered.eventNode

            RPlainNode _ rendered _ ->
                pure $ elementToNode rendered.element /\ rendered.eventNode

            RTagger coyo eventNode ->
                coyo # unCoyoneda \_ subNode -> do
                    loc <-
                        fst <$> redrawAt subNode

                    -- If we're redrawing where a tagger was, we don't want the
                    -- eventNode created for the tagger ... we want its parent
                    -- instead.
                    parent <-
                        parentEventNode eventNode

                    pure $ loc /\ parent

            RText _ domNode eventNode ->
                pure $ domNode /\ eventNode

            RThunk subNode _ ->
                redrawAt subNode

            RThunk2 subNode _ ->
                redrawAt subNode

            RThunk3 subNode _ ->
                redrawAt subNode


-- For PlainNode and KeyedNode, this is the extra information we have once
-- we've rendered the node.
type RenderedNodeRecord msg =
    { handlers :: StrMap (Listener /\ Ref (HandlerInfo msg))
    , element :: Element
    , eventNode :: EventNodeRef msg
    }


-- The type of our listeners.
type Listener =
    EventListener
        ( infinity :: INFINITY
        , dom :: DOM
        , ref :: REF
        )


type Thunk msg = Exists (ThunkRecord1 msg)
type Thunk2 msg = Exists2 (ThunkRecord2 msg)
type Thunk3 msg = Exists3 (ThunkRecord3 msg)


newtype ThunkRecord1 msg a = ThunkRecord1
    { func :: a -> Node msg
    , arg :: a /\ Maybe (a -> a -> Bool)
    }


newtype ThunkRecord2 msg a b = ThunkRecord2
    { func :: a -> b -> Node msg
    , arg1 :: a /\ Maybe (a -> a -> Bool)
    , arg2 :: b /\ Maybe (b -> b -> Bool)
    }


newtype ThunkRecord3 msg a b c = ThunkRecord3
    { func :: a -> b -> c -> Node msg
    , arg1 :: a /\ Maybe (a -> a -> Bool)
    , arg2 :: b /\ Maybe (b -> b -> Bool)
    , arg3 :: c /\ Maybe (c -> c -> Bool)
    }


-- | Not an `Eq` instance, because it's not deciable ... will be some false
-- | negatives.
equalThunks :: ∀ a b. Thunk a -> Thunk b -> Bool
equalThunks left right =
    left # runExists (\(ThunkRecord1 a) ->
    right # runExists (\(ThunkRecord1 b) ->
        -- The two functions need to be the same, and the best we can do
        -- is reference equality. That also gives us our warrant for
        -- believing that the arguments have matching types.
        reallyUnsafeRefEq a.func b.func &&
        equalArgs a.arg (unsafeCoerce b.arg)
    ))


-- | Not an `Eq` instance, because it's not deciable ... will be some false
-- | negatives.
equalThunks2 :: ∀ a b. Thunk2 a -> Thunk2 b -> Bool
equalThunks2 left right =
    left # runExists2 (\(ThunkRecord2 a) ->
    right # runExists2 (\(ThunkRecord2 b) ->
        -- The two functions need to be the same, and the best we can do
        -- is reference equality. That also gives us our warrant for
        -- believing that the arguments have matching types.
        reallyUnsafeRefEq a.func b.func &&
        equalArgs a.arg1 (unsafeCoerce b.arg1) &&
        equalArgs a.arg2 (unsafeCoerce b.arg2)
    ))


-- | Not an `Eq` instance, because it's not deciable ... will be some false
-- | negatives.
equalThunks3 :: ∀ a b. Thunk3 a -> Thunk3 b -> Bool
equalThunks3 left right =
    left # runExists3 (\(ThunkRecord3 a) ->
    right # runExists3 (\(ThunkRecord3 b) ->
        -- The two functions need to be the same, and the best we can do
        -- is reference equality. That also gives us our warrant for
        -- believing that the arguments have matching types.
        reallyUnsafeRefEq a.func b.func &&
        equalArgs a.arg1 (unsafeCoerce b.arg1) &&
        equalArgs a.arg2 (unsafeCoerce b.arg2) &&
        equalArgs a.arg3 (unsafeCoerce b.arg3)
    ))


-- | Checks possibly equality where we may have captured an `Eq` instance or
-- | may not. Assumes that we've already coerced the types if we have
-- | sufficient evidence to have done that.
equalArgs :: ∀ a. Tuple a (Maybe (a -> a -> Bool)) -> Tuple a (Maybe (a -> a -> Bool)) -> Bool
equalArgs (Tuple left leftEq) (Tuple right rightEq) =
    unsafeRefEq left right ||
    fromMaybe (\a b -> false) (leftEq <|> rightEq) left right


-- | > Create a DOM node with a tag name, a list of HTML properties that can
-- | > include styles and event listeners, a list of CSS properties like `color`, and
-- | > a list of child nodes.
-- | >
-- | >     import Json.Encode as Json
-- | >
-- | >     hello :: Node msg
-- | >     hello =
-- | >       node "div" [] [ text "Hello!" ]
-- | >
-- | >     greeting :: Node msg
-- | >     greeting =
-- | >       node "div"
-- | >         [ property "id" (Json.string "greeting") ]
-- | >         [ text "Hello!" ]
node :: ∀ f g msg. Foldable f => Foldable g => String -> f (Property msg) -> g (Node msg) -> Node msg
node tag properties children =
    PlainNode
        { tag
        , namespace: organized.namespace
        , facts: organized.facts
        }
        -- TODO: It would be nice to modify `diffChildren` so we could keep the
        -- supplied foldable rather than turning it into a list!
        (fromFoldable children)

    where
        organized =
            organizeFacts properties


-- | > Works just like `node`, but you add a unique identifier to each child
-- | > node. You want this when you have a list of nodes that is changing: adding
-- | > nodes, removing nodes, etc. In these cases, the unique identifiers help make
-- | > the DOM modifications more efficient.
keyedNode :: ∀ f g msg. Foldable f => Foldable g => String -> f (Property msg) -> g (Tuple String (Node msg)) -> Node msg
keyedNode tag properties children =
    KeyedNode
        { tag
        , namespace: organized.namespace
        , facts: organized.facts
        }
        (fromFoldable children)

    where
        organized =
            organizeFacts properties


-- | > Just put plain text in the DOM. It will escape the string so that it appears
-- | > exactly as you specify.
-- | >
-- | >     text "Hello World!"
text :: ∀ msg. String -> Node msg
text = Text


-- PROPERTIES

-- | > When using HTML and JS, there are two ways to specify parts of a DOM node.
-- | >
-- | >   1. Attributes &mdash; You can set things in HTML itself. So the `class`
-- | >      in `<div class="greeting"></div>` is called an *attribute*.
-- | >
-- | >   2. Properties &mdash; You can also set things in JS. So the `className`
-- | >      in `div.className = 'greeting'` is called a *property*.
-- | >
-- | > So the `class` attribute corresponds to the `className` property. At first
-- | > glance, perhaps this distinction is defensible, but it gets much crazier.
-- | > *There is not always a one-to-one mapping between attributes and properties!*
-- | > Yes, that is a true fact. Sometimes an attribute exists, but there is no
-- | > corresponding property. Sometimes changing an attribute does not change the
-- | > underlying property. For example, as of this writing, the `webkit-playsinline`
-- | > attribute can be used in HTML, but there is no corresponding property!
--
-- The `Styles` constrcutor uses continuation passing so that we can capture
-- the `Foldable` instance.
data Property msg
    = CustomProperty Key Value
    | Attribute Key String
    | AttributeNS Namespace Key String
    | Styles (∀ r. (∀ f. Foldable f => f (Tuple String String) -> r) -> r)
    | OnEvent Key Options (Decoder msg)


instance functorProperty :: Functor Property where
    map func prop =
        case prop of
            CustomProperty key value ->
                CustomProperty key value

            Attribute key value ->
                Attribute key value

            AttributeNS ns key value ->
                AttributeNS ns key value

            Styles cc ->
                -- It looks like you need to call back out to the external
                -- function to "re-capture" the Foldable instance. Both
                -- `derive instance functor ...` and `Styles cc` result
                -- in compiler errors.
                cc style

            OnEvent key options decoder ->
                OnEvent key options (map func decoder)


-- | > Transform the messages produced by a `Property`.
-- |
-- | Equivalent to Purescript's `map`
mapProperty :: ∀ a b. (a -> b) -> Property a -> Property b
mapProperty = map


-- This represents the properties that should be applied to a node, organized for
-- quick lookup (since the API supplies them as a list).
type OrganizedFacts msg =
    { attributes :: StrMap String
    , attributesNS :: StrMap (StrMap String)
    , events :: StrMap (Tuple Options (Decoder msg))
    , styles :: StrMap String
    , properties :: StrMap Value
    }


type Namespace = String
type Key = String


-- Basically, this seems to take a list of properties and group them according
-- to their subtypes. Also, if properties of the same subtype have the same
-- key, it would only retain one of the properties.
--
-- One alternative would be a foldl, with the OrganizedFacts as the memo. Or,
-- possibly some kind of Writer monad?
organizeFacts :: ∀ f msg. Foldable f => f (Property msg) -> {namespace :: Maybe String, facts :: OrganizedFacts msg}
organizeFacts factList =
    pureST do
        -- Create a bunch of accumulators for StrMap
        mutableAttributes <- new
        mutableAttributesNS <- new
        mutableEvents <- new
        mutableStyles <- new
        mutableProperties <- new

        -- And a reference for the namespace
        mutableNamespace <- newSTRef Nothing

        -- Iterate through the facts
        for_ factList \fact -> do
            case fact of
                Attribute key value ->
                    void $ poke mutableAttributes key value

                -- We make mutableAttributesNS a map of maps, where the outer map
                -- is keyed by the namespace. This fixes a bug in the original
                -- Elm Javascript ... see https://github.com/elm-lang/virtual-dom/issues/16
                AttributeNS ns key value -> void do
                    submap <-
                        peek mutableAttributesNS ns >>=
                            case _ of
                                Just existing ->
                                    pure existing

                                Nothing ->
                                    new

                    void $ poke submap key value
                    void $ poke mutableAttributesNS ns submap

                OnEvent key options decoder ->
                    void $ poke mutableEvents key (Tuple options decoder)

                Styles cc ->
                    cc \foldable ->
                        for_ foldable \(Tuple key value) ->
                            void $ poke mutableStyles key value

                CustomProperty key value ->
                    -- So, the normal case here is that we're setting an arbitrary property
                    -- on the node. However, the original Elm code also allows you to use
                    -- a special "namespace" property, to specify the namespace of the node
                    -- itself. This seems a bit odd ... would probably be better to have an
                    -- explicit API for namespaced nodes.
                    if key == "namespace" then
                        case extract $ runExceptT $ readString value of
                            Left _ ->
                                -- It wasn't a string, so don't handle specially
                                void $ poke mutableProperties key value

                            Right s ->
                                -- It was a string, so store as the namespace
                                void $ writeSTRef mutableNamespace (Just s)

                    else if key == "className" then do
                        -- The Elm code also special-cases `className` so that
                        -- if you supply it multiple times, it will be
                        -- additive, rather than clobbering the previous one.
                        oldString <-
                            peek mutableProperties key
                            <#> maybe (throwError $ pure $ ErrorAtProperty "className" $ JSONError "missing") readString

                        let newString = readString value
                        let combined = lift2 (\a b -> toForeign $ a <> " " <> b) oldString newString

                        -- If we've had any kind of error above, we just use the raw value
                        void $ poke mutableProperties key $ either (const value) id $ runExcept combined

                    else
                        -- This is the general case.
                        void $ poke mutableProperties key value

        -- These are unsafe in the sense that further modifications to the mutable
        -- versions would also modify the pure. So, we won't do that ...
        -- The alternative is freezeST, but that actually does a copy, which in
        -- this context isn't really necessary.
        attributes <- unsafeFreeze mutableAttributes
        events <- unsafeFreeze mutableEvents
        styles <- unsafeFreeze mutableStyles
        properties <- unsafeFreeze mutableProperties

        -- I also need to iterate over all of the submaps and "freeze" them ...
        -- and then freeze the resulting outer map
        pureOuterMap <- unsafeFreeze mutableAttributesNS
        accumulator <- new

        void $ foldM (\accum key submap ->
            unsafeFreeze submap >>= poke accum key
        ) accumulator pureOuterMap

        attributesNS <- unsafeFreeze accumulator

        -- And read the namespace
        namespace <- readSTRef mutableNamespace

        pure
            { namespace
            , facts: { attributes, attributesNS, events, styles, properties }
            }


-- | > Create arbitrary *properties*.
-- | >
-- | >     import JavaScript.Encode as Json
-- | >
-- | >     greeting : Html
-- | >     greeting =
-- | >         node "div" [ property "className" (Json.string "greeting") ] [
-- | >           text "Hello!"
-- | >         ]
-- | >
-- | > Notice that you must give the *property* name, so we use `className` as it
-- | > would be in JavaScript, not `class` as it would appear in HTML.
property :: ∀ msg. String -> Value -> Property msg
property = CustomProperty


-- | > Create arbitrary HTML *attributes*. Maps onto JavaScript’s `setAttribute`
-- | > function under the hood.
-- | >
-- | >     greeting : Html
-- | >     greeting =
-- | >         node "div" [ attribute "class" "greeting" ] [
-- | >           text "Hello!"
-- | >         ]
-- | >
-- | > Notice that you must give the *attribute* name, so we use `class` as it would
-- | > be in HTML, not `className` as it would appear in JS.
attribute :: ∀ msg. String -> String -> Property msg
attribute = Attribute


-- | > Would you believe that there is another way to do this?! This corresponds
-- | > to JavaScript's `setAttributeNS` function under the hood. It is doing pretty
-- | > much the same thing as `attribute` but you are able to have "namespaced"
-- | > attributes. This is used in some SVG stuff at least.
-- |
-- | Note that the first argument is the namespace, the second the label, and
-- | third the value.
attributeNS :: ∀ msg. String -> String -> String -> Property msg
attributeNS = AttributeNS


-- | > Specify a list of styles.
-- | >
-- | >     myStyle :: Property msg
-- | >     myStyle =
-- | >       style
-- | >         [ Tuple "backgroundColor" "red"
-- | >         , Tuple "height" "90px"
-- | >         , Tuple "width" "100%"
-- | >         ]
-- | >
-- | >     greeting :: Node msg
-- | >     greeting =
-- | >       node "div" [ myStyle ] [ text "Hello!" ]
style :: ∀ f msg. Foldable f => f (Tuple String String) -> Property msg
style styles =
    Styles \func -> func styles


-- EVENTS

-- | > Create a custom event listener.
-- | >
-- | >     import Json.Decode as Json
-- | >
-- | >     onClick : msg -> Property msg
-- | >     onClick msg =
-- | >       on "click" (Json.succeed msg)
-- | >
-- | > You first specify the name of the event in the same format as with JavaScript’s
-- | > `addEventListener`. Next you give a JSON decoder, which lets you pull
-- | > information out of the event object. If the decoder succeeds, it will produce
-- | > a message and route it to your `update` function.
on :: ∀ msg. String -> Decoder msg -> Property msg
on = flip onWithOptions defaultOptions


-- | > Same as `on` but you can set a few options.
onWithOptions :: ∀ msg. String -> Options -> Decoder msg -> Property msg
onWithOptions = OnEvent


-- | > Options for an event listener. If `stopPropagation` is true, it means the
-- | > event stops traveling through the DOM so it will not trigger any other event
-- | > listeners. If `preventDefault` is true, any built-in browser behavior related
-- | > to the event is prevented. For example, this is used with touch events when you
-- | > want to treat them as gestures of your own, not as scrolls.
type Options =
    { stopPropagation :: Bool
    , preventDefault :: Bool
    }


-- | > Everything is `False` by default.
-- | >
-- | >     defaultOptions =
-- | >         { stopPropagation = False
-- | >         , preventDefault = False
-- | >         }
defaultOptions :: Options
defaultOptions =
    { stopPropagation: false
    , preventDefault: false
    }


-- | Elm doesn't need this function because its `==` can handle record types
-- | magically. This isn't possible in Purescript without a newtype, which we'd
-- | like to avoid here. So, we define a custom equality function.
equalOptions :: Options -> Options -> Bool
equalOptions a b =
    a.stopPropagation == b.stopPropagation &&
    a.preventDefault == b.preventDefault


-- OPTIMIZATION

-- | > A performance optimization that delays the building of virtual DOM nodes.
-- | >
-- | > Calling `(view model)` will definitely build some virtual DOM, perhaps a lot of
-- | > it. Calling `(lazy view model)` delays the call until later. During diffing, we
-- | > can check to see if `model` is referentially equal to the previous value used,
-- | > and if so, we just stop. No need to build up the tree structure and diff it,
-- | > we know if the input to `view` is the same, the output must be the same!
-- |
-- | The diffing process will operate somewhat more efficiently if the function you
-- | provide has a stable reference (that we can check for reference equality the
-- | next time we see it).
-- |
-- | For a version of this function that doesn't require an `Eq` instance, see
-- | `lazy_`. This one will do a better job of detecting equality.
--
-- TODO: One idea I coul explore is an `Eq`-like instance that represents
-- something better than `unsafeRefEq` but not as reliable as `Eq` ... that is,
-- no false positives, but some false negatives. Perhaps `PartialEq`? Though
-- `Partial` doesn't quite have the right connotation.
lazy :: ∀ a msg. Eq a => (a -> Node msg) -> a -> Node msg
lazy func arg =
    Thunk (defer \_ -> func arg) $ mkExists $ ThunkRecord1
        { func
        , arg : arg /\ Just eq
        }


-- | Like `lazy`, but does not require an `Eq` instance. Using `lazy` will do
-- | a better job of detecting equality.
--
-- In Purescript 0.12, I should be able to pick up a possibly-existing `Eq`
-- instance with instance chains, without needing a separate function. (I could
-- do it now with overlapping instances, but may as well wait).
lazy_ :: ∀ a msg. (a -> Node msg) -> a -> Node msg
lazy_ func arg =
    Thunk (defer \_ -> func arg) $ mkExists $ ThunkRecord1
        { func
        , arg : arg /\ Nothing
        }


-- | > Same as `lazy` but checks on two arguments.
-- |
-- | The diffing process will operate somewhat more efficiently if the function you
-- | provide has a stable reference (that we can check for reference equality the
-- | next time we see it).
lazy2 :: ∀ a b msg. Eq a => Eq b => (a -> b -> Node msg) -> a -> b -> Node msg
lazy2 func arg1 arg2 =
    Thunk2 (defer \_ -> func arg1 arg2) $ mkExists2 $ ThunkRecord2
        { func
        , arg1 : arg1 /\ Just eq
        , arg2 : arg2 /\ Just eq
        }


-- | Like `lazy2`, but does not require an `Eq` instance. Using `lazy2` will do
-- | a better job of detecting equality.
lazy2_ :: ∀ a b msg. (a -> b -> Node msg) -> a -> b -> Node msg
lazy2_ func arg1 arg2 =
    Thunk2 (defer \_ -> func arg1 arg2) $ mkExists2 $ ThunkRecord2
        { func
        , arg1 : arg1 /\ Nothing
        , arg2 : arg2 /\ Nothing
        }


-- | > Same as `lazy` but checks on three arguments.
-- |
-- | The diffing process will operate somewhat more efficiently if the function you
-- | provide has a stable reference (that we can check for reference equality the
-- | next time we see it).
lazy3 :: ∀ a b c msg. Eq a => Eq b => Eq c => (a -> b -> c -> Node msg) -> a -> b -> c -> Node msg
lazy3 func arg1 arg2 arg3 =
    Thunk3 (defer \_ -> func arg1 arg2 arg3) $ mkExists3 $ ThunkRecord3
        { func
        , arg1 : arg1 /\ Just eq
        , arg2 : arg2 /\ Just eq
        , arg3 : arg3 /\ Just eq
        }


-- | Like `lazy3`, but does not require an `Eq` instance. Using `lazy3` will do
-- | a better job of detecting equality.
lazy3_ :: ∀ a b c msg. (a -> b -> c -> Node msg) -> a -> b -> c -> Node msg
lazy3_ func arg1 arg2 arg3 =
    Thunk3 (defer \_ -> func arg1 arg2 arg3) $ mkExists3 $ ThunkRecord3
        { func
        , arg1 : arg1 /\ Nothing
        , arg2 : arg2 /\ Nothing
        , arg3 : arg3 /\ Nothing
        }


-- RENDER

-- The liberal use of `Ref` here could possibly be reduced via some kind of
-- use of soemthing like a State monad. I suppose it would be `render` that
-- would create some kind of context in which `update` would run? In any event,
-- I'll explore that later.
--
-- The complication is that we're setting up things which the event listeners
-- will need to access, and which (for performance reasons) we'd like to modify
-- without modifying the event listeners. So, it may be that we can do no better
-- than `Ref` at least for parts of this.


-- | Indicates where a virtual Node should be rendered in the document.
data RenderAt
    = ReplaceNode DOM.Node
    | AppendChildTo DOM.Node


-- | Render a virtual node by replacing the indicated Node in the DOM.
replaceNode :: DOM.Node -> RenderAt
replaceNode = ReplaceNode


-- | Render a virtual node by appending a new child to the indicated node in
-- | the DOM..
appendChildTo :: DOM.Node -> RenderAt
appendChildTo = AppendChildTo


renderInDocument :: RenderAt -> IOSync Document
renderInDocument location =
    case location of
        ReplaceNode n ->
            documentForNode n

        AppendChildTo n ->
            documentForNode n


renderAt :: RenderAt -> DOM.Node -> IOSync Unit
renderAt location domNode = liftEff
    case location of
        AppendChildTo parent ->
            void $ appendChild domNode parent

        ReplaceNode targetNode ->
            -- Now, some nodes don't have parents ... and thus cannot be
            -- replaced ... but we won't call this for those!  Could
            -- reconsider the design, I suppose.
            parentNode targetNode >>=
                traverse_ (replaceChild domNode targetNode)


-- | Render the virtual node as a new DOM node, and attach it as the last child
-- | of the provided parent node.
-- |
-- | The first parameter is a function which will be used to handle messages
-- | that the node produces. (In an Elm `program`, they simply are fed into
-- | the program's `update` function).
-- |
-- | Note that this is not part of the Elm API. Normally, you won't need to
-- | call this directly -- it is used internally by `program` to setup your
-- | program in the Elm architecture.
-- |
-- | The `RenderedNode` you get back is a structure you can provide to `update`
-- | with a new virtual node to update the DOM to match the new virtual node.
render :: ∀ msg. (msg -> IOSync Unit) -> Node msg -> RenderAt -> IOSync (RenderedNode msg)
render cb vNode refNode = do
    eventNode <-
        liftEff $ newRef $ mkExists $ RootEventNode cb

    renderHelp eventNode vNode refNode


renderHelp :: ∀ msg. EventNodeRef msg -> Node msg -> RenderAt -> IOSync (RenderedNode msg)
renderHelp eventNode vNode refNode =
    case vNode of
        Thunk lazyNode thunk -> do
            rendered <- renderHelp eventNode (force lazyNode) refNode
            newRenderedNode $
                RThunk rendered thunk

        Thunk2 lazyNode thunk -> do
            rendered <- renderHelp eventNode (force lazyNode) refNode
            newRenderedNode $
                RThunk2 rendered thunk

        Thunk3 lazyNode thunk -> do
            rendered <- renderHelp eventNode (force lazyNode) refNode
            newRenderedNode $
                RThunk3 rendered thunk

        Text string -> do
            doc <- renderInDocument refNode
            domNode <- liftEff $ textToNode <$> createTextNode string doc
            renderAt refNode domNode
            newRenderedNode $
                RText string domNode eventNode

        PlainNode rec children -> do
            element <-
                renderInDocument refNode >>=
                    case rec.namespace of
                        Just ns ->
                            liftEff <<< createElementNS rec.namespace rec.tag

                        Nothing ->
                            liftEff <<< createElement rec.tag

            let domNode = elementToNode element

            renderedRec <-
                renderFacts eventNode rec.facts element

            renderedChildren <-
                for children \child ->
                    renderHelp eventNode child (appendChildTo domNode)

            renderAt refNode domNode

            newRenderedNode $
                RPlainNode rec renderedRec renderedChildren

        KeyedNode rec children -> do
            element <-
                renderInDocument refNode >>=
                    case rec.namespace of
                        Just ns ->
                            liftEff <<< createElementNS rec.namespace rec.tag

                        Nothing ->
                            liftEff <<< createElement rec.tag

            let domNode = elementToNode element

            renderedRec <-
                renderFacts eventNode rec.facts element

            renderedChildren <-
                for children \(key /\ child) ->
                    renderHelp eventNode child (appendChildTo domNode)
                        <#> \rendered -> (key /\ rendered)

            renderAt refNode domNode

            newRenderedNode $
                RKeyedNode rec renderedRec renderedChildren

        Tagger coyo ->
            coyo # unCoyoneda \func subNode -> do
                subEventNode <-
                    liftEff $ newRef $ mkExists $
                        SubEventNode func eventNode

                rendered <-
                    renderHelp subEventNode subNode refNode

                -- TODO: Review the unsafeCoerce ... could it be avoided?
                newRenderedNode $
                    RTagger (coyoneda func rendered) (unsafeCoerce subEventNode)


newRenderedNode :: ∀ msg. RenderedNodeF msg -> IOSync (RenderedNode msg)
newRenderedNode nodeF =
    liftEff $ RenderedNode <$> newRef nodeF


renderFacts :: ∀ msg. EventNodeRef msg -> OrganizedFacts msg -> Element -> IOSync (RenderedNodeRecord msg)
renderFacts eventNode facts element = do
    forWithIndex_ facts.attributes \key value ->
        liftEff $ setAttribute key value element

    forWithIndex_ facts.attributesNS \ns ->
        traverseWithIndex_ \key value ->
            setAttributeNS ns key value element

    forWithIndex_ facts.styles \key value ->
        setStyle key value element

    forWithIndex_ facts.properties \key value ->
        setProperty key value element

    handlers <-
        forWithIndex facts.events \key (options /\ decoder) -> liftEff do
            handlerInfoRef <- newRef { options, decoder }
            let listener = eventListener $ runIOSync' <<< handleEvent handlerInfoRef eventNode
            addEventListener (EventType key) listener false (elementToEventTarget element)
            pure (Tuple listener handlerInfoRef)

    pure
        { element
        , eventNode
        , handlers
        }

-- | Use a new virtual Node to update a previously rendered node. This is an
-- | effectful function in two senses: the DOM which the `RenderedNode` had
-- | previously produced will be updated, and the `RenderedNode` itself will
-- | be updated to reflect the new virtual node.
update :: ∀ msg. Node msg -> RenderedNode msg -> IOSync Unit
update vNode rNode =
    updateHelp (Just id) vNode rNode


-- We don't necessarily know that the `msg` type for the virtual node is the
-- saame as the `msg` type for the rendered node, so we model that with a
-- `Maybe Leibniz` type.
--
-- Note that this modifies the `RenderedNode` in-place (it is actually a
-- `Ref`).  This is basically to conform with what it's doing to the DOM that
-- the RenderedNode holds references to ... since modifies the one in place, it
-- logically needs to modify the other in-place, and be effectful in relation
-- to both.
updateHelp :: ∀ vMsg rMsg. Maybe (vMsg ~ rMsg) -> Node vMsg -> RenderedNode rMsg -> IOSync Unit
updateHelp proof vNode wrapped@(RenderedNode rNode) = do
    renderedNode <- liftEff $ readRef rNode

    case vNode, renderedNode of
        Text vText, RText rText domNode eventNode ->
            -- In this case, we might not even really need to keep the original
            -- text, sincw we could just ask the DOM for it. Presumably that
            -- would be slower, though?
            unless (vText == rText) $ liftEff do
                setTextContent vText domNode
                writeRef rNode $ RText vText domNode eventNode

        Thunk vLazy vThunk, RThunk subNode rThunk ->
            -- For thunks, there is nothing to do if we can conclude that the
            -- two thunks are equal. Otherwise, we force the lazy virtual node
            -- and continue updating.
            --
            -- TODO: Review the type implications of this, and the unsafeCoerce
            -- ... I'm not sure I have this right.
            unless (equalThunks vThunk rThunk) do
                updateHelp proof (force vLazy) subNode
                liftEff $ writeRef rNode $
                    RThunk subNode (unsafeCoerce vThunk)

        Thunk2 vLazy vThunk, RThunk2 subNode rThunk ->
            unless (equalThunks2 vThunk rThunk) do
                updateHelp proof (force vLazy) subNode
                liftEff $ writeRef rNode $
                    RThunk2 subNode (unsafeCoerce vThunk)

        Thunk3 vLazy vThunk, RThunk3 subNode rThunk ->
            unless (equalThunks3 vThunk rThunk) do
                updateHelp proof (force vLazy) subNode
                liftEff $ writeRef rNode $
                    RThunk3 subNode (unsafeCoerce vThunk)

        -- Elm will diff the children whether or not the taggers are equal, so
        -- long as there are the same number of nested taggers.  So, it diffs
        -- children even we can't know that they use the same `msg` type. The
        -- reason, essentially, is that we can't always know that two taggers
        -- are equal, even if they really are, so we want to minimize the cost
        -- of those false negatives.  It wouldn't be great to always have to
        -- redraw them, for instance.
        Tagger vCoyo, RTagger rCoyo eventNodeRef ->
            vCoyo # unCoyoneda \vFunc vSubnode ->
            rCoyo # unCoyoneda \rFunc rSubnode ->
                if reallyUnsafeRefEq vFunc rFunc then
                    -- They are the same function, so we don't need to adjust
                    -- the eventNode. We can just continue diffing. We also use
                    -- the equality of the functions as warrant for believing
                    -- that `vMsg ~ rMsg` for the subnodes ... I should
                    -- probably think that through more carefully at some
                    -- point.
                    updateHelp (Just $ Leibniz unsafeCoerce) vSubnode rSubnode
                else do
                    -- In this case, we can't be sure that the functions were
                    -- the same, but there is lots of scope for false
                    -- negatives, so we want to minimize the cost of that. So,
                    -- we update the eventNode in place, and proceed to update
                    -- the children.  We can't manufacture proof that `vMsg ~
                    -- rMsg` for the children, since the functions may be
                    -- coming from different types, for all we know.
                    unsafeUpdateEventNodeRef vFunc eventNodeRef
                    updateHelp Nothing vSubnode rSubnode

        KeyedNode vNodeRecord vChildren, RKeyedNode rNodeRecord rendered rChildren ->
            unsafeCrashWith "TODO"

        PlainNode vNodeRecord vChildren, RPlainNode rNodeRecord rendered rChildren ->
            unsafeCrashWith "TODO"

        _, _ -> do
            -- Where the constructors don't match, we just redraw.
            ( redraw /\ eventNode ) <-
                lmap replaceNode <$>
                    redrawAt wrapped

            (RenderedNode newlyRendered) <-
                renderHelp (unsafeCoerce eventNode) vNode redraw

            -- Now, we need to mutate our old rendered node with the new one.
            liftEff $ readRef (unsafeCoerce newlyRendered) >>= writeRef rNode


-- This is unsafe because we're updating the function even though we don't
-- actually have evidence about the types. The entire scheme is designed to
-- make the types work out in the end, but it would be difficult to convince
-- the compiler of that. So, this is something that could be reviewed at some
-- point to see whether a better design is possible (given the Elm API, and the
-- efficiencies we're trying to obtain in cases where we get false negatives
-- when comparing taggers for equality).
unsafeUpdateEventNodeRef :: ∀ msg a b. (a -> b) -> EventNodeRef msg -> IOSync Unit
unsafeUpdateEventNodeRef func eventNodeRef =
    liftEff $ modifyRef eventNodeRef $
        runExists \eventNode ->
            case eventNode of
                RootEventNode _ ->
                    -- This shouldn't actually happen ... at some stage, I should
                    -- review the design to see whether I can avoid this case.
                    mkExists eventNode

                SubEventNode _ parent ->
                    -- So, we're just substituting the func while keeping the same
                    -- parent ... this is the part we don't really have warrant for
                    -- in the types, except that we're going to proceed to update
                    -- the children so that the types will work out.
                    mkExists $ SubEventNode (unsafeCoerce func) parent


--  DIFF

--
-- Elm's Virtual DOM continues to diff children past taggers which are not
-- necessarily equal. Thus, we can't insist that the old node and the new node
-- have the same `msg` type -- we need to try to continue to diff even where
-- they do not. However, we use Leibniz equality to track whether we have
-- evidence that they are the same, so our behaviour can be a bit different in
-- one case vs. the other.

{-
diffHelp :: ∀ msg1 msg2. Maybe (msg1 ~ msg2) -> Node msg1 -> Node msg2 -> List Int -> List (Exists Patch) -> List (Exists Patch)
diffHelp proof a b index accum =
    PlainNode aNode aChildren, PlainNode bNode bChildren ->
        if aNode.tag /= bNode.tag || aNode.namespace /= bNode.namespace
            then snoc accum (makePatch (PRedraw b) index)
            else
                let
                    factsDiff =
                        diffFacts proof aNode.facts bNode.facts

                    patchesWithFacts =
                        if Array.null factsDiff
                            then accum
                            else snoc accum (makePatch (PFacts factsDiff) index)

                in
                    -- diffChildren calls diffHelp, and I'm guessing that it
                    -- possibly won't be tail-recursive, because it's not calling
                    -- itself? So, may need to do something about that? Or
                    -- possibly it's not a problem.
                    diffChildren proof aChildren bChildren patchesWithFacts index

-}

{-
diffFacts :: ∀ oldMsg newMsg. Maybe (oldMsg ~ newMsg) -> OrganizedFacts oldMsg -> OrganizedFacts newMsg -> Array (FactChange newMsg)
diffFacts proof old new =
    runPure do
        runSTArray do
            -- I suppose the other alternative would be a Writer monad ... perhaps
            -- that would be better?
            accum <- emptySTArray

            let
                newProperty _ k newValue =
                    case lookup k old.properties of
                        Just oldValue ->
                            -- Where the key is "value", we defer the check to
                            -- the actual DOM, rather than the virtual DOM, because
                            -- the browser will change "value" behind our backs.
                            -- Possibly "checked" should be handled this way as well:
                            -- https://github.com/elm-lang/virtual-dom/issues/117
                            --
                            -- The value is a Foreign, so the best we can do is
                            -- an unsafeRefEq here, I suppose?
                            when ((not (unsafeRefEq newValue oldValue)) || (k == "value")) $ void $
                                pushSTArray accum (AddProperty k newValue)

                        Nothing ->
                            void $
                                pushSTArray accum (AddProperty k newValue)

                oldProperty _ k oldValue =
                    case lookup k new.properties of
                        Just _ ->
                            -- We'll have done this one already ...
                            pure unit

                        Nothing ->
                            void $
                                pushSTArray accum (RemoveProperty k)

                newAttribute _ k newValue =
                    case lookup k old.attributes of
                        Just oldValue ->
                            unless (newValue == oldValue) $ void $
                                pushSTArray accum (AddAttribute k newValue)

                        Nothing ->
                            void $
                                pushSTArray accum (AddAttribute k newValue)

                oldAttribute _ k oldValue =
                    case lookup k new.attributes of
                        Just _ ->
                            -- We'll have done this one already ...
                            pure unit

                        Nothing ->
                            void $
                                pushSTArray accum (RemoveAttribute k)

                newAttributeNS _ ns newSubmap =
                    let
                        submapper _ k newValue =
                            case lookup ns old.attributesNS of
                                Just oldSubmap ->
                                    -- We had something in this namespace, so iterate
                                    case lookup k oldSubmap of
                                        Just oldValue ->
                                            unless (newValue == oldValue) $ void $
                                                pushSTArray accum (AddAttributeNS ns k newValue)

                                        Nothing ->
                                            void $
                                                pushSTArray accum (AddAttributeNS ns k newValue)

                                Nothing ->
                                    -- There wasn't anything in this namespace, so
                                    -- we'll need to add them all.
                                    void $
                                        pushSTArray accum (AddAttributeNS ns k newValue)

                    in
                        foldM submapper unit newSubmap

                oldAttributeNS _ ns oldSubmap =
                    let
                        submapper _ k value =
                            case lookup ns new.attributesNS of
                                Just newSubmap ->
                                    case lookup k newSubmap of
                                        Just newValue ->
                                            -- We'll have checked the newValue already
                                            pure unit

                                        Nothing ->
                                            void $
                                                pushSTArray accum (RemoveAttributeNS ns k)

                                Nothing ->
                                    -- There now isn't anything in this namespace, so remove all
                                    void $
                                        pushSTArray accum (RemoveAttributeNS ns k)

                    in
                        foldM submapper unit oldSubmap

                newStyle _ k newValue =
                    case lookup k old.styles of
                        Just oldValue ->
                            unless (newValue == oldValue) $ void $
                                pushSTArray accum (AddStyle k newValue)

                        Nothing ->
                            void $
                                pushSTArray accum (AddStyle k newValue)

                oldStyle _ k oldValue =
                    case lookup k new.styles of
                        Just _ ->
                             -- We'll handle this when iterating the new stuff.
                            pure unit

                        Nothing ->
                            void $
                                pushSTArray accum (RemoveStyle k)

                newEvent _ k (newOptions /\ newDecoder) =
                    case lookup k old.events of
                        Just (oldOptions /\ oldDecoder ) ->
                            unless (equalOptions oldOptions newOptions && equalDecodersL proof oldDecoder newDecoder) do
                                void $ pushSTArray accum (MutateEvent (wrap k) newOptions newDecoder)

                        Nothing ->
                            void $
                                pushSTArray accum (AddEvent (wrap k) newOptions newDecoder)

                oldEvent _ k (oldOptions /\ oldDecoder) =
                    case lookup k new.events of
                        Just _ ->
                            -- We'll handle this when iterating the new stuff.
                            pure unit

                        Nothing ->
                            void $
                                pushSTArray accum (RemoveEvent (wrap k) oldOptions)


            -- Push removals first, then additions. I've forgotten why each of
            -- these has an unused initial parameter ...
            foldM oldAttribute unit old.attributes
            foldM oldAttributeNS unit old.attributesNS
            foldM oldProperty unit old.properties
            foldM oldEvent unit old.events

            foldM newAttribute unit new.attributes
            foldM newAttributeNS unit new.attributesNS
            foldM newProperty unit new.properties
            foldM newEvent unit new.events

            case Tuple (isEmpty old.styles) (isEmpty new.styles) of
                Tuple false true ->
                    -- It wasn't empty but now is, so remove all
                    void $ pushSTArray accum RemoveAllStyles

                Tuple true true ->
                    -- It was empty and still is, so do nothing
                    pure unit

                Tuple false false -> do
                    -- There were some, and still are, so look
                    -- at both
                    foldM oldStyle unit old.styles
                    foldM newStyle unit new.styles

                Tuple true false ->
                    -- It was empty, and now isn't. So, just add.
                    foldM newStyle unit new.styles

            pure accum
-}

{-
diffChildren :: ∀ msg1 msg2. Maybe (msg1 ~ msg2) -> List (Node msg1) -> List (Node msg2) -> List (Exists Patch) -> List Int -> List (Exists Patch)
diffChildren proof aChildren bChildren patches rootIndex =
    let
        aLen = length aChildren
        bLen = length bChildren

        insertsAndRemovals =
            if aLen > bLen
                then snoc patches (makePatch (PRemoveLast (aLen - bLen)) rootIndex)
                else
                    if aLen < bLen
                        then snoc patches (makePatch (PAppend (drop aLen bChildren)) rootIndex)
                        else patches

        pairs =
            zip aChildren bChildren

        diffPairs =
            foldl diffChild { subIndex: 0, patches: insertsAndRemovals } pairs

        diffChild memo (Tuple aChild bChild) =
            { subIndex: memo.subIndex + 1
            , patches: diffHelp proof aChild bChild (snoc rootIndex memo.subIndex) memo.patches
            }

    in
        diffPairs.patches
-}

-- APPLY FACTS

{-
applyFacts :: ∀ f msg. (Foldable f) => f (FactChange msg) -> Element -> IOSync Unit
applyFacts operations elem = do
    for_ operations \operation ->
        case operation of
            AddAttribute key value ->
                liftEff $ setAttribute key value elem

            RemoveAttribute key ->
                liftEff $ removeAttribute key elem

            AddAttributeNS ns key value ->
                setAttributeNS ns key value elem

            RemoveAttributeNS ns key ->
                removeAttributeNS ns key elem

            AddEvent key options decoder -> do
                handlers  <-
                    (fromMaybe StrMap.empty <<< toMaybe) <$>
                        getHandlers elem

                handler <-
                    makeEventHandler {options, decoder}

                let newHandlers = StrMap.insert (unwrap key) handler handlers

                addEventHandler key handler false (elementToEventTarget elem)
                setHandlers newHandlers elem

            MutateEvent key options decoder -> do
                handlers <-
                    (fromMaybe StrMap.empty <<< toMaybe) <$>
                        getHandlers elem

                let handler =
                        case StrMap.lookup (unwrap key) handlers of
                            Just h ->
                                h

                            Nothing ->
                                unsafeCrashWith "Could not find expected handler in DOM"

                setHandlerInfo {options, decoder} handler

            RemoveEvent key options -> do
                handlers <-
                    (fromMaybe StrMap.empty <<< toMaybe) <$>
                        getHandlers elem

                let handler =
                        case StrMap.lookup (unwrap key) handlers of
                            Just h ->
                                h

                            Nothing ->
                                unsafeCrashWith "Could not find expected handler in DOM"

                removeEventHandler key handler false (elementToEventTarget elem)

                let newHandlers = StrMap.delete (unwrap key) handlers
                setHandlers newHandlers elem

            AddStyle key value ->
                setStyle key value elem

            RemoveStyle key ->
                removeStyle key elem

            RemoveAllStyles ->
                liftEff $ removeAttribute "style" elem

            AddProperty key value ->
                if key == "value"
                    -- Changes to value are deferred to the real DOM, instead of the virtual
                    -- DOM, since the browser will change "value" behind our backs.
                    -- Note that we should probably treat "checked" like this as well:
                    -- https://github.com/elm-lang/virtual-dom/issues/117
                    then setPropertyIfDifferent key value elem
                    else setProperty key value elem

            RemoveProperty key ->
                removeProperty key elem
-}

{-
applyPatch :: ∀ msg. Patch msg -> DOM.Node -> IOSync DOM.Node
applyPatch (Patch index patchOp) domNode = do
    document <-
        documentForNode domNode

    case patchOp of
        PRedraw vNode ->
            redraw domNode vNode

        PFacts changes -> do
            nodeToElement domNode
                <#> applyFacts changes
                # fromMaybe (pure unit)

            pure domNode

        PText string -> do
            liftEff $ setTextContent string domNode
            pure domNode

        PTagger taggerFunc -> do
            -- See comment in `render` ... I believe we can only apply a tagger
            -- to an element, but it might be good to verify that.
            for_ (nodeToElement domNode) \element ->
                taggerFunc # runExists \(TaggerFunc func) -> do
                    tagger <-
                        toMaybe <$> getTagger element

                    case tagger of
                        Just t ->
                            setHandlerInfo func t

                        Nothing ->
                            unsafeCrashWith "Could not find expected tagger in DOM"

            pure domNode

        PRemoveLast howMany -> liftEff do
            -- There must be a replicateM somewhere I'm forgetting ...
            forE 0 howMany \_ ->
                lastChild domNode >>=
                    case _ of
                        Just child -> do
                            void $ removeChild child domNode

                        Nothing ->
                            pure unit

            pure domNode

        PAppend newNodes -> do
            for_ newNodes $
                render document >=> ((flip appendChild) domNode >>> liftEff)

            pure domNode
-}



-- When Elm knows that there previously was a listener for an event, it does a
-- kind of "mutate listener" instead of removing the listener and adding a new
-- one. This might happen frequently, because testing the equality of decoders
-- is not fully decidable. So, you may end up with a lot of spurious listener
-- mutations. Removing and adding listeners is probably a little expensive, and
-- could even affect behaviour in some cases. So, instead, Elm mutates the
-- listener, by having the listener refer to some data "deposited" as a
-- property on the DOM node. Thus, we can mutate the listener by changing that
-- data.


-- | The information that a handler needs (in addition to the event!)
type HandlerInfo msg =
    { decoder :: Decoder msg
    , options :: Options
    }


-- | The generic listener for any Elm event.
-- |
-- | The `HandlerInfo` is a `Ref` so that we can track it externally and mutate
-- | it, without detaching and re-attaching the listener. Likewise the
-- | `EventNodeRef` ...  we can mutate it without changing the various
-- | listeners that might be using it. The Elm code achieves similar purposes,
-- | though through different techniques.
-- |
-- | We'll have to see how these references play with garbage collection ...
-- | hopefully it will be fine, but I should test it at some point to see
-- | whether we end up diposing of things as expected when they are no longer
-- | used.
-- |
-- | To actually create a listener that can be supplied to `addListener`, we
-- | basically just partially apply the first two parameters.
handleEvent :: ∀ msg. Ref (HandlerInfo msg) -> EventNodeRef msg -> Event -> IOSync Unit
handleEvent handlerInfoRef eventNodeRef event = do
    info <-
        liftEff $ readRef handlerInfoRef

    case decodeValue info.decoder $ toForeign event of
        Err _ ->
            -- Should we allow the client to do something with the error? Might
            -- not be a bad idea.  However, failing the decoder at this stage
            -- is a genuine strategy ... that way, you don't have to go through
            -- an `update` round.  So, it's not as though this *necessarily*
            -- represents a bug.
            pure unit

        Ok msg -> do
            -- Perhaps curiously, the Elm code doesn't stopPropagation or
            -- preventDefault unless the decoder succeeds ... so, we'll do that
            -- as well.
            when info.options.preventDefault $
                liftEff $ preventDefault event

            when info.options.stopPropagation $
                liftEff $ stopPropagation event

            -- Now, we run it through the event node.
            runEventNode msg eventNodeRef


-- | > Check out the docs for [`Html.App.program`][prog].
-- | > It works exactly the same way.
-- |
-- | [prog]: http://package.elm-lang.org/packages/elm-lang/html/latest/Html-App#program
program :: ∀ flags model msg.
    { init :: Tuple model (Cmd msg)
    , update :: msg -> model -> Tuple model (Cmd msg)
    , subscriptions :: model -> Sub msg
    , view :: model -> Node msg
    }
    -> Program flags model msg
program config =
    programWithFlags $
        config { init = const config.init }


-- | > Check out the docs for [`Html.App.programWithFlags`][prog].
-- | > It works exactly the same way.
-- |
-- | [prog]: http://package.elm-lang.org/packages/elm-lang/html/latest/Html-App#programWithFlags
programWithFlags :: ∀ flags model msg.
    { init :: flags -> Tuple model (Cmd msg)
    , update :: msg -> model -> Tuple model (Cmd msg)
    , subscriptions :: model -> Sub msg
    , view :: model -> Node msg
    }
    -> Program flags model msg
programWithFlags config =
    wrap
        { init : config.init
        , update : config.update
        , subscriptions : config.subscriptions
        , view : Just (renderer config.view)
        }


renderer :: ∀ model msg. (model -> Node msg) -> AVar msg -> IO (AVar model)
renderer view mailbox = liftAff do
    models <-
        makeEmptyVar

    -- Eventually this will need to be parameterized to make `embed`
    -- work ... for the moment, just assume `fullScreen`
    doc <-
        liftEff $ window >>= document

    -- TODO: Integrate with requestAnimationFrame
    let loop state = do
            newModel <-
                liftAff $ takeVar models

            let newView = view newModel

            {-
            newNode <-
                case state of
                    Nothing -> do
                        newNode <-
                            liftIOSync $ render (htmlDocumentToDocument doc) (view newModel)

                        maybeBody <-
                            liftEff $ body doc

                        let listener =
                                eventListener \event ->
                                    for_ (eventToCustomEvent event >>= detail) \msg ->
                                        -- This is a bit hackish ... we rely on things having
                                        -- been set up correctly so that the msg is of the
                                        -- correct type ... should revisit at some point.
                                        --
                                        -- Also, we're (necessarily?) in an Eff context here.
                                        -- So, we supply a callback that does nothing, and
                                        -- we get a canceler back which we're ignoring. Should
                                        -- think about what that means in terms of control flow.
                                        liftEff $ putVar (unsafeCoerce msg) mailbox (const $ pure unit)

                        for_ maybeBody \b -> liftEff do
                            void $ appendChild newNode (htmlElementToNode b)
                            addEventListener elmMsgEvent listener false (htmlElementToEventTarget b)

                        pure newNode

                    Just (oldView /\ oldNode) -> do
                        -- liftIOSync $ update oldView newView oldNode
                        pure oldNode
                -}

            loop $ Nothing -- Just (newView /\ newNode)

    void $ forkAff $ runIO' $ loop Nothing

    pure models


{-

// PROGRAMS

var program = makeProgram(checkNoFlags);
var programWithFlags = makeProgram(checkYesFlags);

function makeProgram(flagChecker)
{
	return F2(function(debugWrap, impl)
	{
		return function(flagDecoder)
		{
			return function(object, moduleName, debugMetadata)
			{
				var checker = flagChecker(flagDecoder, moduleName);
				if (typeof debugMetadata === 'undefined')
				{
					normalSetup(impl, object, moduleName, checker);
				}
				else
				{
					debugSetup(A2(debugWrap, debugMetadata, impl), object, moduleName, checker);
				}
			};
		};
	});
}

function staticProgram(vNode)
{
	var nothing = _elm_lang$core$Native_Utils.Tuple2(
		_elm_lang$core$Native_Utils.Tuple0,
		_elm_lang$core$Platform_Cmd$none
	);
	return A2(program, _elm_lang$virtual_dom$VirtualDom_Debug$wrap, {
		init: nothing,
		view: function() { return vNode; },
		update: F2(function() { return nothing; }),
		subscriptions: function() { return _elm_lang$core$Platform_Sub$none; }
	})();
}


// FLAG CHECKERS

function checkNoFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flags === 'undefined')
		{
			return init;
		}

		var errorMessage =
			'The `' + moduleName + '` module does not need flags.\n'
			+ 'Initialize it with no arguments and you should be all set!';

		crash(errorMessage, domNode);
	};
}

function checkYesFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flagDecoder === 'undefined')
		{
			var errorMessage =
				'Are you trying to sneak a Never value into Elm? Trickster!\n'
				+ 'It looks like ' + moduleName + '.main is defined with `programWithFlags` but has type `Program Never`.\n'
				+ 'Use `program` instead if you do not want flags.'

			crash(errorMessage, domNode);
		}

		var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
		if (result.ctor === 'Ok')
		{
			return init(result._0);
		}

		var errorMessage =
			'Trying to initialize the `' + moduleName + '` module with an unexpected flag.\n'
			+ 'I tried to convert it to an Elm value, but ran into this problem:\n\n'
			+ result._0;

		crash(errorMessage, domNode);
	};
}

function crash(errorMessage, domNode)
{
	if (domNode)
	{
		domNode.innerHTML =
			'<div style="padding-left:1em;">'
			+ '<h2 style="font-weight:normal;"><b>Oops!</b> Something went wrong when starting your Elm program.</h2>'
			+ '<pre style="padding-left:1em;">' + errorMessage + '</pre>'
			+ '</div>';
	}

	throw new Error(errorMessage);
}


//  NORMAL SETUP

function normalSetup(impl, object, moduleName, flagChecker)
{
	object['embed'] = function embed(node, flags)
	{
		while (node.lastChild)
		{
			node.removeChild(node.lastChild);
		}

		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, node),
			impl.update,
			impl.subscriptions,
			normalRenderer(node, impl.view)
		);
	};

	object['fullscreen'] = function fullscreen(flags)
	{
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, document.body),
			impl.update,
			impl.subscriptions,
			normalRenderer(document.body, impl.view)
		);
	};
}

function normalRenderer(parentNode, view)
{
	return function(tagger, initialModel)
	{
		var eventNode = { tagger: tagger, parent: undefined };
		var initialVirtualNode = view(initialModel);
		var domNode = render(initialVirtualNode, eventNode);
		parentNode.appendChild(domNode);
		return makeStepper(domNode, view, initialVirtualNode, eventNode);
	};
}


// STEPPER

var rAF =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { setTimeout(callback, 1000 / 60); };

function makeStepper(domNode, view, initialVirtualNode, eventNode)
{
	var state = 'NO_REQUEST';
	var currNode = initialVirtualNode;
	var nextModel;

	function updateIfNeeded()
	{
		switch (state)
		{
			case 'NO_REQUEST':
				throw new Error(
					'Unexpected draw callback.\n' +
					'Please report this to <https://github.com/elm-lang/virtual-dom/issues>.'
				);

			case 'PENDING_REQUEST':
				rAF(updateIfNeeded);
				state = 'EXTRA_REQUEST';

				var nextNode = view(nextModel);
				var patches = diff(currNode, nextNode);
				domNode = applyPatches(domNode, currNode, patches, eventNode);
				currNode = nextNode;
				return;

			case 'EXTRA_REQUEST':
				state = 'NO_REQUEST';
				return;
		}
	}

	return function stepper(model)
	{
		if (state === 'NO_REQUEST')
		{
			rAF(updateIfNeeded);
		}
		state = 'PENDING_REQUEST';
		nextModel = model;
	};
}

-}


-- UTILITIES

-- Perhaps should suggest this for purescript-dom?
nodeToElement :: DOM.Node -> Maybe Element
nodeToElement n =
    unsafePartial
        -- `nodeType` is marked `Partial`, because it uses `toEnum`, though
        -- you'd think it could discharge the constraint ... perhaps I should
        -- suggest that.
        case nodeType n of
            ElementNode ->
                Just $ unsafeNodeToElement n

            _ ->
                Nothing

    where
        unsafeNodeToElement :: DOM.Node -> Element
        unsafeNodeToElement = unsafeCoerce


-- Set arbitrary property. TODO: Should suggest for purescript-dom
foreign import setProperty :: String -> Foreign -> Element -> IOSync Unit

-- Get arbitrary property.
foreign import getProperty :: String -> Element -> IOSync (Nullable Foreign)

-- Remove a property.
foreign import removeProperty :: String -> Element -> IOSync Unit

-- Set if not already equal. A bit of a hack ... not suitable for general use.
foreign import setPropertyIfDifferent :: String -> Foreign -> Element -> IOSync Unit


-- TODO: Should suggest these for purescript-dom
foreign import setAttributeNS :: String -> String -> String -> Element -> IOSync Unit
foreign import getAttributeNS :: String -> String -> Element -> IOSync (Nullable String)
foreign import removeAttributeNS :: String -> String -> Element -> IOSync Unit


-- Sets the style named in the first param to the value of the second param
foreign import setStyle :: String -> String -> Element -> IOSync Unit

-- Removes the style
foreign import removeStyle :: String -> Element -> IOSync Unit


-- | Given a node, returns the document which the node belongs to.
documentForNode :: DOM.Node -> IOSync Document
documentForNode n =
    -- The unsafeCoerce should be safe, because if `ownerDocument`
    -- returns null, then the node itself must be the document.
    ownerDocument n
        <#> fromMaybe (unsafeCoerce n)
        # liftEff
