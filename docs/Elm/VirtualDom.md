## Module Elm.VirtualDom

> API to the core diffing algorithm. Can serve as a foundation for libraries
> that expose more helper functions for HTML or SVG.

#### `Node`

``` purescript
data Node msg
```

> An immutable chunk of data representing a DOM node. This can be HTML or SVG.

##### Instances
``` purescript
Functor Node
```

#### `text`

``` purescript
text :: forall msg. String -> Node msg
```

> Just put plain text in the DOM. It will escape the string so that it appears
> exactly as you specify.
>
>     text "Hello World!"

#### `node`

``` purescript
node :: forall f g msg. Foldable f => Foldable g => String -> f (Property msg) -> g (Node msg) -> Node msg
```

> Create a DOM node with a tag name, a list of HTML properties that can
> include styles and event listeners, a list of CSS properties like `color`, and
> a list of child nodes.
>
>     import Json.Encode as Json
>
>     hello :: Node msg
>     hello =
>       node "div" [] [ text "Hello!" ]
>
>     greeting :: Node msg
>     greeting =
>       node "div"
>         [ property "id" (Json.string "greeting") ]
>         [ text "Hello!" ]

#### `Property`

``` purescript
data Property msg
```

> When using HTML and JS, there are two ways to specify parts of a DOM node.
>
>   1. Attributes &mdash; You can set things in HTML itself. So the `class`
>      in `<div class="greeting"></div>` is called an *attribute*.
>
>   2. Properties &mdash; You can also set things in JS. So the `className`
>      in `div.className = 'greeting'` is called a *property*.
>
> So the `class` attribute corresponds to the `className` property. At first
> glance, perhaps this distinction is defensible, but it gets much crazier.
> *There is not always a one-to-one mapping between attributes and properties!*
> Yes, that is a true fact. Sometimes an attribute exists, but there is no
> corresponding property. Sometimes changing an attribute does not change the
> underlying property. For example, as of this writing, the `webkit-playsinline`
> attribute can be used in HTML, but there is no corresponding property!

##### Instances
``` purescript
Functor Property
```

#### `property`

``` purescript
property :: forall msg. String -> Value -> Property msg
```

> Create arbitrary *properties*.
>
>     import JavaScript.Encode as Json
>
>     greeting : Html
>     greeting =
>         node "div" [ property "className" (Json.string "greeting") ] [
>           text "Hello!"
>         ]
>
> Notice that you must give the *property* name, so we use `className` as it
> would be in JavaScript, not `class` as it would appear in HTML.

#### `attribute`

``` purescript
attribute :: forall msg. String -> String -> Property msg
```

> Create arbitrary HTML *attributes*. Maps onto JavaScript’s `setAttribute`
> function under the hood.
>
>     greeting : Html
>     greeting =
>         node "div" [ attribute "class" "greeting" ] [
>           text "Hello!"
>         ]
>
> Notice that you must give the *attribute* name, so we use `class` as it would
> be in HTML, not `className` as it would appear in JS.

#### `attributeNS`

``` purescript
attributeNS :: forall msg. String -> String -> String -> Property msg
```

> Would you believe that there is another way to do this?! This corresponds
> to JavaScript's `setAttributeNS` function under the hood. It is doing pretty
> much the same thing as `attribute` but you are able to have "namespaced"
> attributes. This is used in some SVG stuff at least.

Note that the first argument is the namespace, the second the label, and
third the value.

#### `mapProperty`

``` purescript
mapProperty :: forall a b. (a -> b) -> Property a -> Property b
```

> Transform the messages produced by a `Property`.

Equivalent to Purescript's `map`

#### `style`

``` purescript
style :: forall f msg. Foldable f => f (Tuple String String) -> Property msg
```

> Specify a list of styles.
>
>     myStyle :: Property msg
>     myStyle =
>       style
>         [ Tuple "backgroundColor" "red"
>         , Tuple "height" "90px"
>         , Tuple "width" "100%"
>         ]
>
>     greeting :: Node msg
>     greeting =
>       node "div" [ myStyle ] [ text "Hello!" ]

#### `on`

``` purescript
on :: forall msg. String -> Decoder msg -> Property msg
```

> Create a custom event listener.
>
>     import Json.Decode as Json
>
>     onClick : msg -> Property msg
>     onClick msg =
>       on "click" (Json.succeed msg)
>
> You first specify the name of the event in the same format as with JavaScript’s
> `addEventListener`. Next you give a JSON decoder, which lets you pull
> information out of the event object. If the decoder succeeds, it will produce
> a message and route it to your `update` function.

#### `onWithOptions`

``` purescript
onWithOptions :: forall msg. String -> Options -> Decoder msg -> Property msg
```

> Same as `on` but you can set a few options.

#### `Options`

``` purescript
type Options = { stopPropagation :: Bool, preventDefault :: Bool }
```

> Options for an event listener. If `stopPropagation` is true, it means the
> event stops traveling through the DOM so it will not trigger any other event
> listeners. If `preventDefault` is true, any built-in browser behavior related
> to the event is prevented. For example, this is used with touch events when you
> want to treat them as gestures of your own, not as scrolls.

#### `defaultOptions`

``` purescript
defaultOptions :: Options
```

> Everything is `False` by default.
>
>     defaultOptions =
>         { stopPropagation = False
>         , preventDefault = False
>         }

#### `equalOptions`

``` purescript
equalOptions :: Options -> Options -> Bool
```

Elm doesn't need this function because its `==` can handle record types
magically. This isn't possible in Purescript without a newtype, which we'd
like to avoid here. So, we define a custom equality function.

#### `lazy`

``` purescript
lazy :: forall a msg. Eq a => (a -> Node msg) -> a -> Node msg
```

> A performance optimization that delays the building of virtual DOM nodes.
>
> Calling `(view model)` will definitely build some virtual DOM, perhaps a lot of
> it. Calling `(lazy view model)` delays the call until later. During diffing, we
> can check to see if `model` is referentially equal to the previous value used,
> and if so, we just stop. No need to build up the tree structure and diff it,
> we know if the input to `view` is the same, the output must be the same!

The diffing process will operate somewhat more efficiently if the function you
provide has a stable reference (that we can check for reference equality the
next time we see it).

For a version of this function that doesn't require an `Eq` instance, see
`lazy_`. This one will do a better job of detecting equality.

#### `lazy_`

``` purescript
lazy_ :: forall a msg. (a -> Node msg) -> a -> Node msg
```

Like `lazy`, but does not require an `Eq` instance. Using `lazy` will do
a better job of detecting equality.

#### `lazy2`

``` purescript
lazy2 :: forall a b msg. Eq a => Eq b => (a -> b -> Node msg) -> a -> b -> Node msg
```

> Same as `lazy` but checks on two arguments.

The diffing process will operate somewhat more efficiently if the function you
provide has a stable reference (that we can check for reference equality the
next time we see it).

#### `lazy2_`

``` purescript
lazy2_ :: forall a b msg. (a -> b -> Node msg) -> a -> b -> Node msg
```

Like `lazy2`, but does not require an `Eq` instance. Using `lazy2` will do
a better job of detecting equality.

#### `lazy3`

``` purescript
lazy3 :: forall a b c msg. Eq a => Eq b => Eq c => (a -> b -> c -> Node msg) -> a -> b -> c -> Node msg
```

> Same as `lazy` but checks on three arguments.

The diffing process will operate somewhat more efficiently if the function you
provide has a stable reference (that we can check for reference equality the
next time we see it).

#### `lazy3_`

``` purescript
lazy3_ :: forall a b c msg. (a -> b -> c -> Node msg) -> a -> b -> c -> Node msg
```

Like `lazy3`, but does not require an `Eq` instance. Using `lazy3` will do
a better job of detecting equality.

#### `keyedNode`

``` purescript
keyedNode :: forall f g msg. Foldable f => Foldable g => String -> f (Property msg) -> g (Tuple String (Node msg)) -> Node msg
```

> Works just like `node`, but you add a unique identifier to each child
> node. You want this when you have a list of nodes that is changing: adding
> nodes, removing nodes, etc. In these cases, the unique identifiers help make
> the DOM modifications more efficient.

#### `program`

``` purescript
program :: forall flags model msg. { init :: Tuple model (Cmd msg), update :: msg -> model -> Tuple model (Cmd msg), subscriptions :: model -> Sub msg, view :: model -> Node msg } -> Program flags model msg
```

> Check out the docs for [`Html.App.program`][prog].
> It works exactly the same way.

[prog]: http://package.elm-lang.org/packages/elm-lang/html/latest/Html-App#program

#### `programWithFlags`

``` purescript
programWithFlags :: forall flags model msg. { init :: flags -> Tuple model (Cmd msg), update :: msg -> model -> Tuple model (Cmd msg), subscriptions :: model -> Sub msg, view :: model -> Node msg } -> Program flags model msg
```

> Check out the docs for [`Html.App.programWithFlags`][prog].
> It works exactly the same way.

[prog]: http://package.elm-lang.org/packages/elm-lang/html/latest/Html-App#programWithFlags


### Re-exported from Prelude:

#### `map`

``` purescript
map :: forall a b f. Functor f => (a -> b) -> f a -> f b
```

