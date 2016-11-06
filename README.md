# Introduction

This library provides an easy and powerful way to use nested children
components in your Elm program with very little boilerplate. Parent
components can easily communicate with children components and react to
any changes that might occur in them.

You can nest components to any depth and the components are just
regular Elm programs, with the usual `Msg`, `Model`, `update`,
`view`, `subscription`, that can be compiled and tested individually.

The only boilerplate you need to add to a parent in order for it
to handle any number or variety of components, is an extra message
constructor in your `Msg` that takes an `Updater` of the parent's
`Model` and `Msg` type:

```elm
type Msg = ...
         | UpdaterMsg (Updater Model Msg)
```

Then, in your `update` function, you just have to add one line to
handle the updater message type, which applies the updater to your
current model:

```elm
update msg model = case msg of
                      ...
                      UpdaterMsg u -> u model

```

`Updater`s are generated by `Converter`s that have been applied to a child
component's message type. To make a converter for a child component,
you just need to declare an `Interface` with the following information:

* `get` - how to get the child component's model from the parent model.
* `set` - how to update the child component's model within the parent model.
* `update` - the child component's standard `update` function.
* `react` - (optional) lets the parent specially react to any of the child's messages.

Once defined, a `Converter` can be used with `Html.map`, `Sub.map`, or
`Cmd.map` to convert any `Html`, `Sub`, or `Cmd` containing the child
component's message type. Just `map` the converter onto the child
message and `Updater` will take care of the rest (and if Elm had
typeclasses, you wouldn't even have to specify the `map`).

This library makes using nested components easy and efficient.
It should encourage the creation and sharing of re-usable, decoupled
components.
It also allows for some really nice abstractions, like the `Many`
component (shown as in examples far below), which lets you easily
create and display a collection of any component.

# Examples

### The Stock Modules

There are three stock components that we'll use to build up more
complicated components:

* `EditableLabel` is a label that can be changed with an edit button
([demo](http://otherway.org/updater/EditableLabel.html), [source](https://github.com/mpdairy/elm-component-updater/blob/master/examples/src/Component/EditableLabel.elm))
* `Timer` is a standard kitchen timer that counts down from whatever
you set and flashes when it reaches zero
([demo](http://otherway.org/updater/Timer.html), [source](https://github.com/mpdairy/elm-component-updater/blob/master/examples/src/Component/Timer.elm))
* `SuperBuzzer` is just a big, animated "buzzer" that really grabs
your attention. When you click on it, it stops.
([demo](http://otherway.org/updater/SuperBuzzer.html), [source](https://github.com/mpdairy/elm-component-updater/blob/master/examples/src/Component/SuperBuzzer.elm))

These components and the examples to follow are only styled enough to
make them usable for the purpose of demonstration.

## TaskTimer

A `TaskTimer` is just a kitchen timer with a task name and
description. The idea is that later we will use many of them to make a
timed todo list.

First, the module name and imports:

```elm
module Component.TaskTimer exposing (Msg (Reset, BuzzMessage), Model, init, update, view, subscriptions)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Updater exposing (converter, Updater, Converter, Interface, toCmd, noReaction)

import Component.Timer as Timer
import Component.EditableLabel as Label
```

#### Model

For the `Model`, we'll include the `Timer`, two `Label`s, and a
`buzzCount`, which will keep track of the number of times the timer
has buzzed:

```elm
type alias Model = { timer : Timer.Model
                   , name : Label.Model
                   , task : Label.Model
                   , buzzCount : Int }
```

#### Msg

Since the `TaskTimer` is meant to be a useful child component, we'll
have to make some useful messages for whatever parent will be using it.
Let's make a `Reset` message that will reset the `Timer`s clock, and a
`BuzzMessage` that will occur whenever the `Timer` buzzes, sending out
the `name` and `task` description.

```elm
type Msg = Reset
         | BuzzMessage String String
         | UpdaterMsg (Updater Model Msg)
```

`UpdaterMsg` is required for updating the children components.


#### Converters

Now it's time to define the `Converter` interfaces for each of the
children components. The function `converter` takes an updater message
constructor (`UpdaterMsg` in this case, though it could be named
anything). It also takes an `Interface`, which is defined in the `Updater`
module as:

```elm
type alias Interface pModel pMsg cModel cMsg =
    { get    : ( pModel -> Maybe cModel )
    , set    : ( cModel -> pModel -> pModel )
    , update : ( cMsg -> cModel -> ( cModel, Cmd cMsg ) )
    , react  : ( cMsg -> cModel -> pModel -> ( pModel, Cmd pMsg ) ) }
```

where `pModel` and `pMsg` are the parent's `Model` and `Msg`, and `cModel`
and `cMsg` are the child component's `Model` and `Msg`.

The `react` function is given any of the child component's
messages and the child's model (already updated with that message), as
well as the parent's model.

Here are the converters for both the `name` and `task` labels:

```elm
nameC : Converter Msg Label.Msg
nameC = converter
        UpdaterMsg
        { get = Just << .name
        , set = (\ cm m -> { m | name = cm } )
        , update = Label.update
        , react = noReaction }

taskC : Converter Msg Label.Msg
taskC = converter
        UpdaterMsg
        { get = Just << .task
        , set = (\ cm m -> { m | task = cm } )
        , update = Label.update
        , react = noReaction }
```

The `set` functions are just the standard Elm record updating syntax.
The `get` function results are wrapped in a `Maybe`, which is
necessary when getting children models out of `Dict` or `Array`. Here
we compose the record getting syntax with a `Just`.

We don't need to react to anything that the labels do. To access their
text we can just reach directly into their models using
`model.task.text` or `model.name.text`. The `noReaction` function is defined in
the `Updater` module and always returns `(model, Cmd.none)`.

Now let's define `timerC` for the timer. The `get` and `set` functions
will be similar to `taskC` and `nameC`. But this time we want to react
to its messages: whenever it sees the `Timer.Buzz` message, it will
increment the `buzzCount` and have the `TaskTimer` trigger a `BuzzMessage` message:

```elm
timerC : Converter Msg Timer.Msg
timerC = converter
         UpdaterMsg
         { get = Just << .timer
         , set = (\ cm m -> { m | timer = cm } )
         , update = Timer.update
         , react = (\ tmsg _ model ->
                         case tmsg of
                             Timer.Buzz ->
                                 { model | buzzCount = model.buzzCount + 1 }
                                 ! [ toCmd <| BuzzMessage model.name.text model.task.text ]
                             _ -> model ! [] ) }
```

The `toCmd` function is part of the `Updater` module and uses `Task.perform` to box
any message into a `Cmd`, which lets you send messages to the `update`
function, and, as we'll see below, directly to any child components.

#### view

The view function is pretty normal. Everytime it displays a
child component's model, it will pass the child's model into the child's `view`
function. For example, to display the timer, we can call:

```elm
Timer.view model.timer
```
This, however, will return an `Html Timer.Msg`, which needs to be
converted to an `Html Msg`. We can do this using `Html.map` and the
`timerC` converter defined above:

```elm
Html.map timerC <| Timer.view model.timer
```

Notice that `Timer.view` is called manually, so if the `Timer` module
has some other, more advanced `view` function that takes other
arguments, it could be used instead, or you could wrap the `Timer.view`
in custom `Html` tags with `Timer.Msg` events. Whatever you write, it just
needs to return an `Html Timer.Msg` before you
convert it with `Html.map timerC`.

Here's the complete `view` function with all three child components.
Most of the bloat comes from my sloppy CSS:

```elm
view : Model -> Html Msg
view model =
  div [ style [ ("border", "1px solid #aaa"), ("margin", "10px")
              , ("width", "175px"), ("padding", "10px")
              , ("float", "left"), ("height", "320px")] ]
      [ Html.map timerC <| Timer.view model.timer
      , div [] [ h3 [] [text "Name:"]
               , Html.map nameC <| Label.view model.name ]
      , div [] [ h3 [] [text "Description:"]
               , Html.map taskC <| Label.view model.task ]

      , h4 [] [ text <| "Times Buzzed: " ++ toString model.buzzCount ]
      ]
```
#### update

The `update` function only needs to handle three options. One of them
is the `UpdaterMsg`, which only needs to apply the updater to the
current model. Another is `BuzzMessage`, which is just an informative
broadcast.

`Reset` is a command for a parent to reset the `Timer` of the
`TaskTimer`. One option would be to directly update the model of the
`Timer` using Elm's record updating syntax, but that would be tedius
and sloppy.
`Timer.Msg` already has a `Stop` message that does what we want. To
send it to the `timer`, `Timer.Stop` needs to be turned into a `Cmd`
using `toCmd`, then convert it using `Cmd.map` and the `timerC`
converter:

```elm
Cmd.map timerC <| toCmd Timer.Stop
```

Here's the complete `update` function:

```elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
      Reset -> model ! [ Cmd.map timerC <| toCmd Timer.Stop ]

      BuzzMessage _ _ -> model ! []

      UpdaterMsg u -> u model
```

#### subscriptions

The `Label` components don't use any subscriptions, but the `Timer`
component needs subscriptions sometimes to count down every second.
`Timer.subscriptions` returns `Sub Timer.Msg`, which we can convert
using `Sub.map timerC`:

```elm
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map timerC <| Timer.subscriptions model.timer
```

#### init

The `Label` components export `initModel` functions, which take an
initial label text string as an argument.

The `Timer` component exports a standard `init` function that returns

```elm
(Timer.Model, Cmd Timer.Msg)
```

The `Cmd`s that `Timer.init` returns are because the creator of the
timer component thought it would be a good idea to have a random
initial starting value between 1 and 30 seconds, so it calls a
`Random` task right away to generate them. We need to include these
`Cmd`s and convert them using `timerC`.

```elm
init : (Model, Cmd Msg)
init = let ( tmodel, tcmd ) = Timer.init
       in
           { timer = tmodel
           , task = Label.initModel "Enter a task description here."
           , name = Label.initModel "Timer Name"
           , buzzCount = 0 }
    ! [ Cmd.map timerC tcmd ]
```

#### main

```elm
main : Program Never
main =
    Html.program { init = init
                 , update = update
                 , subscriptions = subscriptions
                 , view = view }
```

That's it! Now you can compile and test `TaskTimer` as an individual
module (demo, source).

## TaskTimer Cluster

What if you want to have a whole bunch of `TaskTimer`s, like an
unlimited amount of them that the user can add or delete at will?

I'll show you two ways to do this. First, a tedious way that uses a
`Dict`, like I'm sure you've done before if you've worked with Elm
much. Second, I'll show you the `Many` component, which abstracts away
the boring part and can be used to make a collection of any type of component.

### The Tedious Way

#### module and imports

```elm
module Component.ManualTimerCluster exposing (Msg (..), Model, init, update, view)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing ( onClick )
import Dict exposing (Dict)
import Updater exposing (converter, Updater, Converter, Interface, toCmd, noReaction)

import Component.TaskTimer as TaskTimer
```

#### model and message

Inside the model, a `Dict` will store the `TaskTimer`s and give each of them a
unique id.

For the `Msg`, `AddTimer` and `DeleteTimer` will add
or delete the timers from the `Dict`.

`newID` will store the next unused id, and will increment every time a
new timer is added.

```elm
type alias Model = { timers : Dict Int TaskTimer.Model
                   , newID : Int }


type Msg = AddTimer (TaskTimer.Model, Cmd TaskTimer.Msg)
         | DeleteTimer Int
         | UpdaterMsg (Updater Model Msg)

```

Note that `AddTimer` basically takes the result of `TaskTimer.init` as
an argument.

#### Timer Converter

The `TaskTimer` models are stored inside a `Dict`, so we'll have to
have the converter's `get` and `set` functions reach into the `Dict`.
The converter will also need to take an `Int` id as an argument to
know which `TaskTimer` in the `Dict` should be accessed.

```elm
timerC : Int -> Converter Msg TaskTimer.Msg
timerC n = converter
           UpdaterMsg
           { get = (\ model -> Dict.get n model.timers)
           , set = (\ timer model -> { model | timers = Dict.insert n timer model.timers } )
           , update = TaskTimer.update
           , react = noReaction }
```

Now we can just call, for example, `timerC 5` and get a `Converter` for the
`TaskTimer` stored with the id of `5`.

#### init

The `init` is easy because it starts with an empty `Dict`:

```elm
init : (Model, Cmd Msg)
init = { timers = Dict.empty
       , newID = 0 }
    ! [ ]
```

#### update

For the `update` function, `DeleteTimer id` just needs to remove the
timer from the `Dict`.

For `AddTimer`, it needs to:

* add the new timer's init model to the `Dict`
* increment the `newID` record
* because `TaskTimer.init` sends out some initial `Cmd`s to get a
random starting value for its `Timer`, we need to use `timerC` with the new
`TaskTimer`'s id to use the messages on the new entry.

```elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
      AddTimer (timerModel, timerCmd) ->
          { model | timers = Dict.insert model.newID timerModel model.timers
          , newID = model.newID + 1 }
          ! [ Cmd.map (timerC model.newID) <| timerCmd ]

      DeleteTimer id -> { model | timers = Dict.remove id model.timers } ! []

      UpdaterMsg u -> u model
```

#### subscriptions

For the subscriptions, just cycle through the `TaskTimer`s in
the dict, converting each subscription by id:

```elm
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <| List.map (\ (id, timerModel) ->
                              Sub.map (timerC id) <| TaskTimer.subscriptions timerModel )
                    (Dict.toList model.timers)
```

#### view

For the `view`, you can just iterate through a `toList` of the `Dict`,
use `TaskTimer.view` on each model and map `timerC` with the `id` to
convert to the parent's message. The `deletableTimer` function wraps
each timer with a delete button that calls the `DeleteTimer` message
for an `id`.

There's also an "Add Timer" button that just returns the `Addtimer`
message with `TaskTimer.init` as an argument.

```elm
view : Model -> Html Msg
view model =
  div [ ]
      [ div [ style [("height", "420px")]] <|
            List.map (\ (id, timerModel) ->
                          deletableTimer id <|
                          Html.map (timerC id) <| TaskTimer.view timerModel)
            (Dict.toList model.timers)
      , div [] [ button [ onClick <| AddTimer TaskTimer.init ] [ text "Add Timer" ] ]
      ]

deletableTimer : Int -> Html Msg -> Html Msg
deletableTimer id html = div [ style [ ("width", "215px")
                                     , ("float", "left")
                                     , ("height", "320px") ] ]
               [ html
               , button [ onClick <| DeleteTimer id ] [ text "Delete" ]
               ]
```

#### main

```elm
main : Program Never
main =
    Html.program { init = init
                 , update = update
                 , subscriptions = subscriptions
                 , view = view }
```

That's it! Another fully functional component (demo, source). But do
you really like juggling objects in a `Dict` like that?

## TaskTimer Cluster using Many

Using a `Dict` isn't too bad for one collection, but imagine you had a
page with many different collections of components. Think how bloated
your code might become from all the calls to `Dict.toList`, and how
many `AddThis` and `RemoveThat` messages you might have to pollute
your `Msg` with!

Using the `Many` component, you can abstract this `Dict`-handling, and
more-easily and succinctly handle collections of components.

#### headers

```elm
module Component.ManyTimerCluster exposing (Msg (..), Model, init, update, view, subscriptions)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing ( onClick )
import Updater exposing (converter, Updater, Converter, Interface, toCmd, noReaction)

import Component.TaskTimer as TaskTimer
import Component.Many as Many
```

#### Many Msg and Model type aliases

`Many` has polymorphic types for its model and message:

```elm
type alias Model cModel cMsg = ...

type Msg cModel cMsg = ...
```

`cModel` and `cMsg` are the msg and model of whatever component will
be stored in the `Many` component, which, in this case, is a
`TaskTimer`, so the `Many` model would be `Many.Model TaskTimer.Model
TaskTimer.Msg`. We'll use type aliases to avoid having to write such
long types:

```elm
type alias TimersModel = Many.Model TaskTimer.Model TaskTimer.Msg

type alias TimersMsg = Many.Msg TaskTimer.Model TaskTimer.Msg
```

#### Msg and Model

```elm
type alias Model = { timers : TimersModel }

type Msg = NoOp
         | UpdaterMsg (Updater Model Msg)
```

Pretty boring, huh? I just added in `NoOp` for decoration, really.

#### Converter

```elm
timersC : Converter Msg TimersMsg
timersC = converter
           UpdaterMsg
           { get = Just << .timers
           , set = (\ cm model -> { model | timers = cm } )
           , update = Many.update
           , react = noReaction }
```

Notice that `update` is `Many.update` and not
 `TaskTimer.update`.


#### init

```elm
init : (Model, Cmd Msg)
init = { timers = Many.initModel TaskTimer.update TaskTimer.subscriptions }
    ! [ ]
```

`Many.initModel` takes the hosted component type's `update` function and the `subscriptions`
function for the initial argument.

It doesn't actually need to `init` any `TaskTimer`s until they are added using the `Many.Add`
message (which could be done in the `Cmd Msg` of this init).

#### update

```elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
      NoOp -> model ! []
      UpdaterMsg u -> u model
```

#### subscriptions

```elm
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map timersC <| Many.subscriptions model.timers
```

#### view

Ah! Now for something interesting!

The `Many` module's model generates a few convenience functions
that make it easy to display the components in its collection. One of
those functions is called `viewAll`, which you access through the
model, like `model.timer.viewAll`. It expects a callback function with
this type signature:

```elm
( Int -> cModel -> ( Html cMsg -> Html (Msg cModel cMsg) )
                -> Maybe (Html (Msg cModel cMsg)) )
```

Don't be intimidated by this, it's actually really simple. The
callback function that you supply will get these arguments:

* the id of the component
* the model of the component
* a conversion function that changes `Html (component message)` to
`Html (Many message)`.

The `(component message)` is referring, in this case, to a
`TaskTimer.Msg`, which means that you can display `Html TaskTimer.Msg`
types, like by calling `TaskTimer.view`. The conversion function is
then used to transform that into, in this case, `Html (Many
TaskTimer.Model TaskTimer.Msg)`, or, using the type alias already
defined, just `Html TimersMsg`.

This allows you to display something for the `TaskTimer`, but also to
surround it with events that control its place in the `Many`
component, such as by using the `Many.Delete id` message to delete
it.

The callback function returns the `Html` in a `Maybe` so you can
display `Nothing` if you want to skip displaying certain components.

```elm
view : Model -> Html Msg
view model =
    div []
        [ Html.map timersC <| viewTimers model.timers ]

viewTimers : TimersModel -> Html TimersMsg
viewTimers timers =
    div [ class "timers" ]
        [ div [ style [("height", "420px")]] <|

              timers.viewAll
              (\ id timer conv -> Just <|
                   div [ style [ ("width", "215px")
                               , ("float", "left")
                               , ("height", "320px") ] ]
                   [ conv <| TaskTimer.view timer
                   , button [ onClick <| Many.Delete id ] [ text "Delete" ] ])

        , div [] [ button [ onClick <| Many.Add TaskTimer.init ] [ text "Add Timer" ]]]
```

#### main

```elm
main : Program Never
main =
    Html.program { init = init
                 , update = update
                 , subscriptions = subscriptions
                 , view = view }
```

Here's the complete source and a demo.

You can see how much easier it is to reason about using `Many` and how
much better organized it is than throwing in your own `Dict` solution
for every component type.

## A Super Timer Application

I won't make you suffer through a complete walkthrough of my
SuperTimer application. By now you should be able to look at the
source code and understand it, except for some of the message passing
parts that use `Many`, which I'll explain.

#### SuperTimer!

The `SuperTimer` is just like the Timer Clusters we looked at in the
last two examples, except it also connects to the `SuperBuzzer` module
to help alert the user of the `SuperTimer` when any timer is buzzing.
Also, a list of actively buzzing tasks will be displayed next to the
SuperBuzzer. When the giant pulsating red SuperBuzzer is clicked, a
message will get passed back to stop all the buzzing timers.

#### Reading messages through a Many

The `SuperTimer` uses the same `TaskTimer` in a `Many` setup, except
it needs to react to any `TaskTimer.BuzzMessage` messages so it can
trigger the `SuperBuzzer` and store the task name and description in
the collection of active tasks.

Here's the `timersC` conversion function with a `react` that does just
that:

```elm
timersC : Converter Msg TimersMsg
timersC = converter
          UpdaterMsg
          { get = Just << .timers
          , set = \ cm model -> { model | timers = cm }
          , update = Many.update
          , react = \ mMsg _ model ->
                         case mMsg of
                             Many.From id tMsg _ ->
                                 case tMsg of
                                     TaskTimer.BuzzMessage name description ->
                                         model
                                         ! [ toCmd <| Buzz id name description ]
                                     _ -> model ! []
                             _ -> model ! [] }
```

The message type, `Many.From` passes down the id, message, and model of
the component.

#### Sending messages through a Many

What if you want to send a message to a component stored inside a
`Many`? For that, use the message, `Many.SendTo`, which takes the
component id and a component's message.

In the `SuperTimer` app, all the id's for the `TaskTimer`s that have
buzzed, and their task names and descriptions, are stored in the
`buzzedTimers` dictionary. Below is how to send the `TaskTimer.Reset`
message through the `Many` component:

```elm
update msg model =
   case msg of
      ...
      UnBuzz -> { model | buzzedTimers = Dict.empty }
                ! (List.map
                       (\ id -> Cmd.map timersC <| toCmd <|
                            Many.SendTo id TaskTimer.Reset )
                            (Dict.keys model.buzzedTimers))
```

#### Demo and Source

See a demo of the SuperTimer, or view the source code.


# Component Conventions

In my opinion, this `Updater` library should make using reusable components in
Elm much more popular. Wouldn't it be great if everybody was
contributing a bunch of useful components? Think how fast you could
build up really complicated apps!

And the components aren't limited to just GUI widgets. You could make
headless components that provide a useful `Msg` API to interact with
other web API's, like Google Maps, Facebook, or reThink DB. Basically,
anything that needs to manage its own state should be a component,
especially if you want to use it more than once.

By splitting your program up into many component
modules, not only do you get great reusability, but it's easier
to keep your main app's `Msg` small and understandable. It's also
easier to test individual components.

### Component.Xxxxxx Module Names

It seems like we should name anything that is just meant to be a
component with a module name like `Component.SuperTimer`, with a
`Component.` in front. That way
it's obvious when you're importing components and when you're
importing libraries that provide more general functionality.

### Component Files

Even if you want to spread out a component over multiple files, please
expose one main file, `Component.Xxxxx`, that exposes the main useful things
like `Msg`, `Model`, `update`, `subscriptions`, etc, so that it's easy
to import.

### Component Msg

Limit which parts of the message get exposed and document which
messages are useful for output or for component control. Try to make
it difficult for people to wire up an infinite `Cmd` loop between your
component and their `react` function.
