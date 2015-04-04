[![Build Status](https://secure.travis-ci.org/MedeaMelana/JsonGrammar2.png?branch=master)](https://travis-ci.org/MedeaMelana/JsonGrammar2)

# Introducing JsonGrammar

JsonGrammar is a Haskell library for converting between Haskell datatypes and
[JSON](http://en.wikipedia.org/wiki/JSON) ASTs. See the [API documentation on
Hackage](http://hackage.haskell.org/package/JsonGrammar).

*"What, another JSON library? Don't we have enough already?"*

It's true that there are already a few JSON libraries out there. These
libraries, however, require you to write `fromJson` and `toJson` separately.

*"Uhm, yes... is that bad?"*

Yes. It violates the [DRY
principle](http://en.wikipedia.org/wiki/Don%27t_repeat_yourself). If I show
you an implementation of `fromJson` for a certain type, you can write a
corresponding `toJson` without requiring any further information. Similarly,
if I show you an implementation of `toJson`, you can write the accompanying
`fromJson`. Writing down the same thing twice is tedious and opens up the
possibility to make mistakes.

*"But most of these libraries offer Template Haskell support that does this
work for you!""*

This is true, but they also make all the choices for you about how your
datatypes should map to JSON. Usually they assume the names of your record
fields map directly to JSON property names. The shapes of your family of
datatypes need to correspond to how the objects in JSON are nested. These
libraries give you the choice: either you write out `fromJson` and `toJson` by
hand and have full control over the mapping, or you give up this control and
let Template Haskell do all the work for you.

JsonGrammar gives you the best of both worlds: it gives you full control over
what the mapping should be, with an API that lets you define `fromJson` and
`toJson` at the same time. It achieves this by separating the
constructing/destructing of datatype constructors and its fields from the
description of the JSON values. The former is derived by Template Haskell, the
latter is provided by the programmer.

## Design principles

* Write JSON *grammars* that specify bidirectional conversion between JSON and Haskell datatypes
* Grammars are succinct yet flexible
* Adapt to existing JSON formats
* Adapt to existing Haskell datatypes
* Highly modular

## An example

Suppose we have these two datatypes describing people and their current
location:

```
data Person = Person
  { name   :: String
  , gender :: Gender
  , age    :: Int
  , lat    :: Float
  , lng    :: Float
  }

data Gender = Male | Female
```

Sadly, the JSON source we are communicating with is using JSON with Dutch
property names and values, so we cannot use Template Haskell to derive the
JSON mapping for us, like we would do with other JSON libraries. Neither do we
want to use Dutch names for our record selectors; nobody would be able to
understand our code anymore! Fortunately this isn't a problem with
JsonGrammar.

The first step is to have Template Haskell derive the constructor-destructor
pairs:

```
person         = $(deriveIsos ''Person)
(male, female) = $(deriveIsos ''Gender)
```

Then we write instances of the `Json` type class to define the mapping from/to
Json. The order in which the properties are listed matches that of the fields
in the datatype:

<pre>
instance Json Person where
  grammar = person . object
    ( prop "naam"
    . prop "geslacht"
    . prop "leeftijd"
    . prop "lat"
    . prop "lng"
    )

instance Json Gender where
  grammar =  male   . litJson "man"
          &lt;&gt; female . litJson "vrouw"
</pre>

The `.` operator is from `Control.Category`. The `<>` is just another name for
`mappend` from `Data.Monoid` and denotes choice.

That's all! We have just defined both `fromJson` and `toJson` in one simple
definition. Here's how you can use these grammars:

```
> let anna = Person "Anna" Female 36 53.0163038 5.1993053
> let Just annaJson = toJson anna annaJson
Object (fromList [("geslacht",String "vrouw"),("lat",Number
53.01630401611328),("leeftijd",Number 36),("lng",Number
5.199305534362793),("naam",String "Anna")])
> fromJson annaJson :: Maybe Person
Just (Person {name = "Anna", gender = Female, age = 36, lat = 53.016304,
lng = 5.1993055})
````````````````````````````````````````````````````````````````````````

## Show me the types!

The library is based on *partial isomorphisms*:

```
data Iso a b = Iso (a -> Maybe b) (b -> Maybe a)

instance Category Iso
instance Monoid (Iso a b)
```

A value of type `Iso a b` gives you a function that converts an `a` into a
`Maybe b`, and a function that converts a `b` into a `Maybe a`. This composes
beautifully as a `Category`. The `Monoid` instance denotes choice: first try
the left-hand conversion function, and if it fails, try the right-hand side.

A JSON `grammar` for some type `a` is nothing more than a value of type `Iso
Value a`, where `Value` is the type of a JSON AST from the
[aeson](http://hackage.haskell.org/package/aeson) package. That is, it's a
pair of conversion functions between JSON trees and your own datatype.
Building JSON grammars like the one above is about composing isomorphisms that
translate between intermediate types.

The isomorphisms `person`, `male` and `female` translate between constructors and their individual fields. For example:

```
person :: Iso (String, Gender, Int, Float, Float) Person
```

Converting from a constructor to its fields might fail, because the value that
is passed to the conversion function might be a different constructor of the
same datatype. This is why the `Monoid` instance is so useful: we can give
multiple grammars, usually one for each constructor, and they will be tried in
sequence. They are effectively *composable pattern matches*.

## Stack isomorphisms

There is a problem with encoding the fields of such a constructor as an
n-tuple: if we want to compose it with other isomorphisms that handle the
individual fields, we have to use complicated tuple projections to select the
fields that we're interested in. Basically we have unwrapped the fields from
one constructor only to wrap them in another one!

The solution is to use heterogenous stacks of values. They are reminiscent of
continuation-passing style, because in the way we use them they usually have a
polymorphic tail:

```
person :: Iso (String :- Gender :- Int :- Float :- Float :- t) (Person :- t)
```

Read `:-` as 'cons', but then for types instead of values. Its definition is simple:

```
data h :- t = h :- t
```

The polymorphic tail says that `person` doesn't care what's on the stack below
the two `Floats`; it will simply pass that part of the stack on to the
right-hand side. And vice versa, if we're working with the isomorphism in the
opposite direction.

Have you thought about what the types of `male` and `female` would be in the
non-stack versions of the isomorphisms? They don't have any fields; we would
have to leave the first type parameter of `Iso` empty somehow, for example by
choosing `()`. Stack isomorphisms have no such problem; we simply make the
first type argument the polymorphic tail on its own, without any values on
top:

```
male   :: Iso t (Gender :- t)
female :: Iso t (Gender :- t)
```

Stack isomorphisms compose beautifully using `.`, often without needing any
special projection functions. To get a feeling for it, try compiling the
example Json grammars and looking at the types of the individual components.

I lied when I wrote that grammars have type `Iso Value a`; they actually use
stacks themselves, too. Here is the true definition of the `Json` type class:

```
class Json a where
  grammar :: Iso (Value :- t) (a :- t)
```

## Different tree shapes

Let's take our Person example and make a small modification. We decide that
because (lat, lng)-pairs are so common together, we'd like to put them
together in their own datatype:

```
data Coords = Coords { lat :: Float, lng :: Float }
  deriving (Eq, Show)

data Person = Person
  { name     :: String
  , gender   :: Gender
  , age      :: Int
  , location :: Coords
  } deriving (Eq, Show)
```

However, in this example we have no control over the JSON format and cannot
change it to match our new structure. With JsonGrammar we can express mappings
where the nesting is not one-to-one:

```
instance Json Person where
  grammar = person . object
    ( prop "naam"
    . prop "geslacht"
    . prop "leeftijd"
    . coordsProps
    )

coordsProps :: Iso (Object :- t) (Object :- Coords :- t)
coordsProps = duck coords . prop "lat" . prop "lng"
```

Here `duck coords` wraps (or unwraps, depending on the direction) the two
matched `Float` properties in their own `Coords` constructor before continuing
matching the other properties in an object. Function `duck` is a combinator
that makes a grammar (`coords` in this case) work one element down the stack.
Here it makes sure the top values can remain `Object`s, which is needed by
`prop` to build/destruct JSON objects one property at a time.

What is important to note here is that not only can we express mappings with
different nestings, we can also capture this behaviour in its own grammar for
reuse. JsonGrammar allows this level of modularity in everything it does.

## History and related work

The ideas behind JsonGrammar go back a bit. They are based on
[Zwaluw](https://github.com/MedeaMelana/Zwaluw), a library that Sjoerd
Visscher and I worked on. The library aids in writing bidirectional
parsers/pretty-printers for type-safe URLs, also in a DRY manner. Zwaluw, too,
uses stacks to achieve a high level of modularity. In turn, Zwaluw was
inspired by [HoleyMonoid](http://hackage.haskell.org/package/HoleyMonoid),
which shows that the CPS-like manner of using polymorphic stack tails allows
combinators to build up a list of expected arguments for use in printf-like
functionality.

The `Iso` datatype comes from
[partial-isomorphisms](http://hackage.haskell.org/package/partial-isomorphisms)
and is described in more detail in [Invertible syntax descriptions: Unifying
parsing and pretty
printing](http://www.informatik.uni-marburg.de/~rendel/unparse/) by Tillmann
Rendel and Klaus Ostermann. They also use stacks (in the form of nested binary
tuples), but they are not using the trick with the polymorphic tail (yet?).

## Future work

Although JsonGrammar is usable, there is still work to be done:

* **Supporting new use cases**. JsonGrammar has not been used in the wild much yet. If you find any use cases that the library currently does not support, please let me know!
* **Benchmarking**. No performance testing or memory usage profiling has been done yet.
* **Improved error messages**. The `Maybe` return values indicate whenever conversion has failed, but never *how* it has failed. The `aeson` package gives nice error message when for example an expected property was not found. Such error reporting still has to be added to JsonGrammar.
* **Other experiments**. Perhaps a library can be written on top of JsonGrammar that allows grammars to be specified that also compile to JSON Schema. Or maybe grammars could compile to specialized JSON parsers, improving efficiency.

If you have any questions, comments, ideas or bug reports, feel to leave a
comment or [open a ticket on
GitHub](https://github.com/MedeaMelana/JsonGrammar/issues/new).
