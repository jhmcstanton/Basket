# (Fruit) Basket
A simple, arrowized FRP library inspired by Yampa.

## Basket and Yampa
The basic Signal datatype (called SF in Yampa or a Behavior in other libraries) is where Basket differs from
Yampa the most. Yampa provides the datatype

```Haskell
newtype SF a b = SF (a -> b)
```

while Basket provides a type very similar to

```Haskell
newtype Signal s a b = Signal {
                         runSignal :: Time -> s -> a -> (b, s)
                     }
```

the types here have been modified to protect the innocent (for now), but this is the basic idea
behind Basket. There is a little history to how this type was derived

## (a -> b) becomes (Time -> a -> b)
FRP is often described as a pattern that aims to model systems that are explicitly dependent
on Time. To model this many libraries provide a Time behavior to pipe a Time value into various user defined
behaviors. This works alright, but depending on the implemention and the system being modeled this can become
tedious. To get around this, the original datatype in Basket simply wrapped around ```(Time -> a -> b)``` and 
pipes the current program time into the user defined Signals directly, and as it turns out this composes
with Arrow methods nicely.

## (Time -> a -> b) becomes (Time -> s -> a -> (b, s))
The other issue I bumped into with Yampa came up when modeling systems with large amounts of state. This is 
is something that Yampa actually does quite nicely. The methodology
used in Yampa allows the user to create an ```SF (a, s) (b, s)``` and turn it into ```SF a b``` by providing
an initial state. This actually works quite nicely and allows the user to compose arbitrary signals of varying
state simply by inlining a call to change ```SF (a, s) (b, s)``` to ```SF a b```. However, given enough
state in the system this can be tedious.

This is where ```(Time -> s -> a -> (b, s))``` comes in. Given that ```Signal``` is parameterized over 
```s a b```   then ``` Signal s ``` becomes an Arrow instance, and Signals that share state can compose together
via typical Arrow combinators. 

```Haskell
(#>) :: Signal s a b -> Signal s' b c -> Signal s'' a c
```
Again, the types have been modified for the innocent. This ultimately allows the top level Signal to represent
all of the state of the entire system explicitly in main and for state to be passed into the system only once.


... Add Signal diagrams for comparison 
