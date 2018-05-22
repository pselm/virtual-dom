[![Build Status](https://travis-ci.org/pselm/virtual-dom.svg?branch=master)](https://travis-ci.org/pselm/virtual-dom)

# pselm-virtual-dom

This is a Purescript implementation of Elm's virtual DOM library.

The goal is to implement the Elm API as literally as possible, to make it as
straightforward as possible to translate an Elm program into Purescript.
However, the actual code diverges from the Elm code a fair bit, and thus may
not be bug-for-bug compatible with the Elm implementation. Performance
characteristics may also differ -- I'm focused initially on constructing as
type-safe and maintainable an implementation as possible, and will then take a
look at optimizing performance.


