# Conway's Game of Life Using Comonad

This is a implementation of [Conway's Life](https://en.wikipedia.org/wiki/Conway's_Game_of_Life) in [purescript](http://purescript.org/)
using [Comonad](https://www.quora.com/What-is-a-Comonad-and-when-should-I-use-them).
The goal of this project was to implement generic comonadic data-structure useful for writing [cellural automatons](https://en.wikipedia.org/wiki/Cellular_automaton)
using [ad hoc polymorphism](https://en.wikipedia.org/wiki/Ad_hoc_polymorphism) and then build the Life on top.
This program also includes [FRP](https://en.wikipedia.org/wiki/Functional_reactive_programming) evolving simulation
with rendering to Canvas.

Instead of infinite grid the `Grid` data-structure is "repeating" itself infinitely.
Last and first cell in a zipper are acting as neighbors.
You can visualize this as a Grid drawn on a surface of a sphere if you wish.
The lowest level implementation is done in [`Data.Array.Zipper`](https://github.com/turboMaCk/pure-life-in-comonad/blob/master/src/Data/Array/Zipper.purs)
which implements basic zipper using native `Array`. This type implements `Show`, `Eq`, `Functor`, `Extend`, `Comonad`, `Foldable` and `Traversable` type classes.
[`Data.Grid`](https://github.com/turboMaCk/pure-life-in-comonad/blob/master/src/Data/Grid.purs) is build as `Zipper (Zipper a)`
and implements `Show`, `Eq`, `Functor`, `Extend` and `Comonad`. Life itself is then defined with [`Life`](https://github.com/turboMaCk/pure-life-in-comonad/blob/master/src/Life.purs)
module using `Grid Boolean` in straight forward way.

UI for defining initial state isn't part of this program.
Instead initial state is hardcoded to [Glider](https://en.wikipedia.org/wiki/Glider_(Conway's_Life)) - the [emblem of hackers](http://www.inkblurt.com/2007/05/16/the-glider-as-hacker-emblem/).
All UI implementation can be found in [`Main`](https://github.com/turboMaCk/pure-life-in-comonad/blob/master/src/Main.purs) module.

This was my first project in pure-script and therefore I'm not going to publish any part of this implementation as a library.
Anyway feel free to use any code from this repository if you want to (BSD 3 Clause).

## Installation

First make sure you have all dependencies installed. Assuming you have [node.js](https://nodejs.org/en/) on your machine run:

```bash
$ npm install -g bower pulp purescript
```

Then install dependencies of this project using bower:

```bash
$ bower install
```

and build project using `pulp`:

```bash
$ pulp --watch browserify -O --to dist/Main.js
```

Now you can open index.html in your browser.

## Credits and Resources

Many good resources published by good people were used as studying material for this project. To name at least few:

- [E. Millon's blog post](http://blog.emillon.org/posts/2012-10-18-comonadic-life.html)
- [S. Tay's blog post](https://samtay.github.io/posts/comonadic-game-of-life.html)
- [G. Roodt's (non comonadic) implementation](https://github.com/groodt/purescript-game-of-life)
- [T. Petricek's talk](https://www.youtube.com/watch?v=mqCsfYERzzE)
- good work done of purescript community

## License

BSD 3 Clause
