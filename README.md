# solitaire.hs

```ruby
╭───────────── Solitaire ──────────────╮                                       
│╭──╮│╭──╮╭──╮╭──╮╭──╮╭──╮╭──╮╭──╮│╭  ╮│ Score:   0                            
││λ=││╭──╮╭──╮╭──╮╭──╮╭──╮╭──╮│7♠││    │                                       
│╰──╯│╭──╮╭──╮╭──╮╭──╮╭──╮│K♥│╰──╯│╰  ╯│ Moves:   0                            
│╭──╮│╭──╮╭──╮╭──╮╭──╮│J♣│╰──╯    │╭  ╮│                                       
││3♠││╭──╮╭──╮╭──╮│6♦│╰──╯        │    │ [New]                                 
│╭──╮│╭──╮╭──╮│9♣│╰──╯            │╰  ╯│                                       
││3♥││╭──╮│Q♠│╰──╯                │╭  ╮│ [Undo]                                
│╭──╮││4♠│╰──╯                    │    │                                       
││7♦││╰──╯                        │╰  ╯│                                       
│╰──╯│                            │╭  ╮│                                       
│    │                            │    │                                       
│    │                            │╰  ╯│                                       
│    │                            │    │                                       
╰──────────────────────────────────────╯                                       
```

## Essay
For more background on this project, [read the blog
post](https://jbuckland.com/blog/game-solitaire/) I wrote about developing it.

## Prerequisites
You'll need to install:
- [haskell](https://www.haskell.org/platform/), a standardized, general-purpose
  purely functional programming language, with non-strict semantics and strong
  static typing.
- [stack](https://docs.haskellstack.org/en/stable/README/), a cross-platform
  program for developing Haskell projects.

## Playing `solitaire`
You can clone this repo and use `stack` to build and run the executable like so:
```
git clone https://github.com/ambuc/solitaire.git
cd solitaire
stack build
stack exec solitaire-exe
```

## Links
- [brick](https://hackage.haskell.org/package/brick), a Haskell terminal user
  interface programming library.
  - [guide.rst](https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst),
    the Brick User Guide
  - [snake](https://samtay.github.io/articles/brick.html), a walkthrough of
    writing a snake game in Brick
- [microlens](https://hackage.haskell.org/package/microlens), a small extract of
  the larger [`Control.Lens`](http://hackage.haskell.org/package/lens) library,
  which implements functional references.
  - [`Control.Lens.Tutorial`](https://hackage.haskell.org/package/lens-tutorial/docs/Control-Lens-Tutorial.html),
    a great tutorial for understanding and using Haskell lenses or microlenses 

