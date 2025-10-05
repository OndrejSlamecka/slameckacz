[My personal website](https://www.slamecka.cz/),
built with [LaTeX.css](https://latex.now.sh/), [Hakyll](https://jaspervdj.be/hakyll/), and [Nix](https://nixos.org/).

    # Dev process
    nix develop

    cabal run site clean
    cabal run site build
    cabal run site rebuild
    cabal run site watch

    git subtree push --prefix _site origin gh-pages

**Things I might add**

* Support for drafts https://www.jdreaver.com/posts/2014-06-22-math-programming-blog-hakyll.html
* Better linebreaks https://github.com/robertknight/tex-linebreak
* Use et-book, a font more suitable for screens [but only when it supports all necessary characters](https://github.com/edwardtufte/et-book/issues). Also consider Garvis by James Hultquist-Todd.
