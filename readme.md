[My personal website](https://www.slamecka.cz/),
built with [LaTeX.css](https://latex.now.sh/), [Hakyll](https://jaspervdj.be/hakyll/), and [Nix](https://nixos.org/).

    # To build the site generator
    nix-build --show-trace

    # To preview locally
    ./result/bin/site watch

    # To deploy to AWS S3 + CloudFront
    ./result/bin/site build && ../s5upload/s5upload.py


**Things I might add**

* Support for drafts https://www.jdreaver.com/posts/2014-06-22-math-programming-blog-hakyll.html
* Better linebreaks https://github.com/robertknight/tex-linebreak
