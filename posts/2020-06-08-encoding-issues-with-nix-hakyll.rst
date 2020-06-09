---
title: Encoding issues with Hakyll & Nix
---

When setting up this blog I already had a good experience with `Hakyll <https://jaspervdj.be/hakyll/>`_ and
decided to use it again but now I wanted to try building it with Nix. Fortunately, there's `a great guide how to
do that <https://robertwpearce.com/hakyll-pt-6-pure-builds-with-nix.html>`_, except the compiled site
generator won't work with non-ASCII UTF-8 source files:

.. code-block:: log

    [ERROR] _cache/91cc3e6aa55338437b161dc62c21b154 for Hakyll.Core.Resource.Provider.MetadataCache/contact.markdown/body: Store.set: invalid argument (invalid byte sequence)

One option is to move out of the "small hut in the mountains of Kumano Kodō on Kii Hantō" (where you live
according to Hakyll's default ``contact.markdown``) to a plain ASCII place. The other is to
build the Hakyll site generator with support for more locales.

Being a total Nix rookie I mostly achieved that
by consulting with ``nprindle`` on the Nix Discord server
and by `copying a few lines from other Nix scripts
<https://github.com/Infinisil/all-hies/blob/a585074f75729de46669d859636ad5e9586a6a2a/update.nix>`_. The final
result is to replace the ``hakyll-nix-example`` portion of ``release.nix`` with

.. code-block:: nix

      hakyll-nix-example = (hpNew.callCabal2nix "hakyll-nix-example" ./. { }).overrideAttrs (old: {
            nativeBuildInputs = old.nativeBuildInputs or [] ++ [
              pkgs.makeWrapper
            ];
            postInstall = old.postInstall or "" + ''
              wrapProgram $out/bin/site \
                --set LANG "en_US.UTF-8" \
                --set LOCALE_ARCHIVE "${pkgs.glibcLocales}/lib/locale/locale-archive"
            '';
          });


``nprindle`` was also building his blog at the time and you might find other neat tricks like proper
cleaning of input to build (see ``clean``) in `his nix setup
<https://github.com/nprindle/blog/blob/8900bdc4dd9dd093e2bdf048d130a24babebc1bf/nix/overlay.nix>`_.
