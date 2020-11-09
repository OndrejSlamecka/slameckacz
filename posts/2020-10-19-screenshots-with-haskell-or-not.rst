---
title: Taking website screenshots with Haskell (or not)
---

.. role:: haskell(code)
    :language: haskell


`Hexfray<https://www.hexfray.com>`_'s map browser has recently received an improvement that lets
it show map previews. This article describes the road towards implementing the feature, including the
encountered bumps.


.. figure:: /images/2020-10-19-screenshots-with-haskell-or-not/54_preview.png
  :align: center
  :alt: Swampland by mahks

Before diving in let me shortly describe `Hexfray<https://www.hexfray.com>`_ as an online turn based strategy
game played between two players on maps created by the player community using a provided map editor. When a
creator publishes their map it is added to the list of available maps together with a preview like the one
above. The question of showing these previews is what concerns this article.


Picking a way to display previews
#################################

There are several options:

1. Generate images upfront by porting the PureScript client-side rendering to Haskell,
2. generate images upfront by generalising the PureScript rendering code to work on both client and server,
3. render maps on the client and scale them to the preview size,
4. generate images upfront by taking a screenshot.

Before analysing the options let me clarify that Hexfray is currently a side-project with just a few players
so I am firstly optimising for time spent coding (implementation and maintenance), then also for the amount of
fun and learning to be had, but not very much for runtime speed or scaling (although it's fun to consider how that would
be done!) Now to compare these options.

The maintenance implied by the first option seems unappealing.

The second option, generalising the rendering code to work on the server with nodejs and producing an image
directly would be my favourite for a production-grade system, however, I would have to create a PureScript
interface for the `nodejs canvas library<https://www.npmjs.com/package/canvas>`_ and another disadvantage is
that the rendering code would get more complicated and harder to develop further with two rendering targets.

A worry with the third option is that rendering many maps, some potentially huge, might be too demanding for
players' browsers. Also, this solution adds both front-end and back-end code that might need more tuning and
maintenance than images served by nginx.

Taking screenshots would reuse a lot of the existing code so it seems like a great choice for my criteria. I
just need the resulting image to have a transparent background to suit any background the preview might be
placed on. Ideally, the screenshot would already have a transparent background so I wouldn't have to calculate
the position of the "void" hexagons around the corners to cut them out.


Taking screenshots with code
############################

Easy, I thought, let's use Firefox. That won't work because Firefox `doesn't support a transparent
background<https://bugzilla.mozilla.org/show_bug.cgi?id=1549432>`_.

Let's use Chrome. I recalled there's `Selenium<https://www.selenium.dev/>`_ that lets developers
control a browser programmatically.  And good news, there's
`webdriver<https://hackage.haskell.org/package/webdriver-0.9.0.1>`_, a Haskell package implementing a client for
the protocol.

It's easy to spawn a browser and take a screenshot. First, install ChromeDriver, Selenium server (on Arch Linux
it's `selenium-server-standalone` in AUR), and run it :code:`java -jar
/usr/share/selenium-server/selenium-server-standalone.jar`, then


.. code-block:: haskell

  #!/usr/bin/env stack
  -- stack --resolver lts-16.15 script --package webdriver
  {-# LANGUAGE OverloadedStrings #-}

  import Test.WebDriver

  main = runSession (useBrowser chrome defaultConfig) $ do
    openPage "https://www.hexfray.com/map_screenshot/13"
    saveScreenshot "screenshot.png"
    -- note there's also `screenshot` producing ByteString
    closeSession


It works, we have :code:`screenshot.png`! Now to set the background to transparent.

Towards transparent background
##############################

:code:`Emulation.setDefaultBackgroundColorOverride` is the command to use and searching GitHub immediately
suggests `a neat little PHP
implementation<https://github.com/highscoresl/browser/blob/803a61b6cd8a394b6b9f34e3dd2852e2641193c6/src/Macros/ExecuteCommand.php#L15>`_
showing how to send this command. Let's just adapit it to Haskell:


.. code-block:: haskell

  #!/usr/bin/env stack
  -- stack --resolver lts-16.15 script --package "aeson aeson-qq http-types webdriver"
  {-# LANGUAGE OverloadedStrings #-}
  {-# LANGUAGE QuasiQuotes #-}

  import Data.Aeson (Value)
  import Data.Aeson.QQ
  import Network.HTTP.Types (methodPost)
  import Test.WebDriver
  import Test.WebDriver.JSON (noReturn)
  import Test.WebDriver.Commands.Internal (doSessCommand)

  main = runSession (useBrowser chrome defaultConfig) $ do
    openPage "https://www.hexfray.com/map_screenshot/13"
    let transparentBackgroundCmd = [aesonQQ|
          { "cmd": "Emulation.setDefaultBackgroundColorOverride"
          , "params": {"color": {"r": 0, "g": 0, "b": 0, "a": 0 } }
          }|]
    noReturn $ doSessCommand methodPost
                             "/chromium/send_command"
                             transparentBackgroundCmd
    saveScreenshot "screenshot.png"
    closeSession


That, however, doesn't work:

.. code-block:: log

  screenshot.hs: UnknownCommand "POST /session/60edae7069e3ef0446d62afbb7c3ef49/chromium/send_command_and_get_result\nBuild info: version: '3.141.59', revision: 'e82be7d358', time: '2018-11-14T08:25:53'\nSystem info: host: 'nuc', ip: '127.0.1.1', os.name: 'Linux', os.arch: 'amd64', os.version: '5.8.14-arch1-1', java.version: '14.0.2'\nDriver info: driver.version: unknown"


Similar code in Python works:

.. code-block:: python

    import json
    from selenium import webdriver
    from selenium.webdriver.common.desired_capabilities import DesiredCapabilities

    driver = webdriver.Remote(
       command_executor='http://127.0.0.1:4444/wd/hub',
       desired_capabilities=DesiredCapabilities.CHROME)
    # driver = webdriver.Chrome() -- easier, but wouldn't use the Selenium instance

    driver.get('https://www.hexfray.com/map_screenshot/13')
    url = driver.command_executor._url + f'/session/{driver.session_id}/chromium/send_command'
    body = { 'cmd': 'Emulation.setDefaultBackgroundColorOverride'
           , 'params': {'color': {'r': 0, 'g': 0, 'b': 0, 'a': 0}}
           }
    driver.command_executor._request('POST', url, json.dumps(body))
    driver.get_screenshot_as_file(r'screenshot.png')


It's unclear to me what causes the difference, inspecting Selenium logs (run with :code:`--debug`) doesn't show
any interesting difference between the Haskell and Python calls. I am not sure how to make this command work,
and if you do then please let me know. Let's think of a different way.

Chrome has a nice command line interface that even lets you specify
background transparency so we try that instead:

.. code-block:: haskell

    transparentChrome = chrome { chromeOptions = ["--default-background-color=0"] }
    main = runSession (useBrowser transparentChrome defaultConfig) $ do
      openPage "https://www.hexfray.com/map_screenshot/13"
      saveScreenshot "screenshot.png"
      closeSession

Unfortunately you can still see a white background in this screenshot (unless I changed my blog's design to have
a white background as well) *for some reason*.

.. figure:: /images/2020-10-19-screenshots-with-haskell-or-not/with_background.png
  :align: center
  :alt: Damned River by mahks, background still present


But since I am not the first one to `run into
problems<https://github.com/kallisti-dev/hs-webdriver/pull/144#issuecomment-443781149>`_ with Haskell :code:`webdriver`,
it had its last commit some time ago, and only officially supports Selenium 2 (I have version 3.141.59 -- apparently
they were aiming for Knuth's versioning scheme), it's worth considering other approaches than this fun
interface.

CLI approach to screenshots
###########################

So why not try using *just* the CLI to take a screenshot, skipping :code:`webdriver` altogether? Chrome does
have the necessary flags and there's the very handy `Turtle
library<https://hackage.haskell.org/package/turtle>`_ for communicating with the shell.

.. code-block:: haskell

    import qualified Turtle -- package turtle

    void $ Turtle.proc
      "google-chrome-stable"
      [ "--headless"
      , "--no-sandbox"
      , "--default-background-color=0"
      , "--disable-gpu"
      , "--screenshot", "https://www.hexfray.com/map_screenshot/18"
      ]
      mempty

.. figure:: /images/2020-10-19-screenshots-with-haskell-or-not/18_preview.png
  :align: center
  :alt: Training Camp by mahks, background transparent

And this works! The Haskell code just dumps a few strings to the shell which is the why of "(or not)" in the
title but at least this solution doesn't depend on an unmaintained library. Using shell is also easier in the
next step of processing the image since I already know some `imagemagick<https://imagemagick.org/>`_, unlike any
Haskell image editing library.

.. code-block:: haskell

    void $ Turtle.proc
      "mogrify"
      [ "-trim"
      , "-resize", "529x>"
      , "-background", "none"
      , "-gravity", "center", "-extent", "529x320>"
      , "screenshot.png"
      ]
      mempty


The disadvantage of this approach is mainly the lack of thread safety due to its reliance on the shell
environment.  For that, I create a worker that pulls its jobs out of an `STM
TChan<https://hackage.haskell.org/package/stm-2.5.0.0/docs/Control-Concurrent-STM-TChan.html>`_ and to be safe
also uses temporary directories.

.. code-block:: haskell

    -- pseudocode
    import qualified Control.Concurrent.STM as STM
    import qualified Control.Concurrent.STM.TChan as STM

    main = do
      ...
      queue <- STM.newTChanIO
      forkIO $ worker queue

    publishMap queue mapId = do
      ...
      Turtle.cp ... -- copy a placeholder before the preview is generated
      STM.atomically $ STM.writeTChan queue mapId

    worker queue = do
      mapId <- STM.atomically $ STM.readTChan queue
      Turtle.sh $ do
        tempDir <- Turtle.using $ Turtle.mktempdir "/tmp" [fmt|screenshot_{mapId}|]
        Turtle.cd tempDir
        -- Turtle.proc take screenshot
        -- Turtle.proc imagemagick
        -- Turtle.cp tempDir/screenshot.png ...
      worker queue


The code doesn't benefit much from Haskell's type system and its behaviour under failures should receive further
scrutiny, nevertheless, it is a working, convenient implementation for my prototype.
