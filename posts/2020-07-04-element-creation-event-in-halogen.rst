---
title: Element creation event in Halogen
---

When you render an ``input`` HTML element you might want to focus it (e.g. when it was a single field the user
asked to reveal). If you're using using `Purescript Halogen
<https://github.com/purescript-halogen/purescript-halogen>`_ you have to go a bit beyond the basic component
lifecycle and bind an action to be evaluated at the moment the element is created. An example using Halogen 5
follows.

.. code-block:: haskell
    :class: numberSource

    import H.Query.Input as H.Query.Input
    import Halogen.HTML as HH
    import Halogen.HTML.Properties as HP
    import Halogen.VDom.DOM.Prop as H.DOM
    import Web.DOM as Web.DOM
    import Web.HTML.HTMLElement as HTMLElement

    data Action = InputCreated Web.DOM.Element

    render
      :: forall cs m
       . MonadAff m
      => State
      -> H.ComponentHTML Action cs m
    render state =
      HH.input [ HP.IProp (H.DOM.Ref inputRefChange) ]
        where
          inputRefChange (H.DOM.Created element)
            = Just (H.Query.Input.Action (InputCreated element))
          inputRefChange (H.DOM.Removed _ )
            = Nothing

    handleAction
      :: forall cs o m
       . MonadAff m
      => Action
      -> H.HalogenM State Action cs o m Unit
    handleAction (InputRendered element) =
      maybe
        (pure unit)
        (H.liftEffect <<< HTMLElement.focus)
        (HTMLElement.fromElement element)
