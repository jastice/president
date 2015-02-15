module President(present,voidSlide) where
{-| 

President turns a `List (Signal Element)` into a presentation
that you can navigate with arrow keys. That's about it.

@docs present

-}

import Text
import Text (..)
import List (..)
import Signal (Signal, (<~), (~), constant, foldp)
import Graphics.Element (..)
import Graphics.Collage (..)
import Keyboard
import Debug
import Signal.Extra (applyMany)
import Window (dimensions)

type Control = Top | Bottom | Previous | Next | None

-- primary state update function
updateSlide: Int -> Control -> Int -> Int
updateSlide lastSlide control current = 
  let nextSlide = case control of
        Top -> 0
        Previous -> current - 1
        Next -> current + 1
        Bottom -> lastSlide
        None -> current
  in clamp 0 lastSlide nextSlide 

controlAction {x,y} = case (x,y) of
  (-1, _) -> Previous
  (1, _) -> Next
  (_, 1) -> Top
  (_, -1) -> Bottom
  _ -> None

chooseSlide: Int -> (List a -> a)
chooseSlide page = drop page >> head

positioning (w,h) slide = container w h middle slide 

voidSlide = fromString "..." |> Text.height 180 |> centered

-- signals

--displaySlide: Int -> Signal Element
displaySlide slides page = 
  case slides of
    [] -> voidSlide
    s :: t -> s

{-|
A signal that displays each of the signals in the list
as a slide. This may be navigated with the arrow keys.
-}
present: List (Signal Element) -> Signal Element
present slides = 
  let lastSlide = length slides - 1
      controls = controlAction <~ Keyboard.arrows
      currentSlide = foldp (updateSlide lastSlide) 0 controls
      theActualSlideSignal = applyMany (chooseSlide <~ currentSlide) slides
  in positioning <~ dimensions ~ theActualSlideSignal
