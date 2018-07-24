{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE PackageImports         #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

module Lib
    ( runFragmentDemo
    , FrameParams(..)
    , FragmentDemo(..)
    , module Graphics.GPipe
    ) where

import Control.Arrow
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import Control.Monad (unless)
import Control.Lens

data FrameParams = FrameParams
  { angle :: FFloat
  , resolution :: V2 FFloat
  }

data FragmentDemo = FragmentDemo
  { windowTitle :: String
  , fragmentEval :: FrameParams -> RasterizedInfo -> V3 FFloat
  }

runFragmentDemo :: FragmentDemo -> IO ()
runFragmentDemo (FragmentDemo {..}) = runContextT GLFW.defaultHandleConfig $ do
  win <- newWindow (WindowFormatColor RGB8) (GLFW.WindowConfig
    { configWidth        = 800
    , configHeight       = 450
    , configTitle        = windowTitle
    , configMonitor      = Nothing
    , configHints        = []
    , configSwapInterval = Nothing
    })

  -- Full-screen quad with two triangles
  vertexBuffer :: Buffer os (B4 Float) <- newBuffer 6
  writeBuffer vertexBuffer 0
    [ V4 (-1) ( 1) 0 1, V4 ( 1) (-1) 0 1, V4 ( 1) ( 1) 0 1
    , V4 (-1) ( 1) 0 1, V4 (-1) (-1) 0 1, V4 ( 1) (-1) 0 1 ]

  -- Storage for FramParams as (angle, resolution)
  ubuffer :: Buffer os (Uniform (B Float, B2 Float)) <- newBuffer 1

  shader <- compileShader $ do
    primitiveStream <- toPrimitiveStream (\(s,_) -> fmap (\x -> (x, ())) s)
    fragmentStream <- rasterize (\(_,viewPort) -> (FrontAndBack, viewPort, DepthRange 0 1)) primitiveStream
    (angle, resolution) <- getUniform (const (ubuffer, 0))
    drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True))) (withRasterizedInfo (\() ri -> fragmentEval (FrameParams angle resolution) ri) fragmentStream)

  -- Main program loop
  let
    go angle = do
      maybeWinSize <- GLFW.getWindowSize win
      let res@(w,h) = case maybeWinSize of { Just x -> x ; Nothing -> (0,0) }
      writeBuffer ubuffer 0 [(angle, V2 (fromIntegral w) (fromIntegral h))]
      render $ do
        clearWindowColor win (V3 0 0 0)
        vertexArray <- newVertexArray vertexBuffer
        shader (toPrimitiveArray TriangleList vertexArray, ViewPort (V2 0 0) (V2 w h))
      swapWindowBuffers win
      closeRequested <- GLFW.windowShouldClose win
      unless (closeRequested == Just True) $
        go $ (angle + 0.01667) `mod''` (2*pi)

  go 0
