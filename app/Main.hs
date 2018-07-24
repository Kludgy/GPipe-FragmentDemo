{-# LANGUAGE RecordWildCards #-}

module Main where

import Lib
import Control.Lens

main :: IO ()
main = runFragmentDemo creation

creation :: FragmentDemo
creation = FragmentDemo "'Creation' by silexars, transcribed from the Shadertoy gallery" go where
  go (FrameParams {..}) (RasterizedInfo {..}) = over each (gamma . val) (pure angle + V3 0 1 2 ^* 0.07)
    where
      gamma = (**2)
      p     = (rasterizedFragCoord^._xy / resolution - 0.5) * V2 (resolution^._x / resolution^._y) 1
      l     = norm p
      uv t  = p + (p ^/ l) ^* (1 + cos t) ^* abs (sin $ 9*l - 2*t)
      val t = 0.01 / norm (fract' (0.5 + uv t) - 0.5) / l

{-
// https://www.shadertoy.com/view/MtlBDf

void mainImage( out vec4 fragColor, in vec2 fragCoord ){
    vec2 p = fragCoord/iResolution.xy - 0.5; // -0.5 -- 0.5
    p.x*=iResolution.x/iResolution.y;
	float l = length(p);
    float t = iTime;
    
	vec3 c;
    for(int i=0;i<=2;i++) {
		t+=.07;               
        vec2 a = p + p/l * (1.+cos(t)) * abs(sin(l*9.-t*2.));
		c[i]=.01/length( fract(0.5+a)-0.5 );
    }
	fragColor=vec4(c/l,0.);
}
-}
