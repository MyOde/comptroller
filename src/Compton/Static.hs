module Compton.Static where

import           Data.Map.Strict (Map, fromList)

-- 43 options
c_shadow = "shadow"
c_noDndShadow = "no-dnd-shadow"
c_noDockShadow = "no-dock-shadow"
c_clearShadow = "clear-shadow"
c_shadowRadius = "shadow-radius"
c_shadowOffsetX = "shadow-offset-x"
c_shadowOffsetY  = "shadow-offset-y"
c_shadowOpacity = "shadow-opacity"
c_shadowRed = "shadow-red"
c_shadowGreen  = "shadow-green"
c_shadowBlue  = "shadow-blue"
c_shadowExclude  = "shadow-exclude"
c_shadowIgnoreShaped  = "shadow-ignore-shaped"
c_menuOpacity = "menu-opacity"
c_inactiveOpacity  = "inactive-opacity"
c_activeOpacity  = "active-opacity"
c_frameOpacity  = "frame-opacity"
c_inactiveOpacityOverride  = "inactive-opacity-override"
c_alphaStep  = "alpha-step"
c_inactiveDim  = "inactive-dim"
c_blurKern  = "blur-kern"
c_blurBackgroundExclude  = "blur-background-exclude"
c_fading  = "fading"
c_fadeInStep  = "fade-in-step"
c_fadeOutStep  = "fade-out-step"
c_fadeExclude  = "fade-exclude"
c_backend  = "backend"
c_markWmwinFocused  = "mark-wmwin-focused"
c_markOvredirFocused  = "mark-ovredir-focused"
c_detectRoundedCorners  = "detect-rounded-corners"
c_detectClientOpacity  = "detect-client-opacity"
c_refreshRate  = "refresh-rate"
c_vsync  = "vsync"
c_dbe  = "dbe"
c_paintOnOverlay  = "paint-on-overlay"
c_focusExclude  = "focus-exclude"
c_detectTransient  = "detect-transient"
c_detectClientLeader  = "detect-client-leader"
c_invertColorInclude  = "invert-color-include"
c_glxCopyFromFront  = "glx-copy-from-front"
c_glxSwapMethod  = "glx-swap-method"
c_wintypes  = "wintypes"
c_inactiveDimFixed  = "inactive-dim-fixed"
c_opacityRule  = "opacity-rule"

allEntries
  = booleanEntries
  ++ floatingEntries
  ++ arrayEntries
  ++ textualEntries
  ++ [ c_opacityRule
     , c_wintypes
     ]

-- 16
booleanEntries =
  [ c_shadow
  , c_noDndShadow
  , c_noDockShadow
  , c_clearShadow
  , c_inactiveOpacityOverride
  , c_fading
  , c_detectRoundedCorners
  , c_detectClientOpacity
  , c_inactiveDimFixed
  , c_shadowIgnoreShaped
  , c_markWmwinFocused
  , c_markOvredirFocused
  , c_dbe
  , c_paintOnOverlay
  , c_detectTransient
  , c_detectClientLeader
  , c_glxCopyFromFront
  ]
-- 15
floatingEntries =
  [ c_shadowRadius
  , c_shadowOffsetX
  , c_shadowOffsetY
  , c_shadowOpacity
  , c_shadowRed
  , c_shadowGreen
  , c_shadowBlue
  , c_menuOpacity
  , c_inactiveOpacity
  , c_activeOpacity
  , c_frameOpacity
  , c_alphaStep
  , c_inactiveDim
  , c_fadeInStep
  , c_fadeOutStep
  , c_refreshRate
  ]

-- 5
arrayEntries =
  [ c_shadowExclude
  , c_blurBackgroundExclude
  , c_fadeExclude
  , c_focusExclude
  , c_invertColorInclude
  ]

-- 4
textualEntries =
  [ c_blurKern
  , c_backend
  , c_vsync
  , c_glxSwapMethod
  ]

enumValues :: Map String [String]
enumValues = fromList
  -- TODO blurkern can also have a dynamic user input value
  [ ( c_blurKern
    , [ "3x3box"
      , "5x5box"
      , "7x7box"
      , "3x3gaussian"
      , "5x5gaussian"
      , "7x7gaussian"
      , "9x9gaussian"
      , "11x11gaussian"
      ]
    )
  , ( c_backend
    , [ "xrender"
      , "glx"
      , "xr_glx_hybrid"
      ]
    )
  , ( c_vsync
    , [ "none"
      , "drm"
      , "opengl"
      , "opengl-oml"
      , "opengl-swc"
      , "opengl-mswc"
      ]
    )
  , ( c_glxSwapMethod
    , [ "undefined"
      , "exchange"
      , "copy"
      , "3"
      , "4"
      , "5"
      , "6"
      , "buffer-age"
      ]
    )
  ]
