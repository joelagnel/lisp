;;; OpenGL.el --- Minor mode commands for editing OpenGL C/C++ code.
;;
;; Copyright (c) 1994-2000 CENA, Heddy Boubaker, Sriram Karra
;; 
;; Authors: Heddy Boubaker <boubaker@dgac.fr>
;;          Sriram Karra <karra@cs.utah.edu>
;; Maintainer : Sriram Karra <karra@cs.utah.edu>
;; version : 1.0
;; Last modified : April 24, 2000.
;; Keywords : OpenGL, minor-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.
;;
;; Latest version of this program can be found at
;; http://www.cs.utah.edu/~karra/code/index.html

;;; Change Log:

;; April 24 2000
;; ** changed the to-do list to better reflect what I have in mind for
;;    future of OpenGL.el
;;
;; March 12 2000 Sriram Karra. 
;; ** added `OpenGL-matrix-push-pop' to insert a glPushMatrix() /
;;    glPopMatrixPop block.

;; ** added `OpenGL-enable-disable' to insert a glEnable(...) / glDisable()
;;    block these have also been added the menu bar.

;; ** added `OpenGL-bind-keys' to bind the most useful interactive functions
;;    to some default keys.  Now, you just have to add this to your
;;    OpenGL-minor-mode-hook

;; ** `OpenGL-*' commands that insert frame now can detect if mark is
;;    active, and inserts the active region into the frame.

;; ** fixed a bug in `OpenGL-complete-symbol' so that it actually
;;    inserts the symbol ;-)

;; ** changed the misleading variable name `OpenGL-version' to what is
;; IMHO a more correct sounding `OpenGL-minor-mode-version'.

;; ** changed `OpenGL-minor-mode' so that the current major mode is not
;;    checked if we only want to turn off OpenGL-minor-mode.  this kind of
;;    makes sense in the following seq.  I load c-mode, then load the
;;    OpenGL-minor-mode, then manually shift to lisp-mode.  Now I cannot
;;    turn off OpenGL-minor-mode because "there is no OpenGL-minor-mode for
;;    Lisp-mode" !!.  After the change, this problem is eliminated.

;; ** changed the structure of all the `OpenGL-*' funcs that insert an
;;    editing frame by introducing a local-only function
;;    `OpenGL-generic-insert' that inserts some text at point by
;;    taking these as arguments.

;; Installation:
;; =============
;;
;;  Byte compile this file (*) somewhere in your `load-path' and add in
;;  your .emacs:
;;   ;; For editing C files in OpenGL minor mode
;; (add-hook 'c-mode-hook
;;  	  '(lambda ()
;;  	     (cond ((string-match "/\\([Oo]pen\\)?[Gg][Ll]/"
;;  				  (buffer-file-name))
;;  		    (require 'OpenGL)
;; 		    (OpenGL-minor-mode 1)
;;		    (OpenGL-setup-keys)))))

;;   Or:
;;(autoload 'OpenGL-minor-mode "OpenGL" "OpenGL editing utilities." t)
;;(add-hook 'OpenGL-minor-mode-hook 'OpenGL-setup-keys)
;;   And load OpenGL-minor-mode as you wish.
;;

;;; Bug Reports:
;;  ===========
;;
;;   To report a bug please use function `OpenGL-submit-bug-report'
;;   Please note that this bug-report facility uses Barry Warsaw's
;;   reporter.el which is part of GNU Emacs v19 and bundled with many
;;   other packages.  If needed, you can obtain a copy of reporter.el
;;   at the elisp-archive.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to-do list.
;;
;; 1.  Allow insertion of template OpenGL programs containing empty
;; call backs and such
;;
;; 2. The list of constants and functions is out of date.  Heddy says
;; they correspond "to one of the first versions of OpenGL".  The one
;; sure way of keeping it uptodate is to write a function that will
;; extract them from the standard header files gl.h glu.h and glut.h
;; and any other OpenGL related headers the user would like to read
;; constants from.  Heddy suggested a perl script to be shipped along
;; with this file that would do the job, but I personally prefer a
;; elisp function so that the whole package would stay in one piece if
;; nothing else.
;;
;; 3.  Enable reverse of the block insertion functions so that one
;; could get rid of a glEnable()/glDisbale() satements sorrounding a
;; statement block : either region based or the least enclosing gl*
;; block.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Code:

;;; Dependencies ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)
(require 'backquote)


;;; User variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;###autoload
(defvar OpenGL-minor-mode-hook nil
  "*Hooks called when starting OpenGL minor mode.")


;;;###autoload
(defvar OpenGL-add-menu-before "Buffers"
  "*Menu name where to add OpenGL menu before.  For XEmacs only.  If
nil OpenGL will not add it's menu to the menubar.")


;;; Internal variables/constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar OpenGL-minor-mode nil
  "Non-nil means using OpenGL minor mode.
Use `OpenGL-minor-mode' to toggle this var.

See function `OpenGL-minor-mode' for complete documentation.")


(defvar OpenGL-minor-mode-map nil
  "Mode map for OpenGL minor mode")


(defvar OpenGL-menu-bar nil
  "Menu bar for OpenGL minor mode. (xemacs only)")


(defconst OpenGL-minor-mode-version "1.0"
  "Current version of OpenGL minor mode.
Not the OpenGL version ...")


(defconst OpenGL-help-address "karra@cs.utah.edu"
  "E-Mail address of OpenGL maintainer.")


(defconst OpenGL-symbols
  (list
   ;;
   ;; GL/gl.h Constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   ;; AccumOp
   '( "GL_ACCUM"                         . (constant "GLenum"))
   '( "GL_LOAD"                          . (constant "GLenum"))
   '( "GL_RETURN"                        . (constant "GLenum"))
   '( "GL_MULT"                          . (constant "GLenum"))
   '( "GL_ADD"                           . (constant "GLenum"))
   ;; AlphaFunction
   '( "GL_NEVER"                         . (constant "GLenum"))
   '( "GL_LESS"                          . (constant "GLenum"))
   '( "GL_EQUAL"                         . (constant "GLenum"))
   '( "GL_LEQUAL"                        . (constant "GLenum"))
   '( "GL_GREATER"                       . (constant "GLenum"))
   '( "GL_NOTEQUAL"                      . (constant "GLenum"))
   '( "GL_GEQUAL"                        . (constant "GLenum"))
   '( "GL_ALWAYS"                        . (constant "GLenum"))
   ;; AttribMask
   '( "GL_CURRENT_BIT"                   . (constant "GLbitfield"))
   '( "GL_POINT_BIT"                     . (constant "GLbitfield"))
   '( "GL_LINE_BIT"                      . (constant "GLbitfield"))
   '( "GL_POLYGON_BIT"                   . (constant "GLbitfield"))
   '( "GL_POLYGON_STIPPLE_BIT"           . (constant "GLbitfield"))
   '( "GL_PIXEL_MODE_BIT"                . (constant "GLbitfield"))
   '( "GL_LIGHTING_BIT"                  . (constant "GLbitfield"))
   '( "GL_FOG_BIT"                       . (constant "GLbitfield"))
   '( "GL_DEPTH_BUFFER_BIT"              . (constant "GLbitfield"))
   '( "GL_ACCUM_BUFFER_BIT"              . (constant "GLbitfield"))
   '( "GL_STENCIL_BUFFER_BIT"            . (constant "GLbitfield"))
   '( "GL_VIEWPORT_BIT"                  . (constant "GLbitfield"))
   '( "GL_TRANSFORM_BIT"                 . (constant "GLbitfield"))
   '( "GL_ENABLE_BIT"                    . (constant "GLbitfield"))
   '( "GL_COLOR_BUFFER_BIT"              . (constant "GLbitfield"))
   '( "GL_HINT_BIT"                      . (constant "GLbitfield"))
   '( "GL_EVAL_BIT"                      . (constant "GLbitfield"))
   '( "GL_LIST_BIT"                      . (constant "GLbitfield"))
   '( "GL_TEXTURE_BIT"                   . (constant "GLbitfield"))
   '( "GL_SCISSOR_BIT"                   . (constant "GLbitfield"))
   '( "GL_ALL_ATTRIB_BITS"               . (constant "GLbitfield"))
   ;; BeginMode
   '( "GL_POINTS"                        . (constant "GLenum"))
   '( "GL_LINES"                         . (constant "GLenum"))
   '( "GL_LINE_LOOP"                     . (constant "GLenum"))
   '( "GL_LINE_STRIP"                    . (constant "GLenum"))
   '( "GL_TRIANGLES"                     . (constant "GLenum"))
   '( "GL_TRIANGLE_STRIP"                . (constant "GLenum"))
   '( "GL_TRIANGLE_FAN"                  . (constant "GLenum"))
   '( "GL_QUADS"                         . (constant "GLenum"))
   '( "GL_QUAD_STRIP"                    . (constant "GLenum"))
   '( "GL_POLYGON"                       . (constant "GLenum"))
   ;; BlendingFactorDest
   '( "GL_ZERO"                          . (constant "GLenum"))
   '( "GL_ONE"                           . (constant "GLenum"))
   '( "GL_SRC_COLOR"                     . (constant "GLenum"))
   '( "GL_ONE_MINUS_SRC_COLOR"           . (constant "GLenum"))
   '( "GL_SRC_ALPHA"                     . (constant "GLenum"))
   '( "GL_ONE_MINUS_SRC_ALPHA"           . (constant "GLenum"))
   '( "GL_DST_ALPHA"                     . (constant "GLenum"))
   '( "GL_ONE_MINUS_DST_ALPHA"           . (constant "GLenum"))
   ;; BlendingFactorSrc
   '( "GL_DST_COLOR"                     . (constant "GLenum"))
   '( "GL_ONE_MINUS_DST_COLOR"           . (constant "GLenum"))
   '( "GL_SRC_ALPHA_SATURATE"            . (constant "GLenum"))
   ;; Boolean
   '( "GL_TRUE"                          . (constant "GLboolean"))
   '( "GL_FALSE"                         . (constant "GLboolean"))
   ;; ClearBufferMask
   ;; ClipPlaneName
   '( "GL_CLIP_PLANE0"                   . (constant "GLenum"))
   '( "GL_CLIP_PLANE1"                   . (constant "GLenum"))
   '( "GL_CLIP_PLANE2"                   . (constant "GLenum"))
   '( "GL_CLIP_PLANE3"                   . (constant "GLenum"))
   '( "GL_CLIP_PLANE4"                   . (constant "GLenum"))
   '( "GL_CLIP_PLANE5"                   . (constant "GLenum"))
   ;; ColorMaterialFace
   ;; ColorMaterialParameter
   ;; CullFaceMode
   ;; DepthFunction
   ;; DrawBufferMode
   '( "GL_NONE"                          . (constant "GLenum"))
   '( "GL_FRONT_LEFT"                    . (constant "GLenum"))
   '( "GL_FRONT_RIGHT"                   . (constant "GLenum"))
   '( "GL_BACK_LEFT"                     . (constant "GLenum"))
   '( "GL_BACK_RIGHT"                    . (constant "GLenum"))
   '( "GL_FRONT"                         . (constant "GLenum"))
   '( "GL_BACK"                          . (constant "GLenum"))
   '( "GL_LEFT"                          . (constant "GLenum"))
   '( "GL_RIGHT"                         . (constant "GLenum"))
   '( "GL_FRONT_AND_BACK"                . (constant "GLenum"))
   '( "GL_AUX0"                          . (constant "GLenum"))
   '( "GL_AUX1"                          . (constant "GLenum"))
   '( "GL_AUX2"                          . (constant "GLenum"))
   '( "GL_AUX3"                          . (constant "GLenum"))
   ;; Enable
   ;; ErrorCode
   '( "GL_NO_ERROR"                      . (constant "GLenum"))
   '( "GL_INVALID_ENUM"                  . (constant "GLenum"))
   '( "GL_INVALID_VALUE"                 . (constant "GLenum"))
   '( "GL_INVALID_OPERATION"             . (constant "GLenum"))
   '( "GL_STACK_OVERFLOW"                . (constant "GLenum"))
   '( "GL_STACK_UNDERFLOW"               . (constant "GLenum"))
   '( "GL_OUT_OF_MEMORY"                 . (constant "GLenum"))
   ;; FeedBackMode
   '( "GL_2D"                            . (constant "GLenum"))
   '( "GL_3D"                            . (constant "GLenum"))
   '( "GL_3D_COLOR"                      . (constant "GLenum"))
   '( "GL_3D_COLOR_TEXTURE"              . (constant "GLenum"))
   '( "GL_4D_COLOR_TEXTURE"              . (constant "GLenum"))
   ;; FeedBackToken
   '( "GL_PASS_THROUGH_TOKEN"            . (constant "GLenum"))
   '( "GL_POINT_TOKEN"                   . (constant "GLenum"))
   '( "GL_LINE_TOKEN"                    . (constant "GLenum"))
   '( "GL_POLYGON_TOKEN"                 . (constant "GLenum"))
   '( "GL_BITMAP_TOKEN"                  . (constant "GLenum"))
   '( "GL_DRAW_PIXEL_TOKEN"              . (constant "GLenum"))
   '( "GL_COPY_PIXEL_TOKEN"              . (constant "GLenum"))
   '( "GL_LINE_RESET_TOKEN"              . (constant "GLenum"))
   ;; FogMode
   '( "GL_EXP"                           . (constant "GLenum"))
   '( "GL_EXP2"                          . (constant "GLenum"))
   ;; FogParameter
   ;; FrontFaceDirection
   '( "GL_CW"                            . (constant "GLenum"))
   '( "GL_CCW"                           . (constant "GLenum"))
   ;; GetMapTarget
   '( "GL_COEFF"                         . (constant "GLenum"))
   '( "GL_ORDER"                         . (constant "GLenum"))
   '( "GL_DOMAIN"                        . (constant "GLenum"))
   ;; GetPixelMap
   ;; GetTarget
   '( "GL_CURRENT_COLOR"                 . (constant "GLenum"))
   '( "GL_CURRENT_INDEX"                 . (constant "GLenum"))
   '( "GL_CURRENT_NORMAL"                . (constant "GLenum"))
   '( "GL_CURRENT_TEXTURE_COORDS"        . (constant "GLenum"))
   '( "GL_CURRENT_RASTER_COLOR"          . (constant "GLenum"))
   '( "GL_CURRENT_RASTER_INDEX"          . (constant "GLenum"))
   '( "GL_CURRENT_RASTER_TEXTURE_COORDS" . (constant "GLenum"))
   '( "GL_CURRENT_RASTER_POSITION"       . (constant "GLenum"))
   '( "GL_CURRENT_RASTER_POSITION_VALID" . (constant "GLenum"))
   '( "GL_CURRENT_RASTER_DISTANCE"       . (constant "GLenum"))
   '( "GL_POINT_SMOOTH"                  . (constant "GLenum"))
   '( "GL_POINT_SIZE"                    . (constant "GLenum"))
   '( "GL_POINT_SIZE_RANGE"              . (constant "GLenum"))
   '( "GL_POINT_SIZE_GRANULARITY"        . (constant "GLenum"))
   '( "GL_LINE_SMOOTH"                   . (constant "GLenum"))
   '( "GL_LINE_WIDTH"                    . (constant "GLenum"))
   '( "GL_LINE_WIDTH_RANGE"              . (constant "GLenum"))
   '( "GL_LINE_WIDTH_GRANULARITY"        . (constant "GLenum"))
   '( "GL_LINE_STIPPLE"                  . (constant "GLenum"))
   '( "GL_LINE_STIPPLE_PATTERN"          . (constant "GLenum"))
   '( "GL_LINE_STIPPLE_REPEAT"           . (constant "GLenum"))
   '( "GL_LIST_MODE"                     . (constant "GLenum"))
   '( "GL_MAX_LIST_NESTING"              . (constant "GLenum"))
   '( "GL_LIST_BASE"                     . (constant "GLenum"))
   '( "GL_LIST_INDEX"                    . (constant "GLenum"))
   '( "GL_POLYGON_MODE"                  . (constant "GLenum"))
   '( "GL_POLYGON_SMOOTH"                . (constant "GLenum"))
   '( "GL_POLYGON_STIPPLE"               . (constant "GLenum"))
   '( "GL_EDGE_FLAG"                     . (constant "GLenum"))
   '( "GL_CULL_FACE"                     . (constant "GLenum"))
   '( "GL_CULL_FACE_MODE"                . (constant "GLenum"))
   '( "GL_FRONT_FACE"                    . (constant "GLenum"))
   '( "GL_LIGHTING"                      . (constant "GLenum"))
   '( "GL_LIGHT_MODEL_LOCAL_VIEWER"      . (constant "GLenum"))
   '( "GL_LIGHT_MODEL_TWO_SIDE"          . (constant "GLenum"))
   '( "GL_LIGHT_MODEL_AMBIENT"           . (constant "GLenum"))
   '( "GL_SHADE_MODEL"                   . (constant "GLenum"))
   '( "GL_COLOR_MATERIAL_FACE"           . (constant "GLenum"))
   '( "GL_COLOR_MATERIAL_PARAMETER"      . (constant "GLenum"))
   '( "GL_COLOR_MATERIAL"                . (constant "GLenum"))
   '( "GL_FOG"                           . (constant "GLenum"))
   '( "GL_FOG_INDEX"                     . (constant "GLenum"))
   '( "GL_FOG_DENSITY"                   . (constant "GLenum"))
   '( "GL_FOG_START"                     . (constant "GLenum"))
   '( "GL_FOG_END"                       . (constant "GLenum"))
   '( "GL_FOG_MODE"                      . (constant "GLenum"))
   '( "GL_FOG_COLOR"                     . (constant "GLenum"))
   '( "GL_DEPTH_RANGE"                   . (constant "GLenum"))
   '( "GL_DEPTH_TEST"                    . (constant "GLenum"))
   '( "GL_DEPTH_WRITEMASK"               . (constant "GLenum"))
   '( "GL_DEPTH_CLEAR_VALUE"             . (constant "GLenum"))
   '( "GL_DEPTH_FUNC"                    . (constant "GLenum"))
   '( "GL_ACCUM_CLEAR_VALUE"             . (constant "GLenum"))
   '( "GL_STENCIL_TEST"                  . (constant "GLenum"))
   '( "GL_STENCIL_CLEAR_VALUE"           . (constant "GLenum"))
   '( "GL_STENCIL_FUNC"                  . (constant "GLenum"))
   '( "GL_STENCIL_VALUE_MASK"            . (constant "GLenum"))
   '( "GL_STENCIL_FAIL"                  . (constant "GLenum"))
   '( "GL_STENCIL_PASS_DEPTH_FAIL"       . (constant "GLenum"))
   '( "GL_STENCIL_PASS_DEPTH_PASS"       . (constant "GLenum"))
   '( "GL_STENCIL_REF"                   . (constant "GLenum"))
   '( "GL_STENCIL_WRITEMASK"             . (constant "GLenum"))
   '( "GL_MATRIX_MODE"                   . (constant "GLenum"))
   '( "GL_NORMALIZE"                     . (constant "GLenum"))
   '( "GL_VIEWPORT"                      . (constant "GLenum"))
   '( "GL_MODELVIEW_STACK_DEPTH"         . (constant "GLenum"))
   '( "GL_PROJECTION_STACK_DEPTH"        . (constant "GLenum"))
   '( "GL_TEXTURE_STACK_DEPTH"           . (constant "GLenum"))
   '( "GL_MODELVIEW_MATRIX"              . (constant "GLenum"))
   '( "GL_PROJECTION_MATRIX"             . (constant "GLenum"))
   '( "GL_TEXTURE_MATRIX"                . (constant "GLenum"))
   '( "GL_ATTRIB_STACK_DEPTH"            . (constant "GLenum"))
   '( "GL_ALPHA_TEST"                    . (constant "GLenum"))
   '( "GL_ALPHA_TEST_FUNC"               . (constant "GLenum"))
   '( "GL_ALPHA_TEST_REF"                . (constant "GLenum"))
   '( "GL_DITHER"                        . (constant "GLenum"))
   '( "GL_BLEND_DST"                     . (constant "GLenum"))
   '( "GL_BLEND_SRC"                     . (constant "GLenum"))
   '( "GL_BLEND"                         . (constant "GLenum"))
   '( "GL_LOGIC_OP_MODE"                 . (constant "GLenum"))
   '( "GL_LOGIC_OP"                      . (constant "GLenum"))
   '( "GL_AUX_BUFFERS"                   . (constant "GLenum"))
   '( "GL_DRAW_BUFFER"                   . (constant "GLenum"))
   '( "GL_READ_BUFFER"                   . (constant "GLenum"))
   '( "GL_SCISSOR_BOX"                   . (constant "GLenum"))
   '( "GL_SCISSOR_TEST"                  . (constant "GLenum"))
   '( "GL_INDEX_CLEAR_VALUE"             . (constant "GLenum"))
   '( "GL_INDEX_WRITEMASK"               . (constant "GLenum"))
   '( "GL_COLOR_CLEAR_VALUE"             . (constant "GLenum"))
   '( "GL_COLOR_WRITEMASK"               . (constant "GLenum"))
   '( "GL_INDEX_MODE"                    . (constant "GLenum"))
   '( "GL_RGBA_MODE"                     . (constant "GLenum"))
   '( "GL_DOUBLEBUFFER"                  . (constant "GLenum"))
   '( "GL_STEREO"                        . (constant "GLenum"))
   '( "GL_RENDER_MODE"                   . (constant "GLenum"))
   '( "GL_PERSPECTIVE_CORRECTION_HINT"   . (constant "GLenum"))
   '( "GL_POINT_SMOOTH_HINT"             . (constant "GLenum"))
   '( "GL_LINE_SMOOTH_HINT"              . (constant "GLenum"))
   '( "GL_POLYGON_SMOOTH_HINT"           . (constant "GLenum"))
   '( "GL_FOG_HINT"                      . (constant "GLenum"))
   '( "GL_TEXTURE_GEN_S"                 . (constant "GLenum"))
   '( "GL_TEXTURE_GEN_T"                 . (constant "GLenum"))
   '( "GL_TEXTURE_GEN_R"                 . (constant "GLenum"))
   '( "GL_TEXTURE_GEN_Q"                 . (constant "GLenum"))
   '( "GL_PIXEL_MAP_I_TO_I"              . (constant "GLenum"))
   '( "GL_PIXEL_MAP_S_TO_S"              . (constant "GLenum"))
   '( "GL_PIXEL_MAP_I_TO_R"              . (constant "GLenum"))
   '( "GL_PIXEL_MAP_I_TO_G"              . (constant "GLenum"))
   '( "GL_PIXEL_MAP_I_TO_B"              . (constant "GLenum"))
   '( "GL_PIXEL_MAP_I_TO_A"              . (constant "GLenum"))
   '( "GL_PIXEL_MAP_R_TO_R"              . (constant "GLenum"))
   '( "GL_PIXEL_MAP_G_TO_G"              . (constant "GLenum"))
   '( "GL_PIXEL_MAP_B_TO_B"              . (constant "GLenum"))
   '( "GL_PIXEL_MAP_A_TO_A"              . (constant "GLenum"))
   '( "GL_PIXEL_MAP_I_TO_I_SIZE"         . (constant "GLenum"))
   '( "GL_PIXEL_MAP_S_TO_S_SIZE"         . (constant "GLenum"))
   '( "GL_PIXEL_MAP_I_TO_R_SIZE"         . (constant "GLenum"))
   '( "GL_PIXEL_MAP_I_TO_G_SIZE"         . (constant "GLenum"))
   '( "GL_PIXEL_MAP_I_TO_B_SIZE"         . (constant "GLenum"))
   '( "GL_PIXEL_MAP_I_TO_A_SIZE"         . (constant "GLenum"))
   '( "GL_PIXEL_MAP_R_TO_R_SIZE"         . (constant "GLenum"))
   '( "GL_PIXEL_MAP_G_TO_G_SIZE"         . (constant "GLenum"))
   '( "GL_PIXEL_MAP_B_TO_B_SIZE"         . (constant "GLenum"))
   '( "GL_PIXEL_MAP_A_TO_A_SIZE"         . (constant "GLenum"))
   '( "GL_UNPACK_SWAP_BYTES"             . (constant "GLenum"))
   '( "GL_UNPACK_LSB_FIRST"              . (constant "GLenum"))
   '( "GL_UNPACK_ROW_LENGTH"             . (constant "GLenum"))
   '( "GL_UNPACK_SKIP_ROWS"              . (constant "GLenum"))
   '( "GL_UNPACK_SKIP_PIXELS"            . (constant "GLenum"))
   '( "GL_UNPACK_ALIGNMENT"              . (constant "GLenum"))
   '( "GL_PACK_SWAP_BYTES"               . (constant "GLenum"))
   '( "GL_UNPACK_LSB_FIRST"              . (constant "GLenum"))
   '( "GL_UNPACK_SKIP_ROWS"              . (constant "GLenum"))
   '( "GL_UNPACK_SKIP_PIXELS"            . (constant "GLenum"))
   '( "GL_UNPACK_ALIGNMENT"              . (constant "GLenum"))
   '( "GL_PACK_SWAP_BYTES"               . (constant "GLenum"))
   '( "GL_PACK_LSB_FIRST"                . (constant "GLenum"))
   '( "GL_PACK_ROW_LENGTH"               . (constant "GLenum"))
   '( "GL_PACK_SKIP_ROWS"                . (constant "GLenum"))
   '( "GL_PACK_SKIP_PIXELS"              . (constant "GLenum"))
   '( "GL_PACK_ALIGNMENT"                . (constant "GLenum"))
   '( "GL_MAP_COLOR"                     . (constant "GLenum"))
   '( "GL_MAP_STENCIL"                   . (constant "GLenum"))
   '( "GL_INDEX_SHIFT"                   . (constant "GLenum"))
   '( "GL_INDEX_OFFSET"                  . (constant "GLenum"))
   '( "GL_RED_SCALE"                     . (constant "GLenum"))
   '( "GL_RED_BIAS"                      . (constant "GLenum"))
   '( "GL_ZOOM_X"                        . (constant "GLenum"))
   '( "GL_ZOOM_Y"                        . (constant "GLenum"))
   '( "GL_GREEN_SCALE"                   . (constant "GLenum"))
   '( "GL_GREEN_BIAS"                    . (constant "GLenum"))
   '( "GL_BLUE_SCALE"                    . (constant "GLenum"))
   '( "GL_BLUE_BIAS"                     . (constant "GLenum"))
   '( "GL_ALPHA_SCALE"                   . (constant "GLenum"))
   '( "GL_ALPHA_BIAS"                    . (constant "GLenum"))
   '( "GL_DEPTH_SCALE"                   . (constant "GLenum"))
   '( "GL_DEPTH_BIAS"                    . (constant "GLenum"))
   '( "GL_MAX_EVAL_ORDER"                . (constant "GLenum"))
   '( "GL_MAX_LIGHTS"                    . (constant "GLenum"))
   '( "GL_MAX_CLIP_PLANES"               . (constant "GLenum"))
   '( "GL_MAX_TEXTURE_SIZE"              . (constant "GLenum"))
   '( "GL_MAX_PIXEL_MAP_TABLE"           . (constant "GLenum"))
   '( "GL_MAX_ATTRIB_STACK_DEPTH"        . (constant "GLenum"))
   '( "GL_MAX_MODELVIEW_STACK_DEPTH"     . (constant "GLenum"))
   '( "GL_MAX_NAME_STACK_DEPTH"          . (constant "GLenum"))
   '( "GL_MAX_PROJECTION_STACK_DEPTH"    . (constant "GLenum"))
   '( "GL_MAX_TEXTURE_STACK_DEPTH"       . (constant "GLenum"))
   '( "GL_MAX_VIEWPORT_DIMS"             . (constant "GLenum"))
   '( "GL_SUBPIXEL_BITS"                 . (constant "GLenum"))
   '( "GL_INDEX_BITS"                    . (constant "GLenum"))
   '( "GL_RED_BITS"                      . (constant "GLenum"))
   '( "GL_GREEN_BITS"                    . (constant "GLenum"))
   '( "GL_BLUE_BITS"                     . (constant "GLenum"))
   '( "GL_ALPHA_BITS"                    . (constant "GLenum"))
   '( "GL_DEPTH_BITS"                    . (constant "GLenum"))
   '( "GL_STENCIL_BITS"                  . (constant "GLenum"))
   '( "GL_ACCUM_RED_BITS"                . (constant "GLenum"))
   '( "GL_ACCUM_GREEN_BITS"              . (constant "GLenum"))
   '( "GL_ACCUM_BLUE_BITS"               . (constant "GLenum"))
   '( "GL_ACCUM_ALPHA_BITS"              . (constant "GLenum"))
   '( "GL_NAME_STACK_DEPTH"              . (constant "GLenum"))
   '( "GL_AUTO_NORMAL"                   . (constant "GLenum"))
   '( "GL_MAP1_COLOR_4"                  . (constant "GLenum"))
   '( "GL_MAP1_INDEX"                    . (constant "GLenum"))
   '( "GL_MAP1_NORMAL"                   . (constant "GLenum"))
   '( "GL_MAP1_TEXTURE_COORD_1"          . (constant "GLenum"))
   '( "GL_MAP1_TEXTURE_COORD_2"          . (constant "GLenum"))
   '( "GL_MAP1_TEXTURE_COORD_3"          . (constant "GLenum"))
   '( "GL_MAP1_TEXTURE_COORD_4"          . (constant "GLenum"))
   '( "GL_MAP1_VERTEX_3"                 . (constant "GLenum"))
   '( "GL_MAP1_VERTEX_4"                 . (constant "GLenum"))
   '( "GL_MAP2_COLOR_4"                  . (constant "GLenum"))
   '( "GL_MAP2_INDEX"                    . (constant "GLenum"))
   '( "GL_MAP2_NORMAL"                   . (constant "GLenum"))
   '( "GL_MAP2_TEXTURE_COORD_1"          . (constant "GLenum"))
   '( "GL_MAP2_TEXTURE_COORD_2"          . (constant "GLenum"))
   '( "GL_MAP2_TEXTURE_COORD_3"          . (constant "GLenum"))
   '( "GL_MAP2_TEXTURE_COORD_4"          . (constant "GLenum"))
   '( "GL_MAP2_VERTEX_3"                 . (constant "GLenum"))
   '( "GL_MAP2_VERTEX_4"                 . (constant "GLenum"))
   '( "GL_MAP1_GRID_DOMAIN"              . (constant "GLenum"))
   '( "GL_MAP1_GRID_SEGMENTS"            . (constant "GLenum"))
   '( "GL_MAP2_GRID_DOMAIN"              . (constant "GLenum"))
   '( "GL_MAP2_GRID_SEGMENTS"            . (constant "GLenum"))
   '( "GL_TEXTURE_1D"                    . (constant "GLenum"))
   '( "GL_TEXTURE_2D"                    . (constant "GLenum"))
   ;; GetTextureParameter
   '( "GL_TEXTURE_WIDTH"                 . (constant "GLenum"))
   '( "GL_TEXTURE_HEIGHT"                . (constant "GLenum"))
   '( "GL_TEXTURE_COMPONENTS"            . (constant "GLenum"))
   '( "GL_TEXTURE_BORDER_COLOR"          . (constant "GLenum"))
   '( "GL_TEXTURE_BORDER"                . (constant "GLenum"))
   ;; HintMode
   '( "GL_DONT_CARE"                     . (constant "GLenum"))
   '( "GL_FASTEST"                       . (constant "GLenum"))
   '( "GL_NICEST"                        . (constant "GLenum"))
   ;; HintTarget
   ;; LightModelParameter
   ;; LightName
   '( "GL_LIGHT0"                        . (constant "GLenum"))
   '( "GL_LIGHT1"                        . (constant "GLenum"))
   '( "GL_LIGHT2"                        . (constant "GLenum"))
   '( "GL_LIGHT3"                        . (constant "GLenum"))
   '( "GL_LIGHT4"                        . (constant "GLenum"))
   '( "GL_LIGHT5"                        . (constant "GLenum"))
   '( "GL_LIGHT6"                        . (constant "GLenum"))
   '( "GL_LIGHT7"                        . (constant "GLenum"))
   ;; LightParameter
   '( "GL_AMBIENT"                       . (constant "GLenum"))
   '( "GL_DIFFUSE"                       . (constant "GLenum"))
   '( "GL_SPECULAR"                      . (constant "GLenum"))
   '( "GL_POSITION"                      . (constant "GLenum"))
   '( "GL_SPOT_DIRECTION"                . (constant "GLenum"))
   '( "GL_SPOT_EXPONENT"                 . (constant "GLenum"))
   '( "GL_SPOT_CUTOFF"                   . (constant "GLenum"))
   '( "GL_CONSTANT_ATTENUATION"          . (constant "GLenum"))
   '( "GL_LINEAR_ATTENUATION"            . (constant "GLenum"))
   '( "GL_QUADRATIC_ATTENUATION"         . (constant "GLenum"))
   ;; ListMode
   '( "GL_COMPILE"                       . (constant "GLenum"))
   '( "GL_COMPILE_AND_EXECUTE"           . (constant "GLenum"))
   ;; ListNameType
   '( "GL_BYTE"                          . (constant "GLenum"))
   '( "GL_UNSIGNED_BYTE"                 . (constant "GLenum"))
   '( "GL_SHORT"                         . (constant "GLenum"))
   '( "GL_UNSIGNED_SHORT"                . (constant "GLenum"))
   '( "GL_INT"                           . (constant "GLenum"))
   '( "GL_UNSIGNED_INT"                  . (constant "GLenum"))
   '( "GL_FLOAT"                         . (constant "GLenum"))
   '( "GL_2_BYTES"                       . (constant "GLenum"))
   '( "GL_3_BYTES"                       . (constant "GLenum"))
   '( "GL_4_BYTES"                       . (constant "GLenum"))
   ;; LogicOp
   '( "GL_CLEAR"                         . (constant "GLenum"))
   '( "GL_AND"                           . (constant "GLenum"))
   '( "GL_AND_REVERSE"                   . (constant "GLenum"))
   '( "GL_COPY"                          . (constant "GLenum"))
   '( "GL_AND_INVERTED"                  . (constant "GLenum"))
   '( "GL_NOOP"                          . (constant "GLenum"))
   '( "GL_XOR"                           . (constant "GLenum"))
   '( "GL_OR"                            . (constant "GLenum"))
   '( "GL_NOR"                           . (constant "GLenum"))
   '( "GL_EQUIV"                         . (constant "GLenum"))
   '( "GL_INVERT"                        . (constant "GLenum"))
   '( "GL_OR_REVERSE"                    . (constant "GLenum"))
   '( "GL_COPY_INVERTED"                 . (constant "GLenum"))
   '( "GL_OR_INVERTED"                   . (constant "GLenum"))
   '( "GL_NAND"                          . (constant "GLenum"))
   '( "GL_SET"                           . (constant "GLenum"))
   ;; MapTarget
   ;; MaterialFace
   ;; MaterialParameter
   '( "GL_EMISSION"                      . (constant "GLenum"))
   '( "GL_SHININESS"                     . (constant "GLenum"))
   '( "GL_AMBIENT_AND_DIFFUSE"           . (constant "GLenum"))
   '( "GL_COLOR_INDEXES"                 . (constant "GLenum"))
   ;; MatrixMode
   '( "GL_MODELVIEW"                     . (constant "GLenum"))
   '( "GL_PROJECTION"                    . (constant "GLenum"))
   '( "GL_TEXTURE"                       . (constant "GLenum"))
   ;; MeshMode1
   ;; MeshMode2
   ;; PixelCopyType
   '( "GL_COLOR"                         . (constant "GLenum"))
   '( "GL_DEPTH"                         . (constant "GLenum"))
   '( "GL_STENCIL"                       . (constant "GLenum"))
   ;; PixelFormat
   '( "GL_COLOR_INDEX"                   . (constant "GLenum"))
   '( "GL_STENCIL_INDEX"                 . (constant "GLenum"))
   '( "GL_DEPTH_COMPONENT"               . (constant "GLenum"))
   '( "GL_RED"                           . (constant "GLenum"))
   '( "GL_GREEN"                         . (constant "GLenum"))
   '( "GL_BLUE"                          . (constant "GLenum"))
   '( "GL_ALPHA"                         . (constant "GLenum"))
   '( "GL_RGB"                           . (constant "GLenum"))
   '( "GL_RGBA"                          . (constant "GLenum"))
   '( "GL_LUMINANCE"                     . (constant "GLenum"))
   '( "GL_LUMINANCE_ALPHA"               . (constant "GLenum"))
   ;; PixelMap
   ;; PixelStore
   ;; PixelTransfer
   ;; PixelType
   '( "GL_BITMAP"                        . (constant "GLenum"))
   ;; PolygonMode
   '( "GL_POINT"                         . (constant "GLenum"))
   '( "GL_LINE"                          . (constant "GLenum"))
   '( "GL_FILL"                          . (constant "GLenum"))
   ;; ReadBufferMode
   ;; RenderingMode
   '( "GL_RENDER"                        . (constant "GLenum"))
   '( "GL_FEEDBACK"                      . (constant "GLenum"))
   '( "GL_SELECT"                        . (constant "GLenum"))
   ;; ShadingModel
   '( "GL_FLAT"                          . (constant "GLenum"))
   '( "GL_SMOOTH"                        . (constant "GLenum"))
   ;; StencilFunction
   ;; StencilOp
   '( "GL_KEEP"                          . (constant "GLenum"))
   '( "GL_REPLACE"                       . (constant "GLenum"))
   '( "GL_INCR"                          . (constant "GLenum"))
   '( "GL_DECR"                          . (constant "GLenum"))
   ;; StringName
   '( "GL_VENDOR"                        . (constant "GLenum"))
   '( "GL_RENDERER"                      . (constant "GLenum"))
   '( "GL_VERSION"                       . (constant "GLenum"))
   '( "GL_EXTENSIONS"                    . (constant "GLenum"))
   ;; TextureCoordName
   '( "GL_S"                             . (constant "GLenum"))
   '( "GL_T"                             . (constant "GLenum"))
   '( "GL_R"                             . (constant "GLenum"))
   '( "GL_Q"                             . (constant "GLenum"))
   ;; TextureEnvMode
   '( "GL_MODULATE"                      . (constant "GLenum"))
   '( "GL_DECAL"                         . (constant "GLenum"))
   ;; TextureEnvParameter
   '( "GL_TEXTURE_ENV_MODE"              . (constant "GLenum"))
   '( "GL_TEXTURE_ENV_COLOR"             . (constant "GLenum"))
   ;; TextureEnvTarget
   '( "GL_TEXTURE_ENV"                   . (constant "GLenum"))
   ;; TextureGenMode
   '( "GL_EYE_LINEAR"                    . (constant "GLenum"))
   '( "GL_OBJECT_LINEAR"                 . (constant "GLenum"))
   '( "GL_SPHERE_MAP"                    . (constant "GLenum"))
   ;; TextureGenParameter
   '( "GL_TEXTURE_GEN_MODE"              . (constant "GLenum"))
   '( "GL_OBJECT_PLANE"                  . (constant "GLenum"))
   '( "GL_EYE_PLANE"                     . (constant "GLenum"))
   ;; TextureMagFilter
   '( "GL_NEAREST"                       . (constant "GLenum"))
   '( "GL_LINEAR"                        . (constant "GLenum"))
   ;; TextureMinFilter
   '( "GL_NEAREST_MIPMAP_NEAREST"        . (constant "GLenum"))
   '( "GL_LINEAR_MIPMAP_NEAREST"         . (constant "GLenum"))
   '( "GL_NEAREST_MIPMAP_LINEAR"         . (constant "GLenum"))
   '( "GL_LINEAR_MIPMAP_LINEAR"          . (constant "GLenum"))
   ;; TextureParameterName
   '( "GL_TEXTURE_MAG_FILTER"            . (constant "GLenum"))
   '( "GL_TEXTURE_MIN_FILTER"            . (constant "GLenum"))
   '( "GL_TEXTURE_WRAP_S"                . (constant "GLenum"))
   '( "GL_TEXTURE_WRAP_T"                . (constant "GLenum"))
   ;; TextureTarget
   ;; TextureWrapMode
   '( "GL_CLAMP"                         . (constant "GLenum"))
   '( "GL_REPEAT"                        . (constant "GLenum"))
   ;;
   ;; GL/gl.h Types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   '( "GLbitfield"                       . (typedef "unsigned int"))
   '( "GLboolean"                        . (typedef "unsigned char"))
   '( "GLbyte"                           . (typedef "signed char"))
   '( "GLclampd"                         . (typedef "double"))
   '( "GLclampf"                         . (typedef "float"))
   '( "GLdouble"                         . (typedef "double"))
   '( "GLenum"                           . (typedef "unsigned int"))
   '( "GLfloat"                          . (typedef "float"))
   '( "GLint"                            . (typedef "int"))
   '( "GLshort"                          . (typedef "short"))
   '( "GLsizei"                          . (typedef "int"))
   '( "GLuint"                           . (typedef "unsigned int"))
   '( "GLushort"                         . (typedef "unsigned short"))
   '( "GLvoid"                           . (typedef "void"))
   ;;
   ;; GL/gl.h Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   '( "glAccum"                          . (function "void" "GLenum op, GLfloat value"))
   '( "glAlphaFunc"                      . (function "void" "GLenum func, GLclampf ref"))
   '( "glBegin"                          . (function "void" "GLenum mode"))
   '( "glBitmap"                         . (function "void" "GLsizei width, GLsizei height, GLfloat xorig, GLfloat yorig, GLfloat xmove, GLfloat ymove, const GLubyte *bitmap"))
   '( "glBlendFunc"                      . (function "void" "GLenum sfactor, GLenum dfactor"))
   '( "glCallList"                       . (function "void" "GLuint list"))
   '( "glCallLists"                      . (function "void" "GLsizei n, GLenum type, const GLvoid *lists"))
   '( "glClear"                          . (function "void" "GLbitfield mask"))
   '( "glClearAccum"                     . (function "void" "GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha"))
   '( "glClearColor"                     . (function "void" "GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha"))
   '( "glClearDepth"                     . (function "void" "GLclampd depth"))
   '( "glClearIndex"                     . (function "void" "GLfloat c"))
   '( "glClearStencil"                   . (function "void" "GLint s"))
   '( "glClipPlane"                      . (function "void" "GLenum plane, const GLdouble *equation"))
   '( "glColor3b"                        . (function "void" "GLbyte red, GLbyte green, GLbyte blue"))
   '( "glColor3bv"                       . (function "void" "const GLbyte *v"))
   '( "glColor3d"                        . (function "void" "GLdouble red, GLdouble green, GLdouble blue"))
   '( "glColor3dv"                       . (function "void" "const GLdouble *v"))
   '( "glColor3f"                        . (function "void" "GLfloat red, GLfloat green, GLfloat blue"))
   '( "glColor3fv"                       . (function "void" "const GLfloat *v"))
   '( "glColor3i"                        . (function "void" "GLint red, GLint green, GLint blue"))
   '( "glColor3iv"                       . (function "void" "const GLint *v"))
   '( "glColor3s"                        . (function "void" "GLshort red, GLshort green, GLshort blue"))
   '( "glColor3sv"                       . (function "void" "const GLshort *v"))
   '( "glColor3ub"                       . (function "void" "GLubyte red, GLubyte green, GLubyte blue"))
   '( "glColor3ubv"                      . (function "void" "const GLubyte *v"))
   '( "glColor3ui"                       . (function "void" "GLuint red, GLuint green, GLuint blue"))
   '( "glColor3uiv"                      . (function "void" "const GLuint *v"))
   '( "glColor3us"                       . (function "void" "GLushort red, GLushort green, GLushort blue"))
   '( "glColor3usv"                      . (function "void" "const GLushort *v"))
   '( "glColor4b"                        . (function "void" "GLbyte red, GLbyte green, GLbyte blue, GLbyte alpha"))
   '( "glColor4bv"                       . (function "void" "const GLbyte *v"))
   '( "glColor4d"                        . (function "void" "GLdouble red, GLdouble green, GLdouble blue, GLdouble alpha"))
   '( "glColor4dv"                       . (function "void" "const GLdouble *v"))
   '( "glColor4f"                        . (function "void" "GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha"))
   '( "glColor4fv"                       . (function "void" "const GLfloat *v"))
   '( "glColor4i"                        . (function "void" "GLint red, GLint green, GLint blue, GLint alpha"))
   '( "glColor4iv"                       . (function "void" "const GLint *v"))
   '( "glColor4s"                        . (function "void" "GLshort red, GLshort green, GLshort blue, GLshort alpha"))
   '( "glColor4sv"                       . (function "void" "const GLshort *v"))
   '( "glColor4ub"                       . (function "void" "GLubyte red, GLubyte green, GLubyte blue, GLubyte alpha"))
   '( "glColor4ubv"                      . (function "void" "const GLubyte *v"))
   '( "glColor4ui"                       . (function "void" "GLuint red, GLuint green, GLuint blue, GLuint alpha"))
   '( "glColor4uiv"                      . (function "void" "const GLuint *v"))
   '( "glColor4us"                       . (function "void" "GLushort red, GLushort green, GLushort blue, GLushort alpha"))
   '( "glColor4usv"                      . (function "void" "const GLushort *v"))
   '( "glColorMask"                      . (function "void" "GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha"))
   '( "glColorMaterial"                  . (function "void" "GLenum face, GLenum mode"))
   '( "glCopyPixels"                     . (function "void" "GLint x, GLint y, GLsizei width, GLsizei height, GLenum type"))
   '( "glCullFace"                       . (function "void" "GLenum mode"))
   '( "glDeleteLists"                    . (function "void" "GLuint list, GLsizei range"))
   '( "glDepthFunc"                      . (function "void" "GLenum func"))
   '( "glDepthMask"                      . (function "void" "GLboolean flag"))
   '( "glDepthRange"                     . (function "void" "GLclampd near, GLclampd far"))
   '( "glDisable"                        . (function "void" "GLenum cap"))
   '( "glDrawBuffer"                     . (function "void" "GLenum mode"))
   '( "glDrawPixels"                     . (function "void" "GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *pixels"))
   '( "glEdgeFlag"                       . (function "void" "GLboolean flag"))
   '( "glEdgeFlagv"                      . (function "void" "const GLboolean *flag"))
   '( "glEnable"                         . (function "void" "GLenum cap"))
   '( "glEnd"                            . (function "void" "void"))
   '( "glEndList"                        . (function "void" "void"))
   '( "glEvalCoord1d"                    . (function "void" "GLdouble u"))
   '( "glEvalCoord1dv"                   . (function "void" "const GLdouble *u"))
   '( "glEvalCoord1f"                    . (function "void" "GLfloat u"))
   '( "glEvalCoord1fv"                   . (function "void" "const GLfloat *u"))
   '( "glEvalCoord2d"                    . (function "void" "GLdouble u, GLdouble v"))
   '( "glEvalCoord2dv"                   . (function "void" "const GLdouble *u"))
   '( "glEvalCoord2f"                    . (function "void" "GLfloat u, GLfloat v"))
   '( "glEvalCoord2fv"                   . (function "void" "const GLfloat *u"))
   '( "glEvalMesh1"                      . (function "void" "GLenum mode, GLint i1, GLint i2"))
   '( "glEvalMesh2"                      . (function "void" "GLenum mode, GLint i1, GLint i2, GLint j1, GLint j2"))
   '( "glEvalPoint1"                     . (function "void" "GLint i"))
   '( "glEvalPoint2"                     . (function "void" "GLint i, GLint j"))
   '( "glFeedbackBuffer"                 . (function "void" "GLsizei size, GLenum type, GLfloat *buffer"))
   '( "glFinish"                         . (function "void" "void"))
   '( "glFlush"                          . (function "void" "void"))
   '( "glFogf"                           . (function "void" "GLenum pname, GLfloat param"))
   '( "glFogfv"                          . (function "void" "GLenum pname, const GLfloat *params"))
   '( "glFogi"                           . (function "void" "GLenum pname, GLint param"))
   '( "glFogiv"                          . (function "void" "GLenum pname, const GLint *params"))
   '( "glFrontFace"                      . (function "void" "GLenum mode"))
   '( "glFrustum"                        . (function "void" "GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble near, GLdouble far"))
   '( "glGenLists"                       . (function "GLuint" "GLsizei range"))
   '( "glGetBooleanv"                    . (function "void" "GLenum pname, GLboolean *params"))
   '( "glGetClipPlane"                   . (function "void" "GLenum plane, GLdouble *equation"))
   '( "glGetDoublev"                     . (function "void" "GLenum pname, GLdouble *params"))
   '( "glGetError"                       . (function "GLenum" "void"))
   '( "glGetFloatv"                      . (function "void" "GLenum pname, GLfloat *params"))
   '( "glGetIntegerv"                    . (function "void" "GLenum pname, GLint *params"))
   '( "glGetLightfv"                     . (function "void" "GLenum light, GLenum pname, GLfloat *params"))
   '( "glGetLightiv"                     . (function "void" "GLenum light, GLenum pname, GLint *params"))
   '( "glGetMapdv"                       . (function "void" "GLenum target, GLenum query, GLdouble *v"))
   '( "glGetMapfv"                       . (function "void" "GLenum target, GLenum query, GLfloat *v"))
   '( "glGetMapiv"                       . (function "void" "GLenum target, GLenum query, GLint *v"))
   '( "glGetMaterialfv"                  . (function "void" "GLenum face, GLenum pname, GLfloat *params"))
   '( "glGetMaterialiv"                  . (function "void" "GLenum face, GLenum pname, GLint *params"))
   '( "glGetPixelMapfv"                  . (function "void" "GLenum map, GLfloat *values"))
   '( "glGetPixelMapuiv"                 . (function "void" "GLenum map, GLuint *values"))
   '( "glGetPixelMapusv"                 . (function "void" "GLenum map, GLushort *values"))
   '( "glGetPolygonStipple"              . (function "void" "GLubyte *mask"))
   '( "glGetString"                      . (Function "Const GLubyte *" "GLenum name"))
   '( "glGetTexEnvfv"                    . (function "void" "GLenum target, GLenum pname, GLfloat *params"))
   '( "glGetTexEnviv"                    . (function "void" "GLenum target, GLenum pname, GLint *params"))
   '( "glGetTexGendv"                    . (function "void" "GLenum coord, GLenum pname, GLdouble *params"))
   '( "glGetTexGenfv"                    . (function "void" "GLenum coord, GLenum pname, GLfloat *params"))
   '( "glGetTexGeniv"                    . (function "void" "GLenum coord, GLenum pname, GLint *params"))
   '( "glGetTexImage"                    . (function "void" "GLenum target, GLint level, GLenum format, GLenum type, GLvoid *pixels"))
   '( "glGetTexLevelParameterfv"         . (function "void" "GLenum target, GLint level, GLenum pname, GLfloat *params"))
   '( "glGetTexLevelParameteriv"         . (function "void" "GLenum target, GLint level, GLenum pname, GLint *params"))
   '( "glGetTexParameterfv"              . (function "void" "GLenum target, GLenum pname, GLfloat *params"))
   '( "glGetTexParameteriv"              . (function "void" "GLenum target, GLenum pname, GLint *params"))
   '( "glHint"                           . (function "void" "GLenum target, GLenum mode"))
   '( "glIndexMask"                      . (function "void" "GLuint mask"))
   '( "glIndexd"                         . (function "void" "GLdouble c"))
   '( "glIndexdv"                        . (function "void" "const GLdouble *c"))
   '( "glIndexf"                         . (function "void" "GLfloat c"))
   '( "glIndexfv"                        . (function "void" "const GLfloat *c"))
   '( "glIndexi"                         . (function "void" "GLint c"))
   '( "glIndexiv"                        . (function "void" "const GLint *c"))
   '( "glIndexs"                         . (function "void" "GLshort c"))
   '( "glIndexsv"                        . (function "void" "const GLshort *c"))
   '( "glInitNames"                      . (function "void" "void"))
   '( "glIsEnabled"                      . (function "GLboolean" "GLenum cap"))
   '( "glIsList"                         . (function "GLboolean" "GLuint list"))
   '( "glLightModelf"                    . (function "void" "GLenum pname, GLfloat param"))
   '( "glLightModelfv"                   . (function "void" "GLenum pname, const GLfloat *params"))
   '( "glLightModeli"                    . (function "void" "GLenum pname, GLint param"))
   '( "glLightModeliv"                   . (function "void" "GLenum pname, const GLint *params"))
   '( "glLightf"                         . (function "void" "GLenum light, GLenum pname, GLfloat param"))
   '( "glLightfv"                        . (function "void" "GLenum light, GLenum pname, const GLfloat *params"))
   '( "glLighti"                         . (function "void" "GLenum light, GLenum pname, GLint param"))
   '( "glLightiv"                        . (function "void" "GLenum light, GLenum pname, const GLint *params"))
   '( "glLineStipple"                    . (function "void" "GLint factor, GLushort pattern"))
   '( "glLineWidth"                      . (function "void" "GLfloat width"))
   '( "glListBase"                       . (function "void" "GLuint base"))
   '( "glLoadIdentity"                   . (function "void" "void"))
   '( "glLoadMatrixd"                    . (function "void" "const GLdouble *m"))
   '( "glLoadMatrixf"                    . (function "void" "const GLfloat *m"))
   '( "glLoadName"                       . (function "void" "GLuint name"))
   '( "glLogicOp"                        . (function "void" "GLenum opcode"))
   '( "glMap1d"                          . (function "void" "GLenum target, GLdouble u1, GLdouble u2, GLint stride, GLint order, const GLdouble *points"))
   '( "glMap1f"                          . (function "void" "GLenum target, GLfloat u1, GLfloat u2, GLint stride, GLint order, const GLfloat *points"))
   '( "glMap2d"                          . (function "void" "GLenum target, GLdouble u1, GLdouble u2, GLint ustride, GLint uorder, GLdouble v1, GLdouble v2, GLint vstride, GLint vorder, const GLdouble *points"))
   '( "glMap2f"                          . (function "void" "GLenum target, GLfloat u1, GLfloat u2, GLint ustride, GLint uorder, GLfloat v1, GLfloat v2, GLint vstride, GLint vorder, const GLfloat *points"))
   '( "glMapGrid1d"                      . (function "void" "GLint un, GLdouble u1, GLdouble u2"))
   '( "glMapGrid1f"                      . (function "void" "GLint un, GLfloat u1, GLfloat u2"))
   '( "glMapGrid2d"                      . (function "void" "GLint un, GLdouble u1, GLdouble u2, GLint vn, GLdouble v1, GLdouble v2"))
   '( "glMapGrid2f"                      . (function "void" "GLint un, GLfloat u1, GLfloat u2, GLint vn, GLfloat v1, GLfloat v2"))
   '( "glMaterialf"                      . (function "void" "GLenum face, GLenum pname, GLfloat param"))
   '( "glMaterialfv"                     . (function "void" "GLenum face, GLenum pname, const GLfloat *params"))
   '( "glMateriali"                      . (function "void" "GLenum face, GLenum pname, GLint param"))
   '( "glMaterialiv"                     . (function "void" "GLenum face, GLenum pname, const GLint *params"))
   '( "glMatrixMode"                     . (function "void" "GLenum mode"))
   '( "glMultMatrixd"                    . (function "void" "const GLdouble *m"))
   '( "glMultMatrixf"                    . (function "void" "const GLfloat *m"))
   '( "glNewList"                        . (function "void" "GLuint list, GLenum mode"))
   '( "glNormal3b"                       . (function "void" "GLbyte nx, GLbyte ny, GLbyte nz"))
   '( "glNormal3bv"                      . (function "void" "const GLbyte *v"))
   '( "glNormal3d"                       . (function "void" "GLdouble nx, GLdouble ny, GLdouble nz"))
   '( "glNormal3dv"                      . (function "void" "const GLdouble *v"))
   '( "glNormal3f"                       . (function "void" "GLfloat nx, GLfloat ny, GLfloat nz"))
   '( "glNormal3fv"                      . (function "void" "const GLfloat *v"))
   '( "glNormal3i"                       . (function "void" "GLint nx, GLint ny, GLint nz"))
   '( "glNormal3iv"                      . (function "void" "const GLint *v"))
   '( "glNormal3s"                       . (function "void" "GLshort nx, GLshort ny, GLshort nz"))
   '( "glNormal3sv"                      . (function "void" "const GLshort *v"))
   '( "glOrtho"                          . (function "void" "GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble near, GLdouble far"))
   '( "glPassThrough"                    . (function "void" "GLfloat token"))
   '( "glPixelMapfv"                     . (function "void" "GLenum map, GLint mapsize, const GLfloat *values"))
   '( "glPixelMapuiv"                    . (function "void" "GLenum map, GLint mapsize, const GLuint *values"))
   '( "glPixelMapusv"                    . (function "void" "GLenum map, GLint mapsize, const GLushort *values"))
   '( "glPixelStoref"                    . (function "void" "GLenum pname, GLfloat param"))
   '( "glPixelStorei"                    . (function "void" "GLenum pname, GLint param"))
   '( "glPixelTransferf"                 . (function "void" "GLenum pname, GLfloat param"))
   '( "glPixelTransferi"                 . (function "void" "GLenum pname, GLint param"))
   '( "glPixelZoom"                      . (function "void" "GLfloat xfactor, GLfloat yfactor"))
   '( "glPointSize"                      . (function "void" "GLfloat size"))
   '( "glPolygonMode"                    . (function "void" "GLenum face, GLenum mode"))
   '( "glPolygonStipple"                 . (function "void" "const GLubyte *mask"))
   '( "glPopAttrib"                      . (function "void" "void"))
   '( "glPopMatrix"                      . (function "void" "void"))
   '( "glPopName"                        . (function "void" "void"))
   '( "glPushAttrib"                     . (function "void" "GLbitfield mask"))
   '( "glPushMatrix"                     . (function "void" "void"))
   '( "glPushName"                       . (function "void" "GLuint name"))
   '( "glRasterPos2d"                    . (function "void" "GLdouble x, GLdouble y"))
   '( "glRasterPos2dv"                   . (function "void" "const GLdouble *v"))
   '( "glRasterPos2f"                    . (function "void" "GLfloat x, GLfloat y"))
   '( "glRasterPos2fv"                   . (function "void" "const GLfloat *v"))
   '( "glRasterPos2i"                    . (function "void" "GLint x, GLint y"))
   '( "glRasterPos2iv"                   . (function "void" "const GLint *v"))
   '( "glRasterPos2s"                    . (function "void" "GLshort x, GLshort y"))
   '( "glRasterPos2sv"                   . (function "void" "const GLshort *v"))
   '( "glRasterPos3d"                    . (function "void" "GLdouble x, GLdouble y, GLdouble z"))
   '( "glRasterPos3dv"                   . (function "void" "const GLdouble *v"))
   '( "glRasterPos3f"                    . (function "void" "GLfloat x, GLfloat y, GLfloat z"))
   '( "glRasterPos3fv"                   . (function "void" "const GLfloat *v"))
   '( "glRasterPos3i"                    . (function "void" "GLint x, GLint y, GLint z"))
   '( "glRasterPos3iv"                   . (function "void" "const GLint *v"))
   '( "glRasterPos3s"                    . (function "void" "GLshort x, GLshort y, GLshort z"))
   '( "glRasterPos3sv"                   . (function "void" "const GLshort *v"))
   '( "glRasterPos4d"                    . (function "void" "GLdouble x, GLdouble y, GLdouble z, GLdouble w"))
   '( "glRasterPos4dv"                   . (function "void" "const GLdouble *v"))
   '( "glRasterPos4f"                    . (function "void" "GLfloat x, GLfloat y, GLfloat z, GLfloat w"))
   '( "glRasterPos4fv"                   . (function "void" "const GLfloat *v"))
   '( "glRasterPos4i"                    . (function "void" "GLint x, GLint y, GLint z, GLint w"))
   '( "glRasterPos4iv"                   . (function "void" "const GLint *v"))
   '( "glRasterPos4s"                    . (function "void" "GLshort x, GLshort y, GLshort z, GLshort w"))
   '( "glRasterPos4sv"                   . (function "void" "const GLshort *v"))
   '( "glReadBuffer"                     . (function "void" "GLenum mode"))
   '( "glReadPixels"                     . (function "void" "GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, GLvoid *pixels"))
   '( "glRectd"                          . (function "void" "GLdouble x1, GLdouble y1, GLdouble x2, GLdouble y2"))
   '( "glRectdv"                         . (function "void" "const GLdouble *v1, const GLdouble *v2"))
   '( "glRectf"                          . (function "void" "GLfloat x1, GLfloat y1, GLfloat x2, GLfloat y2"))
   '( "glRectfv"                         . (function "void" "const GLfloat *v1, const GLfloat *v2"))
   '( "glRecti"                          . (function "void" "GLint x1, GLint y1, GLint x2, GLint y2"))
   '( "glRectiv"                         . (function "void" "const GLint *v1, const GLint *v2"))
   '( "glRects"                          . (function "void" "GLshort x1, GLshort y1, GLshort x2, GLshort y2"))
   '( "glRectsv"                         . (function "void" "const GLshort *v1, const GLshort *v2"))
   '( "glRenderMode"                     . (function "GLint" "GLenum mode"))
   '( "glRotated"                        . (function "void" "GLdouble angle, GLdouble x, GLdouble y, GLdouble z"))
   '( "glRotatef"                        . (function "void" "GLfloat angle, GLfloat x, GLfloat y, GLfloat z"))
   '( "glScaled"                         . (function "void" "GLfloat x, GLfloat y, GLfloat z"))
   '( "glScalef"                         . (function "void" "GLfloat x, GLfloat y, GLfloat z"))
   '( "glScissor"                        . (function "void" "GLint x, GLint y, GLsizei width, GLsizei height"))
   '( "glSelectBuffer"                   . (function "void" "GLsizei size, GLuint *buffer"))
   '( "glShadeModel"                     . (function "void" "GLenum mode"))
   '( "glStencilFunc"                    . (function "void" "GLenum func, GLint ref, GLuint mask"))
   '( "glStencilMask"                    . (function "void" "GLuint mask"))
   '( "glStencilOp"                      . (function "void" "GLenum fail, GLenum zfail, GLenum zpass"))
   '( "glTexCoord1d"                     . (function "void" "GLdouble s"))
   '( "glTexCoord1dv"                    . (function "void" "const GLdouble *v"))
   '( "glTexCoord1f"                     . (function "void" "GLfloat s"))
   '( "glTexCoord1fv"                    . (function "void" "const GLfloat *v"))
   '( "glTexCoord1i"                     . (function "void" "GLint s"))
   '( "glTexCoord1iv"                    . (function "void" "const GLint *v"))
   '( "glTexCoord1s"                     . (function "void" "GLshort s"))
   '( "glTexCoord1sv"                    . (function "void" "const GLshort *v"))
   '( "glTexCoord2d"                     . (function "void" "GLdouble s, GLdouble t"))
   '( "glTexCoord2dv"                    . (function "void" "const GLdouble *v"))
   '( "glTexCoord2f"                     . (function "void" "GLfloat s, GLfloat t"))
   '( "glTexCoord2fv"                    . (function "void" "const GLfloat *v"))
   '( "glTexCoord2i"                     . (function "void" "GLint s, GLint t"))
   '( "glTexCoord2iv"                    . (function "void" "const GLint *v"))
   '( "glTexCoord2s"                     . (function "void" "GLshort s, GLshort t"))
   '( "glTexCoord2sv"                    . (function "void" "const GLshort *v"))
   '( "glTexCoord3d"                     . (function "void" "GLdouble s, GLdouble t, GLdouble r"))
   '( "glTexCoord3dv"                    . (function "void" "const GLdouble *v"))
   '( "glTexCoord3f"                     . (function "void" "GLfloat s, GLfloat t, GLfloat r"))
   '( "glTexCoord3fv"                    . (function "void" "const GLfloat *v"))
   '( "glTexCoord3i"                     . (function "void" "GLint s, GLint t, GLint r"))
   '( "glTexCoord3iv"                    . (function "void" "const GLint *v"))
   '( "glTexCoord3s"                     . (function "void" "GLshort s, GLshort t, GLshort r"))
   '( "glTexCoord3sv"                    . (function "void" "const GLshort *v"))
   '( "glTexCoord4d"                     . (function "void" "GLdouble s, GLdouble t, GLdouble r, GLdouble q"))
   '( "glTexCoord4dv"                    . (function "void" "const GLdouble *v"))
   '( "glTexCoord4f"                     . (function "void" "GLfloat s, GLfloat t, GLfloat r, GLfloat q"))
   '( "glTexCoord4fv"                    . (function "void" "const GLfloat *v"))
   '( "glTexCoord4i"                     . (function "void" "GLint s, GLint t, GLint r, GLint q"))
   '( "glTexCoord4iv"                    . (function "void" "const GLint *v"))
   '( "glTexCoord4s"                     . (function "void" "GLshort s, GLshort t, GLshort r, GLshort q"))
   '( "glTexCoord4sv"                    . (function "void" "const GLshort *v"))
   '( "glTexEnvf"                        . (function "void" "GLenum target, GLenum pname, GLfloat param"))
   '( "glTexEnvfv"                       . (function "void" "GLenum target, GLenum pname, const GLfloat *params"))
   '( "glTexEnvi"                        . (function "void" "GLenum target, GLenum pname, GLint param"))
   '( "glTexEnviv"                       . (function "void" "GLenum target, GLenum pname, const GLint *params"))
   '( "glTexGend"                        . (function "void" "GLenum coord, GLenum pname, GLdouble param"))
   '( "glTexGendv"                       . (function "void" "GLenum coord, GLenum pname, const GLdouble *params"))
   '( "glTexGenf"                        . (function "void" "GLenum coord, GLenum pname, GLfloat param"))
   '( "glTexGenfv"                       . (function "void" "GLenum coord, GLenum pname, const GLfloat *params"))
   '( "glTexGeni"                        . (function "void" "GLenum coord, GLenum pname, GLint param"))
   '( "glTexGeniv"                       . (function "void" "GLenum coord, GLenum pname, const GLint *params"))
   '( "glTexImage1D"                     . (function "void" "GLenum target, GLint level, GLint components, GLsizei width, GLint border, GLenum format, GLenum type, const GLvoid *pixels"))
   '( "glTexImage2D"                     . (function "void" "GLenum target, GLint level, GLint components, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, const GLvoid *pixels"))
   '( "glTexParameterf"                  . (function "void" "GLenum target, GLenum pname, GLfloat param"))
   '( "glTexParameterfv"                 . (function "void" "GLenum target, GLenum pname, const GLfloat *params"))
   '( "glTexParameteri"                  . (function "void" "GLenum target, GLenum pname, GLint param"))
   '( "glTexParameteriv"                 . (function "void" "GLenum target, GLenum pname, const GLint *params"))
   '( "glTranslated"                     . (function "void" "GLdouble x, GLdouble y, GLdouble z"))
   '( "glTranslatef"                     . (function "void" "GLfloat x, GLfloat y, GLfloat z"))
   '( "glVertex2d"                       . (function "void" "GLdouble x, GLdouble y"))
   '( "glVertex2dv"                      . (function "void" "const GLdouble *v"))
   '( "glVertex2f"                       . (function "void" "GLfloat x, GLfloat y"))
   '( "glVertex2fv"                      . (function "void" "const GLfloat *v"))
   '( "glVertex2i"                       . (function "void" "GLint x, GLint y"))
   '( "glVertex2iv"                      . (function "void" "const GLint *v"))
   '( "glVertex2s"                       . (function "void" "GLshort x, GLshort y"))
   '( "glVertex2sv"                      . (function "void" "const GLshort *v"))
   '( "glVertex3d"                       . (function "void" "GLdouble x, GLdouble y, GLdouble z"))
   '( "glVertex3dv"                      . (function "void" "const GLdouble *v"))
   '( "glVertex3f"                       . (function "void" "GLfloat x, GLfloat y, GLfloat z"))
   '( "glVertex3fv"                      . (function "void" "const GLfloat *v"))
   '( "glVertex3i"                       . (function "void" "GLint x, GLint y, GLint z"))
   '( "glVertex3iv"                      . (function "void" "const GLint *v"))
   '( "glVertex3s"                       . (function "void" "GLshort x, GLshort y, GLshort z"))
   '( "glVertex3sv"                      . (function "void" "const GLshort *v"))
   '( "glVertex4d"                       . (function "void" "GLdouble x, GLdouble y, GLdouble z, GLdouble w"))
   '( "glVertex4dv"                      . (function "void" "const GLdouble *v"))
   '( "glVertex4f"                       . (function "void" "GLfloat x, GLfloat y, GLfloat z, GLfloat w"))
   '( "glVertex4fv"                      . (function "void" "const GLfloat *v"))
   '( "glVertex4i"                       . (function "void" "GLint x, GLint y, GLint z, GLint w"))
   '( "glVertex4iv"                      . (function "void" "const GLint *v"))
   '( "glVertex4s"                       . (function "void" "GLshort x, GLshort y, GLshort z, GLshort w"))
   '( "glVertex4sv"                      . (function "void" "const GLshort *v"))
   '( "glViewport"                       . (function "void" "GLint x, GLint y, GLsizei width, GLsizei height"))
   ;;  
   ;; GL/glu.h Constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   ;; Errors
   '( "GLU_INVALID_ENUM"                 . (constant "GLenum"))
   '( "GLU_INVALID_VALUE"                . (constant "GLenum"))
   '( "GLU_OUT_OF_MEMORY"                . (constant "GLenum"))
   ;; For laughs
   '( "GLU_TRUE"                         . (constant "GLenum"))
   '( "GLU_FALSE"                        . (constant "GLenum"))
   ;; Types of normals
   '( "GLU_SMOOTH"                       . (constant "GLenum"))
   '( "GLU_FLAT"                         . (constant "GLenum"))
   '( "GLU_NONE"                         . (constant "GLenum"))
   ;; DrawStyle types
   '( "GLU_POINT"                        . (constant "GLenum"))
   '( "GLU_LINE"                         . (constant "GLenum"))
   '( "GLU_FILL"                         . (constant "GLenum"))
   '( "GLU_SILHOUETTE"                   . (constant "GLenum"))
   ;; Orientation types
   '( "GLU_OUTSIDE"                      . (constant "GLenum"))
   '( "GLU_INSIDE"                       . (constant "GLenum"))
   ;; Callback types
   '( "GLU_BEGIN"                        . (constant "GLenum"))
   '( "GLU_VERTEX"                       . (constant "GLenum"))
   '( "GLU_END"                          . (constant "GLenum"))
   '( "GLU_ERROR"                        . (constant "GLenum"))
   '( "GLU_EDGE_FLAG"                    . (constant "GLenum"))
   ;; Contours types
   '( "GLU_CW"                           . (constant "GLenum"))
   '( "GLU_CCW"                          . (constant "GLenum"))
   '( "GLU_INTERIOR"                     . (constant "GLenum"))
   '( "GLU_EXTERIOR"                     . (constant "GLenum"))
   '( "GLU_UNKNOWN"                      . (constant "GLenum"))
   '( "GLU_TESS_ERROR1"                  . (constant "GLenum"))
   '( "GLU_TESS_ERROR2"                  . (constant "GLenum"))
   '( "GLU_TESS_ERROR3"                  . (constant "GLenum"))
   '( "GLU_TESS_ERROR4"                  . (constant "GLenum"))
   '( "GLU_TESS_ERROR5"                  . (constant "GLenum"))
   '( "GLU_TESS_ERROR6"                  . (constant "GLenum"))
   '( "GLU_TESS_ERROR7"                  . (constant "GLenum"))
   '( "GLU_TESS_ERROR8"                  . (constant "GLenum"))
   ;; Properties
   '( "GLU_AUTO_LOAD_MATRIX"             . (constant "GLenum"))
   '( "GLU_CULLING"                      . (constant "GLenum"))
   '( "GLU_SAMPLING_TOLERANCE"           . (constant "GLenum"))
   '( "GLU_DISPLAY_MODE"                 . (constant "GLenum"))
   ;; Trimming curve types
   '( "GLU_MAP1_TRIM_2"                  . (constant "GLenum"))
   '( "GLU_MAP1_TRIM_3"                  . (constant "GLenum"))
   ;; Display modes
   '( "GLU_OUTLINE_POLYGON"              . (constant "GLenum"))
   '( "GLU_OUTLINE_PATCH"                . (constant "GLenum"))
   ;; Callbacks
   ;; Errors
   '( "GLU_NURBS_ERROR1"                 . (constant "GLenum"))
   '( "GLU_NURBS_ERROR2"                 . (constant "GLenum"))
   '( "GLU_NURBS_ERROR3"                 . (constant "GLenum"))
   '( "GLU_NURBS_ERROR4"                 . (constant "GLenum"))
   '( "GLU_NURBS_ERROR5"                 . (constant "GLenum"))
   '( "GLU_NURBS_ERROR6"                 . (constant "GLenum"))
   '( "GLU_NURBS_ERROR7"                 . (constant "GLenum"))
   '( "GLU_NURBS_ERROR8"                 . (constant "GLenum"))
   '( "GLU_NURBS_ERROR9"                 . (constant "GLenum"))
   '( "GLU_NURBS_ERROR10"                . (constant "GLenum"))
   '( "GLU_NURBS_ERROR11"                . (constant "GLenum"))
   '( "GLU_NURBS_ERROR12"                . (constant "GLenum"))
   '( "GLU_NURBS_ERROR13"                . (constant "GLenum"))
   '( "GLU_NURBS_ERROR14"                . (constant "GLenum"))
   '( "GLU_NURBS_ERROR15"                . (constant "GLenum"))
   '( "GLU_NURBS_ERROR16"                . (constant "GLenum"))
   '( "GLU_NURBS_ERROR17"                . (constant "GLenum"))
   '( "GLU_NURBS_ERROR18"                . (constant "GLenum"))
   '( "GLU_NURBS_ERROR19"                . (constant "GLenum"))
   '( "GLU_NURBS_ERROR20"                . (constant "GLenum"))
   '( "GLU_NURBS_ERROR21"                . (constant "GLenum"))
   '( "GLU_NURBS_ERROR22"                . (constant "GLenum"))
   '( "GLU_NURBS_ERROR23"                . (constant "GLenum"))
   '( "GLU_NURBS_ERROR24"                . (constant "GLenum"))
   '( "GLU_NURBS_ERROR25"                . (constant "GLenum"))
   '( "GLU_NURBS_ERROR26"                . (constant "GLenum"))
   '( "GLU_NURBS_ERROR27"                . (constant "GLenum"))
   '( "GLU_NURBS_ERROR28"                . (constant "GLenum"))
   '( "GLU_NURBS_ERROR29"                . (constant "GLenum"))
   '( "GLU_NURBS_ERROR30"                . (constant "GLenum"))
   '( "GLU_NURBS_ERROR31"                . (constant "GLenum"))
   '( "GLU_NURBS_ERROR32"                . (constant "GLenum"))
   '( "GLU_NURBS_ERROR33"                . (constant "GLenum"))
   '( "GLU_NURBS_ERROR34"                . (constant "GLenum"))
   '( "GLU_NURBS_ERROR35"                . (constant "GLenum"))
   '( "GLU_NURBS_ERROR36"                . (constant "GLenum"))
   '( "GLU_NURBS_ERROR37"                . (constant "GLenum"))
   ;;
   ;; GL/glu.h Types ;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   '( "GLUquadricObj"                    . (typedef "struct GLUquadricObj"))
   '( "GLUtriangulatorObj"               . (typedef "struct GLUtriangulatorObj"))
   '( "GLUnurbsObj"                      . (typedef "struct GLUnurbsObj"))
   ;;
   ;; GL/glu.h Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   '( "gluErrorString"                   . (Function "Const GLubyte *" "GLenum errorCode"))
   '( "gluOrtho2D"                       . (function "void" "GLdouble left, GLdouble right, GLdouble bottom, GLdouble top"))
   '( "gluPerspective"                   . (function "void" "GLdouble fovy, GLdouble aspect, GLdouble zNear, GLdouble zFar"))
   '( "gluPickMatrix"                    . (function "void" "GLdouble x, GLdouble y, GLdouble width, GLdouble height, GLint viewport[4]"))
   '( "gluLookAt"                        . (function "void" "GLdouble eyex, GLdouble eyey, GLdouble eyez, GLdouble centerx, GLdouble centery, GLdouble centerz, GLdouble upx, GLdouble upy, GLdouble upz"))
   '( "gluProject"                       . (function "int"  "GLdouble objx, GLdouble objy, GLdouble objz, const GLdouble modelMatrix[16], const GLdouble projMatrix[16], const GLint viewport[4], GLdouble *winx, GLdouble *winy, GLdouble *winz"))
   '( "gluUnProject"                     . (function "int"  "GLdouble winx, GLdouble winy, GLdouble winz, const GLdouble modelMatrix[16], const GLdouble projMatrix[16], const GLint viewport[4], GLdouble *objx, GLdouble *objy, GLdouble *objz"))
   '( "gluScaleImage"                    . (function "int"  "GLenum format, GLint widthin, GLint heightin, GLenum typein, const void *datain, GLint widthout, GLint heightout, GLenum typeout, void *dataout"))
   '( "gluBuild1DMipmaps"                . (function "int"  "GLenum target, GLint components, GLint width, GLenum format, GLenum type, const void *data"))
   '( "gluBuild2DMipmaps"                . (function "int"  "GLenum target, GLint components, GLint width, GLint height, GLenum format, GLenum type, const void *data"))
   '( "gluNewQuadric"                    . (Function "GLUquadricObj *" "void"))
   '( "gluDeleteQuadric"                 . (function "void" "GLUquadricObj *state"))
   '( "gluQuadricNormals"                . (function "void" "GLUquadricObj *quadObject, GLenum normals"))
   '( "gluQuadricTexture"                . (function "void" "GLUquadricObj *quadObject, GLboolean textureCoords"))
   '( "gluQuadricOrientation"            . (function "void" "GLUquadricObj *quadObject, GLenum orientation"))
   '( "gluQuadricDrawStyle"              . (function "void" "GLUquadricObj *quadObject, GLenum drawStyle"))
   '( "gluCylinder"                      . (function "void" "GLUquadricObj *qobj, GLdouble baseRadius, GLdouble topRadius, GLdouble height, GLint slices, GLint stacks"))
   '( "gluDisk"                          . (function "void" "GLUquadricObj *qobj, GLdouble innerRadius, GLdouble outerRadius, GLint slices, GLint loops"))
   '( "gluPartialDisk"                   . (function "void" "GLUquadricObj *qobj, GLdouble innerRadius, GLdouble outerRadius, GLint slices, GLint loops, GLdouble startAngle, GLdouble sweepAngle"))
   '( "gluSphere"                        . (function "void" "GLUquadricObj *qobj, GLdouble radius, GLint slices, GLint stacks"))
   '( "gluQuadricCallback"               . (function "void" "GLUquadricObj *qobj, GLenum which, void (*fn)()"))
   '( "gluNewTess"                       . (Function "GLUTriangulatorObj *" "void"))
   '( "gluTessCallback"                  . (function "void" "GLUtriangulatorObj *tobj, GLenum which, void (*fn)()"))
   '( "gluDeleteTess"                    . (function "void" "GLUtriangulatorObj *tobj"))
   '( "gluBeginPolygon"                  . (function "void" "GLUtriangulatorObj *tobj"))
   '( "gluEndPolygon"                    . (function "void" "GLUtriangulatorObj *tobj"))
   '( "gluNextContour"                   . (function "void" "GLUtriangulatorObj *tobj, GLenum type"))
   '( "gluTessVertex"                    . (function "void" "GLUtriangulatorObj *tobj, GLdouble v[3], void *data"))
   '( "gluNewNurbsRenderer"              . (function "GLUnurbsObj*" "void"))
   '( "gluDeleteNurbsRenderer"           . (function "void" "GLUnurbsObj *nobj"))
   '( "gluBeginSurface"                  . (function "void" "GLUnurbsObj *nobj"))
   '( "gluBeginCurve"                    . (function "void" "GLUnurbsObj *nobj"))
   '( "gluEndCurve"                      . (function "void" "GLUnurbsObj *nobj"))
   '( "gluEndSurface"                    . (function "void" "GLUnurbsObj *nobj"))
   '( "gluBeginTrim"                     . (function "void" "GLUnurbsObj *nobj"))
   '( "gluEndTrim"                       . (function "void" "GLUnurbsObj *nobj"))
   '( "gluPwlCurve"                      . (function "void" "GLUnurbsObj *nobj, GLint count, GLfloat *array, GLint stride, GLenum type"))
   '( "gluNurbsCurve"                    . (function "void" "GLUnurbsObj *nobj, GLint nknots, GLfloat *knot, GLint stride, GLfloat *ctlarray, GLint order, GLenum type"))
   '( "gluNurbsSurface"                  . (function "void" "GLUnurbsObj *nobj, GLint sknot_count, GLfloat *sknot, GLint tknot_count, GLfloat *tknot, GLint s_stride, GLint t_stride, GLfloat *ctlarray, GLint sorder, GLint torder, GLenum type"))
   '( "gluLoadSamplingMatrices"          . (function "void" "GLUnurbsObj *nobj, const GLfloat modelMatrix[16], const GLfloat projMatrix[16], const GLint viewport[4]"))
   '( "gluNurbsProperty"                 . (function "void" "GLUnurbsObj *nobj, GLenum property, GLfloat value"))
   '( "gluGetNurbsProperty"              . (function "void" "GLUnurbsObj *nobj, GLenum property, GLfloat *value"))
   '( "gluNurbsCallback"                 . (function "void" "GLUnurbsObj *nobj, GLenum which, void (*fn)()"))
   ;;
   ;; GL/glx.h Constants ;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   ;; 
   ;; Names for attributes to glXGetConfig.
   '( "GLX_USE_GL"                       . (constant "int"))
   '( "GLX_BUFFER_SIZE"                  . (constant "int"))
   '( "GLX_LEVEL"                        . (constant "int"))
   '( "GLX_RGBA"                         . (constant "int"))
   '( "GLX_DOUBLEBUFFER"                 . (constant "int"))
   '( "GLX_STEREO"                       . (constant "int"))
   '( "GLX_AUX_BUFFERS"                  . (constant "int"))
   '( "GLX_RED_SIZE"                     . (constant "int"))
   '( "GLX_GREEN_SIZE"                   . (constant "int"))
   '( "GLX_BLUE_SIZE"                    . (constant "int"))
   '( "GLX_ALPHA_SIZE"                   . (constant "int"))
   '( "GLX_DEPTH_SIZE"                   . (constant "int"))
   '( "GLX_STENCIL_SIZE"                 . (constant "int"))
   '( "GLX_ACCUM_RED_SIZE"               . (constant "int"))
   '( "GLX_ACCUM_GREEN_SIZE"             . (constant "int"))
   '( "GLX_ACCUM_BLUE_SIZE"              . (constant "int"))
   '( "GLX_ACCUM_ALPHA_SIZE"             . (constant "int"))
   ;; Error return values from glXGetConfig
   '( "GLX_BAD_SCREEN"                   . (constant "int"))
   '( "GLX_BAD_ATTRIBUTE"                . (constant "int"))
   '( "GLX_NO_EXTENSION"                 . (constant "int"))
   '( "GLX_BAD_VISUAL"                   . (constant "int"))
   ;; protocol error codes
   '( "GLXBadContext"                    . (constant "int"))
   '( "GLXBadContextState"               . (constant "int"))
   '( "GLXBadDrawable"                   . (constant "int"))
   '( "GLXBadPixmap"                     . (constant "int"))
   '( "GLXBadContextTag"                 . (constant "int"))
   '( "GLXBadCurrentWindow"              . (constant "int"))
   '( "GLXBadRenderRequest"              . (constant "int"))
   '( "GLXBadLargeRequest"               . (constant "int"))
   '( "GLXUnsupportedPrivateRequest"     . (constant "int"))
   ;;
   ;; GL/glx.h Types ;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   ;; Ressources
   '( "GLXContextID"                     . (typedef "XID"))
   '( "GLXPixmap"                        . (typedef "XID"))
   '( "GLXDrawable"                      . (typedef "XID"))
   ;; Context
   '( "GLXContext"                       . (typedef "struct __GLXcontextRec *"))
   ;;
   ;;
   ;; GL/glx.h Functions ;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   '( "glXChooseVisual"                  . (function "XVisualInfo *" "Display *dpy, int screen, int *attribList"))
   '( "glXCopyContext"                   . (function "void" "Display *dpy, GLXContext src, GLXContext dst, GLuint mask"))
   '( "glXCreateContext"                 . (function "GLXContext" "Display *dpy, XVisualInfo *vis, GLXContext shareList, Bool direct"))
   '( "glXCreateGLXPixmap"               . (function "GLXPixmap" "Display *dpy, XVisualInfo *vis, Pixmap pixmap"))
   '( "glXDestroyContext"                . (function "void" "Display *dpy, GLXContext ctx"))
   '( "glXDestroyGLXPixmap"              . (function "void" "Display *dpy, GLXPixmap pix"))
   '( "glXGetConfig"                     . (function "int" "Display *dpy, XVisualInfo *vis, int attrib, int *value"))
   '( "glXGetCurrentContext"             . (function "GLXContext" "void"))
   '( "glXGetCurrentDrawable"            . (function "GLXDrawable" "void"))
   '( "glXIsDirect"                      . (function "Bool" "Display *dpy, GLXContext ctx"))
   '( "glXMakeCurrent"                   . (function "Bool" "Display *dpy, GLXDrawable drawable, GLXContext ctx"))
   '( "glXQueryExtension"                . (function "Bool" "Display *dpy, int *errorBase, int *eventBase"))
   '( "glXQueryVersion"                  . (function "Bool" "Display *dpy, int *major, int *minor"))
   '( "glXSwapBuffers"                   . (function "void" "Display *dpy, GLXDrawable drawable"))
   '( "glXUseXFont"                      . (function "void" "Font font, int first, int count, int listBase"))
   '( "glXWaitGL"                        . (function "void" "void"))
   '( "glXWaitX"                         . (function "void" "void"))
   ;;
   ;; GL/glut.h Constants ;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   ;; GLUT API revision history:
   '( "GLUT_API_VERSION"                 . (constant "int"))
   ;; GLUT implementation revision history
   '( "GLUT_XLIB_IMPLEMENTATION"         . (constant "int"))
   ;; display mode bit masks
   '( "GLUT_RGB"                         . (constant "int"))
   '( "GLUT_RGBA"                        . (constant "int"))
   '( "GLUT_INDEX"                       . (constant "int"))
   '( "GLUT_SINGLE"                      . (constant "int"))
   '( "GLUT_DOUBLE"                      . (constant "int"))
   '( "GLUT_ACCUM"                       . (constant "int"))
   '( "GLUT_ALPHA"                       . (constant "int"))
   '( "GLUT_DEPTH"                       . (constant "int"))
   '( "GLUT_STENCIL"                     . (constant "int"))
   '( "GLUT_OVERLAY"                     . (constant "int"))
   ;; mouse buttons
   '( "GLUT_LEFT_BUTTON"                 . (constant "int"))
   '( "GLUT_MIDDLE_BUTTON"               . (constant "int"))
   '( "GLUT_RIGHT_BUTTON"                . (constant "int"))
   ;; mouse button callback state
   '( "GLUT_DOWN"                        . (constant "int"))
   '( "GLUT_UP"                          . (constant "int"))
   ;; entry/exit callback state
   '( "GLUT_LEFT"                        . (constant "int"))
   '( "GLUT_ENTERED"                     . (constant "int"))
   ;; menu usage callback state
   '( "GLUT_MENU_NOT_IN_USE"             . (constant "int"))
   '( "GLUT_MENU_IN_USE"                 . (constant "int"))
   ;; visibility callback state
   '( "GLUT_NOT_VISIBLE"                 . (constant "int"))
   '( "GLUT_VISIBLE"                     . (constant "int"))
   ;; color index component selection values
   '( "GLUT_RED"                         . (constant "int"))
   '( "GLUT_GREEN"                       . (constant "int"))
   '( "GLUT_BLUE"                        . (constant "int"))
   ;; stroke font constants
   '( "GLUT_STROKE_ROMAN"                . (constant "void *"))
   '( "GLUT_STROKE_MONO_ROMAN"           . (constant "void *"))
   ;; bitmap font constants  
   '( "GLUT_BITMAP_9_BY_15"              . (constant "void *"))
   '( "GLUT_BITMAP_8_BY_13"              . (constant "void *"))
   '( "GLUT_BITMAP_TIMES_ROMAN_10"       . (constant "void *"))
   '( "GLUT_BITMAP_TIMES_ROMAN_24"       . (constant "void *"))
   ;; glutGet parameters
   '( "GLUT_WINDOW_X"                    . (constant "GLenum"))
   '( "GLUT_WINDOW_Y"                    . (constant "GLenum"))
   '( "GLUT_WINDOW_WIDTH"                . (constant "GLenum"))
   '( "GLUT_WINDOW_HEIGHT"               . (constant "GLenum"))
   '( "GLUT_WINDOW_BUFFER_SIZE"          . (constant "GLenum"))
   '( "GLUT_WINDOW_STENCIL_SIZE"         . (constant "GLenum"))
   '( "GLUT_WINDOW_DEPTH_SIZE"           . (constant "GLenum"))
   '( "GLUT_WINDOW_RED_SIZE"             . (constant "GLenum"))
   '( "GLUT_WINDOW_GREEN_SIZE"           . (constant "GLenum"))
   '( "GLUT_WINDOW_BLUE_SIZE"            . (constant "GLenum"))
   '( "GLUT_WINDOW_ALPHA_SIZE"           . (constant "GLenum"))
   '( "GLUT_WINDOW_ACCUM_RED_SIZE"       . (constant "GLenum"))
   '( "GLUT_WINDOW_ACCUM_GREEN_SIZE"     . (constant "GLenum"))
   '( "GLUT_WINDOW_ACCUM_BLUE_SIZE"      . (constant "GLenum"))
   '( "GLUT_WINDOW_ACCUM_ALPHA_SIZE"     . (constant "GLenum"))
   '( "GLUT_WINDOW_DOUBLEBUFFER"         . (constant "GLenum"))
   '( "GLUT_WINDOW_RGBA"                 . (constant "GLenum"))
   '( "GLUT_WINDOW_PARENT"               . (constant "GLenum"))
   '( "GLUT_WINDOW_NUM_CHILDREN"         . (constant "GLenum"))
   '( "GLUT_WINDOW_COLORMAP_SIZE"        . (constant "GLenum"))
   '( "GLUT_SCREEN_WIDTH"                . (constant "GLenum"))
   '( "GLUT_SCREEN_HEIGHT"               . (constant "GLenum"))
   '( "GLUT_SCREEN_WIDTH_MM"             . (constant "GLenum"))
   '( "GLUT_SCREEN_HEIGHT_MM"            . (constant "GLenum"))
   '( "GLUT_MENU_NUM_ITEMS"              . (constant "GLenum"))
   '( "GLUT_DISPLAY_MODE_POSSIBLE"       . (constant "GLenum"))
   '( "GLUT_INIT_WINDOW_X"               . (constant "GLenum"))
   '( "GLUT_INIT_WINDOW_Y"               . (constant "GLenum"))
   '( "GLUT_INIT_WINDOW_WIDTH"           . (constant "GLenum"))
   '( "GLUT_INIT_WINDOW_HEIGHT"          . (constant "GLenum"))
   '( "GLUT_INIT_DISPLAY_MODE"           . (constant "GLenum"))
   ;;
   ;; GL/glut.h Types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   '( "GLUTcolorcell"                    . (typedef "struct _GLUTcolorcell"))
   '( "GLUTcolormap"                     . (typedef "struct _GLUTcolormap"))
   '( "GLUTwindow"                       . (typedef "struct _GLUTwindow"))
   '( "GLUTmenu"                         . (typedef "struct _GLUTmenu"))
   '( "GLUTmenuEntry"                    . (typedef "struct _GLUTmenuEntry"))
   '( "GLUTtimer"                        . (typedef "struct _GLUTtimer"))
   ;;
   ;; GL/glut.h Functions ;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   ;; GLUT initialization
   '( "glutInit"                         . (function "void" "int *, char **"))
   '( "glutInitDisplayMode"              . (function "void" "unsigned long"))
   '( "glutInitWindowPosition"           . (function "void" "int, int"))
   '( "glutInitWindowSize"               . (function "void" "int, int"))
   '( "glutMainLoop"                     . (function "void" "void"))
   ;; GLUT window
   '( "glutCreateWindow"                 . (function "int"  "char *"))
   '( "glutCreateSubWindow"              . (function "void" "int, int, int, int, int"))
   '( "glutDestroyWindow"                . (function "void" "int"))
   '( "glutPostRedisplay"                . (function "void" "void"))
   '( "glutSwapBuffers"                  . (function "void" "void"))
   '( "glutGetWindow"                    . (function "int"  "void"))
   '( "glutSetWindow"                    . (function "void" "int"))
   '( "glutSetWindowTitle"               . (function "void" "char *"))
   '( "glutSetIconTitle"                 . (function "void" "char *"))
   '( "glutPositionWindow"               . (function "void" "int, int"))
   '( "glutReshapeWindow"                . (function "void" "int, int"))
   '( "glutPopWindow"                    . (function "void" "void"))
   '( "glutPushWindow"                   . (function "void" "void"))
   '( "glutIconifyWindow"                . (function "void" "void"))
   '( "glutShowWindow"                   . (function "void" "void"))
   '( "glutHideWindow"                   . (function "void" "void"))
   ;; GLUT menu
   '( "glutCreateMenu"                   . (function "int"  "void (*)(int)"))
   '( "glutDestroyMenu"                  . (function "void" "int"))
   '( "glutGetMenu"                      . (function "int"  "void"))
   '( "glutSetMenu"                      . (function "void" "int"))
   '( "glutAddMenuEntry"                 . (function "void" "char *, int"))
   '( "glutAddSubMenu"                   . (function "void" "char *, int"))
   '( "glutChangeToMenuEntry"            . (function "void" "int, char *, int"))
   '( "glutChangeToSubMenu"              . (function "void" "int, char *, int"))
   '( "glutRemoveMenuItem"               . (function "void" "int"))
   '( "glutAttachMenu"                   . (function "void" "int"))
   '( "glutDetachMenu"                   . (function "void" "int"))
   ;; GLUT callback
   '( "glutDisplayFunc"                  . (function "void" "void (*)(void)"))
   '( "glutReshapeFunc"                  . (function "void" "void (*)(int, int)"))
   '( "glutKeyboardFunc"                 . (function "void" "void (*)(unsigned char, int, int)"))
   '( "glutMouseFunc"                    . (function "void" "void (*)(int, int, int, int)"))
   '( "glutMotionFunc"                   . (function "void" "void (*)(int, int)"))
   '( "glutPassiveMotionFunc"            . (function "void" "void (*)(int, int)"))
   '( "glutEntryFunc"                    . (function "void" "void (*)(int)"))
   '( "glutVisibilityFunc"               . (function "void" "void (*)(int)"))
   '( "glutIdleFunc"                     . (function "void" "void (*)(void)"))
   '( "glutTimerFunc"                    . (function "void" "unsigned long, void (*)(int), int"))
   '( "glutMenuStateFunc"                . (function "void" "void (*)(int)"))
   ;; GLUT color index
   '( "glutSetColor"                     . (function "void" "int, GLfloat, GLfloat, GLfloat"))
   '( "glutGetColor"                     . (function "GLfloat" "int, int"))
   '( "glutCopyColormap"                 . (function "void" "int"))
   ;; GLUT state retrieval
   '( "glutGet"                          . (function "int"  "GLenum"))
   ;; GLUT font
   '( "glutStrokeCharacter"              . (function "void" "void *, int"))
   '( "glutBitmapCharacter"              . (function "void" "void *, int"))
   ;; GLUT pre-built models
   '( "glutWireSphere"                   . (function "void" "GLdouble, GLint, GLint"))
   '( "glutSolidSphere"                  . (function "void" "GLdouble, GLint, GLint"))
   '( "glutWireCone"                     . (function "void" "GLdouble, GLdouble, GLint, GLint"))
   '( "glutSolidCone"                    . (function "void" "GLdouble, GLdouble, GLint, GLint"))
   '( "glutWireCube"                     . (function "void" "GLdouble"))
   '( "glutSolidCube"                    . (function "void" "GLdouble"))
   '( "glutWireTorus"                    . (function "void" "GLdouble, GLdouble, GLint, GLint"))
   '( "glutSolidTorus"                   . (function "void" "GLdouble, GLdouble, GLint, GLint"))
   '( "glutWireDodecahedron"             . (function "void" "void"))
   '( "glutSolidDodecahedron"            . (function "void" "void"))
   '( "glutWireTeapot"                   . (function "void" "GLdouble"))
   '( "glutSolidTeapot"                  . (function "void" "GLdouble"))
   '( "glutWireOctahedron"               . (function "void" "void"))
   '( "glutSolidOctahedron"              . (function "void" "void"))
   '( "glutWireTetrahedron"              . (function "void" "void"))
   '( "glutSolidTetrahedron"             . (function "void" "void"))
   '( "glutWireIcosahedron"              . (function "void" "void"))
   '( "glutSolidIcosahedron"             . (function "void" "void"))
   ) 
  "AList of ALL OpenGL symbols with infos associated.

 Each element look like (SYMBOL . (KIND PARAMETERS))

 SYMBOL is a string name of the symbol.
 KIND could be 'function, 'constant or 'typedef.
 PARAMETERS are, if KIND is:
  'function: PARAM1 is the return type, PARAM2 are the parameters of the function.
  'constant: PARAM1 is the type of the function.
  'typedef : PARAM1 is the base type of the typedef.
"
  )



;; list of all major modes OpenGL minor mode could work with.
(defconst ogl--major-modes-allowed '(c-mode c++-mode))


;; nil in GNU FSF Emacs, >= 0 in GNU Lucid Emacs/XEmacs
(defconst ogl--lucid-emacs-p (or (string-match "Lucid"  emacs-version)
                                 (string-match "XEmacs" emacs-version)))


;; OpenGL symbols completion history
(defvar ogl--completion-hist nil)



;;; Internal functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro ogl--non-empty-string-p (s)
  ;; Check if string s is non nil and non empty
  (` (and (, s) (not (equal (, s) "")))))


(defmacro  ogl--symbol-infos-string (sinf sname)
  ;; Return string informations about the OpenGL symbol infos.
  (` (let ((type (car-safe (cdr-safe (, sinf)))))
       (cond
        ((eq type 'constant)
         (format "OpenGL Constant: const %s %s = ...;"
                 (nth 1 (cdr (, sinf))) (, sname)))
        ((eq type 'typedef)
         (format "OpenGL Type: typedef %s %s;"
                 (nth 1 (cdr (, sinf))) (, sname)))
        ((eq type 'function)
         (format "OpenGL Function: %s %s (%s);"
                 (nth 1 (cdr (, sinf))) (, sname)
                 (nth 2 (cdr (, sinf)))))
        (t
         (format "Unknown OpenGL symbol: %s" (, sname)))
        ))))


(defsubst  ogl--try-completion (prefix &optional msg)
  ;; Ask user completion and write information message
  (let ((symb (completing-read "OpenGL symbol: "
                               OpenGL-symbols
                               nil t prefix
                               'ogl--completion-hist)))
    (if (and msg (ogl--non-empty-string-p symb))
        ;; Print symbol infos
        (message (ogl--symbol-infos-string (assoc symb OpenGL-symbols) symb)))
    symb))


(defmacro ogl--indent-region (beg end)
  ;; Indent region according to mode
  (` (let ((nl (count-lines (, beg) (, end)))
           (l  1))
       (save-excursion
         (goto-char (, beg))
         (indent-according-to-mode)
         (while (< l nl)
           (next-line 1)
           (indent-according-to-mode)
           (setq l (+ l 1)))))))


(defmacro ogl--insert-maybe-newline ()
  ;; Insert newline at point if the current line is not empty
  (` (let ((ins nil))
       (save-excursion
         (skip-chars-backward " \t")
         (setq ins (not (bolp))))
       (if ins (insert "\n")))))


(defun ogl--init ()
  ;; Initialisation function for OpenGL minor mode
  (cond (ogl--lucid-emacs-p
         (set-buffer-menubar (copy-sequence current-menubar))
         (if OpenGL-add-menu-before
             (add-menu nil (car OpenGL-menu-bar) (cdr OpenGL-menu-bar) OpenGL-add-menu-before)))))


(defun ogl--shutdown ()
  ;; Shutdown function for OpenGL minor mode
  (cond (ogl--lucid-emacs-p
         (set-buffer-menubar (delete OpenGL-menu-bar current-menubar)))))



;;;; Load time setup routines ;;;;;


;; Keymaps and menues
(unless OpenGL-minor-mode-map
  ;; OpenGL keymap is small
  (setq OpenGL-minor-mode-map (make-sparse-keymap))
  (if ogl--lucid-emacs-p
      (setq OpenGL-menu-bar
            '("OpenGL"
              ["Complete symbol"  OpenGL-complete-symbol         t]
              ["Describe symbol"  OpenGL-describe-symbol-briefly t]
              "-----"
              ["New List"         OpenGL-new-list                t]
              ["New Object"       OpenGL-new-object              t]
	      ["Matrix push/pop"  OpenGL-matrix-push-pop         t]
	      ["glEnable/disable" OpenGL-enable-disable          t]))
    ;; GNU FSF Emacs
    (define-key OpenGL-minor-mode-map [menu-bar OpenGL]
      (cons "OpenGL" (make-sparse-keymap "OpenGL")))
    (define-key OpenGL-minor-mode-map [menu-bar OpenGL OpenGL-describe-symbol-briefly]
      '("Describe symbol" . OpenGL-describe-symbol-briefly))
    (define-key OpenGL-minor-mode-map [menu-bar OpenGL OpenGL-complete-symbol]
      '("Complete Symbol" . OpenGL-complete-symbol))
    (define-key OpenGL-minor-mode-map [menu-bar OpenGL OpenGL-new-list]
      '("New List"        . OpenGL-new-list))
    (define-key OpenGL-minor-mode-map [menu-bar OpenGL OpenGL-new-object]
      '("New Object"      . OpenGL-new-object))
    (define-key OpenGL-minor-mode-map [menu-bar OpenGL OpenGL-matrix-push-pop]
      '("Matrix push/pop" . OpenGL-matrix-push-pop))
    (define-key OpenGL-minor-mode-map [menu-bar OpenGL OpenGL-enable-disable]
      '("glEnable/glDisable block" . OpenGL-enable-disable))
  ))


;; Minor modes inits
(pushnew (cons 'OpenGL-minor-mode OpenGL-minor-mode-map)
         minor-mode-map-alist :test 'equal)
(pushnew '(OpenGL-minor-mode " OpenGL")
         minor-mode-alist     :test 'equal)


;; Local variables
(make-variable-buffer-local 'OpenGL-minor-mode)
(put 'OpenGL-minor-mode 'permanent-local t)



;;; User functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun  OpenGL-complete-symbol ()
  "*Completion of OpenGL symbols.
Read completion prefix from the buffer and ask user to complete it, then
the completed symbol is inserted at point into the buffer.
Print syntax of the symbol in minibuffer.
" 
  (interactive)
  (let ((end (point))
        (compl)
        (prfx)
        (beg))
    (save-excursion
      (skip-chars-backward "a-zA-Z0-9_")
      (setq beg   (point))
      (setq prfx  (buffer-substring beg end))
      ;; OpenGL symbols completion
      (setq compl (ogl--try-completion prfx t)))
    (if (ogl--non-empty-string-p compl)
        (if (ogl--non-empty-string-p prfx)
            ;; Kill old prefix and replace it by full completion
            (progn
	      (kill-region beg end)
	      (insert compl))))))
;; there was a small bug there.  the insertion was getting done outside
;; (if (... prfx), in effect becoming the else for the (if (... compl))

;;;###autoload
(defun  OpenGL-describe-symbol-briefly ()
  "*Short description of an OpenGL symbol.
Print syntax of the symbol in minibuffer."
  (interactive)
  (let ((prfx))
    (save-excursion
      (if (re-search-backward "\\<\\([gG][lL][a-zA-Z0-9_]+\\)[^a-zA-Z0-9_]?"
                              nil t nil)
          (setq prfx (buffer-substring
                      (match-beginning 1)
                      (match-end       1))))
      ;; OpenGL symbols completion
      (ogl--try-completion prfx t))))

;;;###autoload
(defun my-region-beginning ()
  " this is the same as (region-beginning) only that it doesnt blow up
on your face if the mark isnt active.  in that case, it just returns
point, because one way of looking at it is a region with the beginning
and the end at point"

  (interactive)
  (if mark-active
      (if (< (mark) (point))
	  (mark)
	(point))
    (point)))

(defun my-region-end ()
  " this is the same as (region-end) only that it doesnt blow up
on your face if the mark isnt active.  in that case, it just returns
point, because one way of looking at it is a region with the beginning
and the end at point"
  
  (interactive)
  (if mark-active
      (if (> (mark) (point))
	  (mark)
	(point))
    (point)))

;; we notice that all OpenGL-* funcs that insert frame have a similar
;; structure.  they all insert a "header" followed by "{..}" and then
;; a "footer".  this is meant for use by other OpenGL-* funcs, and
;; *not* explicitly by the user.  This func returns a pair of markers
;; marking the beginning and end of the inserted frame.  The beginning
;; frame has insertion type nil, and the end marker has insertion type
;; t.  The returned markers can be used to reposition the point.
;; the following are valid inputs for inactive-pos
;;	0 : leave point where it is
;;	1 : put point on same line as beg-str immediately foll. first "("
;; 	2 : put point on the next line
;; the following are valid inputs for active-pos
;;	0 : leave point where it is
;;	1 : put point on same line as beg-str immediately foll. first "("
;;	2 : put point at beg of line following end-str
;;	3 : insert new line following end-str and put point there.
(defun OpenGL-generic-insert (beg-str end-str inactive-pos active-pos)
  (ogl--insert-maybe-newline)
  (let* ((beg-marker (make-marker))
	 (end-marker (make-marker)))
    (set-marker beg-marker (my-region-beginning))
    (set-marker end-marker (my-region-end))
    (set-marker-insertion-type beg-marker nil)
    (set-marker-insertion-type end-marker t)
    (save-excursion       
      (goto-char (marker-position beg-marker))
      (insert
       beg-str
       "{\n"
       )
      (goto-char (marker-position end-marker))
      (when (not mark-active)
	(insert "\t\n"))
      (insert
       "}\n"
       end-str))
    (if mark-active
	(cond
	 ((eq active-pos 0)
	  (goto-char (marker-position beg-marker)))
	 ((eq active-pos 1)
	  (goto-char (+ (marker-position beg-marker)
			(+ (string-match "(" beg-str) 1))))
	 ((eq active-pos 2)
	  (goto-char (marker-position end-marker)))
	 ((eq active-pos 3)
	  (progn
	    (goto-char (marker-position end-marker))
	    (insert "\n")
	    (forward-line))))
      (cond
       ((eq active-pos 0)
	(goto-char (marker-position beg-marker)))
       ((eq inactive-pos 1)
	(goto-char (+ (marker-position beg-marker)
		      (+ (string-match "(" beg-str) 1))))
       ((eq inactive-pos 2)
	(prog2
	    (goto-char (marker-position beg-marker))
	    (forward-line 2)))))
    (ogl--indent-region (marker-position beg-marker)
			(marker-position end-marker))
    (cons beg-marker end-marker)))

(defun  OpenGL-new-object()
  "Insert an empty glBegin()/glEnd() frame to edit.  If mark is
active, then the marked region is enclosed in a new glBegin()/glEnd()
frame.  In both cases, the point is placed in the correct place to
type the GL_ constant."
  (interactive)
  (OpenGL-generic-insert "glBegin();\n" "glEnd();\n" 1 1)
  (insert "GL_"))

;; insert a new glPushMatrix/glPopMatrix block
(defun OpenGL-matrix-push-pop ()
  "Insert a glPushMatrix()/glPopMatrix() frame to edit"
  (interactive)
  (OpenGL-generic-insert "glPushMatrix();\n" "glPopMatrix();\n"
			 2 0))

;; insert a new glEnable()/glDisable block, and place point inside the
;; enable parans.
(defun OpenGL-enable-disable (name)
  "Insert a glEnable()/glDisable() frame to edit.  takes one argument
interactively to insert between the parans of glEnable() and the
gDisable() statements."
  (interactive "MName of GL_ constant: GL_")
  (let ((st (concat "GL_" name)))
    (OpenGL-generic-insert (concat "glEnable(" st ");\n")
			   (concat "glDisable(" st ");\n")
			   2 0)))

;;;###autoload
(defun  OpenGL-new-list ()
  "*Insert a glNewList/glEndList frame to edit."
  (interactive)
  (OpenGL-generic-insert "glNewList( , GL_COMPILE);\n"
			 "glEndList();\n"
			 1 1))

;;;###autoload
(defun OpenGL-minor-mode (&optional arg)
  "Toggle OpenGL minor mode.
With ARG > 0, turn OpenGL mode on, turn it off otherwise.

OpenGL minor-mode provides some commands to edit OpenGL based
C/C++ programs:
`OpenGL-complete-symbol' interactive completion of OpenGL symbols.
`OpenGL-describe-symbol-briefly' short description of OpenGL symbols.
`OpenGL-new-list' frame for editing glNewList/glEndList.
`OpenGL-new-object' frame for editing glBegin/glEnd.
`OpenGL-matrix-push-pop' frame for editing glPushMatrix()/glPopMatrix()
`OpenGL-enable-disable' frame for editing glEnable(...)/glDisable(...)
`OpenGL-setup-keys' provide a set of default key bindings.

When OpenGL minor mode is on, the menu bar is augmented with OpenGL
commands and the OpenGL commands are enabled. lastly, the hooks set in
`OpenGL-minor-mode-hook' are called.

Turning OpenGL minor mode off reverts the menu bar and disables the
OpenGL commands.
"
  (interactive "P")
  ;; Toggle OpenGL-minor-mode on/off
  (setq OpenGL-minor-mode
        (if (null arg)
            (not OpenGL-minor-mode)
          (> (prefix-numeric-value arg) 0)))
  (if OpenGL-minor-mode
      ;; Check major-mode 
      (if (not (memq major-mode ogl--major-modes-allowed))
	  (error "No OpenGL implementation for %s mode" mode-name))
    
    ;; Toggled on
    (progn
      (ogl--init)
      (run-hooks 'OpenGL-minor-mode-hook)
      )
    ;; Toggled off
    (ogl--shutdown)))


;;;###autoload
(defun OpenGL-submit-bug-report ()
  "*Submit via mail a bug report on OpenGL v0.3."
  (interactive)
  (and
   (y-or-n-p "Do you REALLY want to submit a report on OpenGL-minor-mode? ")
   (require 'reporter)
   (reporter-submit-bug-report
    OpenGL-help-address
    (concat "OpenGL " OpenGL-minor-mode-version)
    (list
     ;; Interesting OpenGL variables
     'OpenGL-minor-mode-hook
     ;; others variables
     'features
     )
    nil
    nil
    "Dear Heddy,")))

;; provide some default key bindings in a function (somewhat in the find-func
;; style.
(defun OpenGL-setup-keys ()
  "Sets up some default key bindings for the OpenGL minor mode.  Best used with the OpenGL-minor-mode-hook."
  (OpenGL-minor-mode 1)
  (define-key OpenGL-minor-mode-map "\e\C-i"
    'OpenGL-complete-symbol)
  (define-key OpenGL-minor-mode-map "\C-ho"
    'OpenGL-describe-symbol-briefly)
  (define-key OpenGL-minor-mode-map "\C-cl"
    'OpenGL-new-list)
  (define-key OpenGL-minor-mode-map "\C-co"
    'OpenGL-new-object)
  (define-key OpenGL-minor-mode-map "\C-cm"
    'OpenGL-matrix-push-pop)
  (define-key OpenGL-minor-mode-map "\C-ce"
    'OpenGL-enable-disable))

;;;;;;;;;;;;;;;;;;;;;

(provide 'OpenGL)

;;; Local variables:
;;; hs-hide-all-on-load: nil
;;; End:

;;; OpenGL.el ends here
