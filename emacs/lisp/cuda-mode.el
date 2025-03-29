;;; cuda-mode.el --- NVIDIA CUDA Major Mode derived from C++-mode. -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Jack Morrison
;; Modified by Raayan Dhar for integration with modern C++ development.
;;
;; Author: Jack Morrison <jackmorrison1@gmail.com>
;;         Modified by Raayan Dhar
;; URL: https://github.com/chachi/cuda-mode
;; Keywords: c, languages, cuda
;; Version: 0.2
;;
;; Package-Requires: ((compat "29"))
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A major mode for editing CUDA code. It is derived from c++-mode so that you can
;; leverage modern C++ tools (such as lsp-mode, company, flycheck, etc.) with CUDA code.
;;
;; To use, place this file in your Emacs load-path (e.g., ~/.emacs.d/lisp/),
;; and add the following to your init file:
;;
;;   (load "cuda-mode.el" t)
;;   (add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))
;;   (add-to-list 'auto-mode-alist '("\\.cuh\\'" . cuda-mode))
;;
;; You can also add a hook for cuda-mode to tweak settings specific to CUDA.
;;
;;; Code:

(require 'compat)
(require 'cc-mode)

;; Load additional CC constants at compile time.
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

;; Make CUDA mode known to the language constant system.
(eval-and-compile
  (c-add-language 'cuda-mode 'c++-mode))

;; Define CUDA-specific keywords by extending the C++ constants.
(c-lang-defconst c-primitive-type-kwds
  "Primitive type keywords specific to CUDA.
These are fontified with the type face."
  cuda
  (append
   '("dim3"
     "char1" "uchar1" "char2" "uchar2" "char3" "uchar3" "char4" "uchar4"
     "short1" "ushort1" "short2" "ushort2" "short3" "ushort3" "short4" "ushort4"
     "int1" "uint1" "int2" "uint2" "int3" "uint3" "int4" "uint4"
     "long1" "ulong1" "long2" "ulong2" "long3" "ulong3" "long4" "ulong4"
     "float1" "float2" "float3" "float4"
     "double1" "double2")
   (c-lang-const c-primitive-type-kwds c++)))

(c-lang-defconst c-modifier-kwds
  cuda (append
	'("__host__" "__device__" "__global__")
	(c-lang-const c-modifier-kwds c++)))

(c-lang-defconst c-type-modifier-prefix-kwds
  cuda (append
	'("__device__" "__constant__" "__shared__" "__grid_constant__" "__managed__" "__restrict__")
	(c-lang-const c-type-modifier-prefix-kwds c++)))

(c-lang-defconst c-other-op-syntax-tokens
  "Tokens that are treated specially in CUDA mode."
  cuda (append
	'("<<<" ">>>")
	(c-lang-const c-other-op-syntax-tokens c++)))

(c-lang-defconst c-primary-expr-kwds
  "Primary expression keywords specific to CUDA."
  cuda '("gridDim" "blockIdx" "blockDim" "threadIdx" "warpSize"))

;; Define a list of CUDA built-in functions for completion and syntax highlighting.
(eval-and-compile
  (defvar cuda-builtins
    '(;; Atom functions
      "atomicAdd" "atomicAnd" "atomicCAS" "atomicDec" "atomicExch" "atomicInc"
      "atomicMax" "atomicMin" "atomicOr" "atomicSub" "atomicXor"
      ;; Address space predicates
      "__isGlobal" "__isShared" "__isConstant" "__isGridConstant" "__isLocal"
      ;; Texture functions and conversions
      "tex1D" "tex1Dfetch" "tex2D" "__float_as_int" "__int_as_float"
      "__float2int_rn" "__float2int_rz" "__float2int_ru" "__float2int_rd"
      "__float2uint_rn" "__float2uint_rz" "__float2uint_ru" "__float2uint_rd"
      "__int2float_rn" "__int2float_rz" "__int2float_ru" "__int2float_rd"
      "__uint2float_rn" "__uint2float_rz" "__uint2float_ru" "__uint2float_rd"
      ;; Additional math and utility functions
      "__fadd_rz" "__fmul_rz" "__fdividef" "__mul24" "__umul24"
      "__mulhi" "__umulhi" "__mul64hi" "__umul64hi"
      "min" "umin" "fminf" "fmin" "max" "umax" "fmaxf" "fmax"
      "abs" "fabsf" "fabs" "sqrtf" "sqrt" "sinf" "__sinf" "sin"
      "cosf" "__cosf" "cos" "sincosf" "__sincosf" "expf" "__expf" "exp"
      "logf" "__logf" "log"
      ;; Runtime and memory management
      "cudaBindTexture" "cudaBindTextureToArray" "cudaChooseDevice"
      "cudaConfigureCall" "cudaCreateChannelDesc" "cudaD3D10GetDevice"
      "cudaD3D10MapResources" "cudaD3D10RegisterResource"
      "cudaD3D10ResourceGetMappedArray" "cudaD3D10ResourceGetMappedPitch"
      "cudaD3D10ResourceGetMappedPointer" "cudaD3D10ResourceGetMappedSize"
      "cudaD3D10ResourceGetSurfaceDimensions" "cudaD3D10ResourceSetMapFlags"
      "cudaD3D10SetDirect3DDevice" "cudaD3D10UnmapResources"
      "cudaD3D10UnregisterResource" "cudaD3D9GetDevice" "cudaD3D9GetDirect3DDevice"
      "cudaD3D9MapResources" "cudaD3D9RegisterResource" "cudaD3D9ResourceGetMappedArray"
      "cudaD3D9ResourceGetMappedPitch" "cudaD3D9ResourceGetMappedPointer"
      "cudaD3D9ResourceGetMappedSize" "cudaD3D9ResourceGetSurfaceDimensions"
      "cudaD3D9ResourceSetMapFlags" "cudaD3D9SetDirect3DDevice" "cudaD3D9UnmapResources"
      "cudaD3D9UnregisterResource" "cudaEventCreate" "cudaEventDestroy"
      "cudaEventElapsedTime" "cudaEventQuery" "cudaEventRecord" "cudaEventSynchronize"
      "cudaFree" "cudaFreeArray" "cudaFreeHost" "cudaGetChannelDesc"
      "cudaGetDevice" "cudaGetDeviceCount" "cudaGetDeviceProperties"
      "cudaGetErrorString" "cudaGetLastError" "cudaGetSymbolAddress"
      "cudaGetSymbolSize" "cudaGetTextureAlignmentOffset" "cudaGetTextureReference"
      "cudaGLMapBufferObject" "cudaGLRegisterBufferObject" "cudaGLSetGLDevice"
      "cudaGLUnmapBufferObject" "cudaGLUnregisterBufferObject" "cudaLaunch"
      "cudaMalloc" "cudaMalloc3D" "cudaMalloc3DArray" "cudaMallocArray"
      "cudaMallocHost" "cudaMallocPitch" "cudaMemcpy" "cudaMemcpy2D"
      "cudaMemcpy2DArrayToArray" "cudaMemcpy2DFromArray" "cudaMemcpy2DToArray"
      "cudaMemcpy3D" "cudaMemcpyArrayToArray" "cudaMemcpyFromArray"
      "cudaMemcpyFromSymbol" "cudaMemcpyToArray" "cudaMemcpyToSymbol"
      "cudaMemset" "cudaMemset2D" "cudaMemset3D" "cudaSetDevice"
      "cudaSetupArgument" "cudaStreamCreate" "cudaStreamDestroy"
      "cudaStreamQuery" "cudaStreamSynchronize" "cudaThreadExit"
      "cudaThreadSynchronize" "cudaUnbindTexture"
      ;; Warp-level functions and others.
      "__syncthreads" "__shfl_sync" "__shfl_up_sync" "__shfl_down_sync"
      "__shfl_xor_sync" "__all_sync" "__any_sync" "__uni_sync"
      "__ballot_sync" "__match_any_sync" "__match_all_sync" "__activemask"
      "__syncwarp" "__ffs" "__fns" "__popc" "__clz" "__brev")
    "Names of built-in CUDA functions."))

(c-lang-defconst c-other-kwds
  cuda `,(append
	  (c-lang-const c-other-kwds c++)
	  cuda-builtins))

(defconst cuda-font-lock-keywords-1
  (c-lang-const c-matchers-1 cuda)
  "Minimal highlighting for CUDA mode.")

(defconst cuda-font-lock-keywords-2
  (c-lang-const c-matchers-2 cuda)
  "Fast normal highlighting for CUDA mode.")

(defconst cuda-font-lock-keywords-3
  (c-lang-const c-matchers-3 cuda)
  "Accurate normal highlighting for CUDA mode.")

(defvar cuda-font-lock-keywords cuda-font-lock-keywords-3
  "Default expressions to highlight in CUDA mode.")

(defvar cuda-mode-syntax-table nil
  "Syntax table used in cuda-mode buffers.")
(or cuda-mode-syntax-table
    (setq cuda-mode-syntax-table
          (funcall (c-lang-const c-make-mode-syntax-table cuda))))

(defvar cuda-mode-abbrev-table nil
  "Abbreviation table used in cuda-mode buffers.")
(c-define-abbrev-table 'cuda-mode-abbrev-table
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)))

(defvar-keymap cuda-mode-map
  :parent c++-mode-map
  :doc "Keymap for CUDA mode, inherited from C++ mode.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . cuda-mode))

;;;###autoload
(define-derived-mode cuda-mode c++-mode "CUDA"
  "Major mode for editing CUDA code.
This mode is derived from C++ mode and inherits its features.
\\{cuda-mode-map}"
  ;; Initialize cc-mode variables for CUDA.
  (c-initialize-cc-mode t)
  (setq abbrev-mode t)
  (c-init-language-vars cuda-mode)
  (c-common-init 'cuda-mode)
  (cc-imenu-init cc-imenu-c++-generic-expression)
  ;; Add CUDA-specific completion.
  (add-hook 'completion-at-point-functions #'(lambda ()
                                                 (when (eq major-mode 'cuda-mode)
                                                   (cuda-completion-function))))
  (c-run-mode-hooks 'c-mode-common-hook))

;; Provide a completion function for CUDA mode.
(defun cuda-completion-function ()
  "Generate a completion list for CUDA built-in functions."
  (when (and (eq major-mode 'cuda-mode)
             (bounds-of-thing-at-point 'symbol))
    (list (car (bounds-of-thing-at-point 'symbol))
          (cdr (bounds-of-thing-at-point 'symbol))
          cuda-builtins
          :exclusive 'no)))

;; A hook for additional CUDA-mode customizations.
(defun my-cuda-mode-setup ()
  "Additional customizations for CUDA mode."
  (setq c-basic-offset 4)   ; Set preferred indentation
  (flycheck-mode 1))        ; Enable Flycheck for on-the-fly syntax checking

(add-hook 'cuda-mode-hook 'my-cuda-mode-setup)

(provide 'cuda-mode)
;;; cuda-mode.el ends here
