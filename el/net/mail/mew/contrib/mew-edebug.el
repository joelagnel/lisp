;; mew-edebug.el --- Help for using edebug for macros in Mew

;; Author:  Sen Nagata <sen@eccosys.com>
;; Created: Nov 11, 2001
;; Version: 0.3

;; Set-up:
;;
;;   Put this file somewhere in your `load-path' and the following in
;; your .emacs:
;;
;;     (add-hook 'mew-init-hook
;;               '(lambda ()
;;                  (require 'mew-edebug)))
;;
;;   Upon instrumenting a function with `edebug-defun',
;; `mew-edebug-macro-init' will be run to instrument Mew macros.

;; Issues:
;;
;;   I've tested many of the macros below with edebug. Although most
;; appear to work, I experienced problems with some
;; (e.g. `mew-time-rfc-*').  I don't know what the problem is yet, but
;; my current suspicion is that it has something to do with
;; `defsubst'.

;;; Code:

(require 'mew)

(defvar mew-macro-names
  '(
    mew-header-encode-cond      
    mew-header-encode-cond2     
    mew-decode-narrow-to-header 
    mew-summary-header-mode     
    mew-add-first               
    mew-insert-after            
    mew-replace-with            
    mew-remove-entry            
    mew-elet                    
    mew-filter                  
    mew-time-rfc-day            
    mew-time-rfc-mon            
    mew-time-rfc-year           
    mew-time-rfc-hour           
    mew-time-rfc-min            
    mew-time-rfc-sec            
    mew-time-rfc-tmzn           
    mew-rendezvous              
    mew-addrstr-parse-syntax-list-check-depth 
    mew-mark-alist-set            
    mew-mode-input-file-name      
    mew-mode-input-directory-name 
    mew-plet                      
    mew-piolet                    
    mew-flet                      
    mew-frwlet                    
    mew-summary-msg-or-part       
    mew-summary-msg               
    mew-summary-part              
    mew-summary-multi-msgs        
    mew-summary-prepare-draft     
    mew-summary-only              
    mew-virtual-only              
    mew-thread-only               
    mew-pickable
    mew-summary-not-in-queue      
    mew-summary-not-in-draft      
    mew-summary-not-in-mdrop      
    ))

(defun mew-edebug-macro-init ()
  "Call `def-edebug-spec' for each macro in `mew-macro-names'.

This function should be invoked before instrumenting a function for
use with edebug."
  (interactive)
  (let ((macro-names mew-macro-names))
    (while macro-names
      (eval `(def-edebug-spec ,(car macro-names) t))
      (setq macro-names (cdr macro-names)))))

;; instrument Mew macros when edebug is used
(add-hook 'edebug-setup-hook
	  '(lambda ()
	     (mew-edebug-macro-init)))

(provide 'mew-edebug)

;;; Copyright Notice:

;; Copyright (C) 2001 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-edebug.el ends here
