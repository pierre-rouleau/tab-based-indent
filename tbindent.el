;;; tbindent.el --- Edit space-indented file in tab-indented buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau001@gmail.com>
;; Maintainer: Pierre Rouleau <prouleau001@gmail.com>
;; URL: https://github.com/pierre-rouleau/tab-based-indent
;; Created   : Monday, November 10 2025.
;; Version: 0.3.0
;; Package-Version: 20251130.1345
;; Keywords: convenience, languages
;; Package-Requires: ((emacs "24.3"))

;; This file is part of the TBINDENT package.
;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; --------------------------------------------------------------------------
;;; Commentary:
;;
;; This provides the `tbindent-mode', a minor mode that seamlessly
;; converts the space-based indentation file into tab-based indented buffer.
;;
;; While the minor mode is active you can change the indentation width by
;; executing the `tbindent-set-tab-width' command.  That command changes the
;; `tab-width' and the indentation width to the new value, to use narrower or
;; wider indentation.
;;
;; When saving the buffer content back to the file, the mode automatically
;; converts the indentation back to the original space-indentation scheme.
;;
;;
;; Why?
;;
;; Although not popular in most software development circles, using hard tabs
;; for indentation provides the undeniable advantage of flexibility in terms
;; of visual rendering.  Once all indentation level correspond to 1 hard-tab
;; you can easily change the visual width of indentation with the
;; `tbindent-set-tab-width' command.  That does not modify the content of the
;; file.
;;
;; This feature may appeal to people that have problems working with small
;; indentation width imposed by language or team conventions.  It acts as a
;; workaround: temporary change the indentation scheme to a hard-tab based
;; indentation for editing, change the width of the hard tab for visibility
;; but keep the original indentation scheme inside the file.
;;
;; For example, Dart and Gleam impose a 2-space indentation level.  For
;; buffers using major modes for those languages, we can use the following
;; procedure:
;;
;; - set the indentation control variable to 2 and the `tab-width' to 2,
;; - tabify the indentation whitespace of the entire buffer excluding all
;;   strings and comments.
;; - change the with of hard tabs (controlled by the `tab-width' variable),
;;   and the width of the variables for the major mode to a larger value.
;;
;; Once set-up we can see the code with a wider indentation and
;; continue to work with the rules imposed by the major mode logic.
;;
;; Later, before saving the buffer back to the file, we simply perform the
;; following steps:
;;
;; - restore the tab and indentation width back to 2,
;; - untabify all indentation whitespace,
;; - save the file.
;;
;; The library provides all necessary functions, along with a special minor-mode
;; that automatically performs all operation seamlessly, allowing editing the
;; files with wider indentation despite the space-based indentation used in
;; the files.
;;
;; The files retain their original space-based indentation scheme but you can
;; edit them with a wider or narrower indention width!
;;
;; For more information see the README.rst.txt file.

;; ---------------------------------------------------------------------------
;;; History:
;;
;; - Version 0.3.0 :
;;     - Simplify use: tbindent-mode automatically adjusts `tab-width' if it
;;       differs from the value of the major mode indentation control
;;       variable; it no longer requires the user to do it manually.  If the
;;       indentation control variable is unknown for the major mode then it
;;       issues an error with instructions.
;;     - All messages start with "tbindent:"
;; - Version 0.2.1:
;;     Improve title.
;; - Version 0.2.0:
;;     Enhance safety: do not activate tbindent-mode if the indentation
;;     variable(s) for the major mode is unknown.
;; - Version 0.1.01:
;;     Update description, docstring typo fix, allow Emacs 24.3.
;; - Version 0.1:
;;     Created, November 10, 2025 from code taken on my PEL project making it
;;     self-sufficient allowing future publishing on MELPA.  Restricted to
;;     Emacs 29.1 to comply with package-lint despite the false positives
;;     reports.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;  Just Emacs provided packages:
(require 'simple)         ; use: `normal-auto-fill-function'
;;
;; The following variables are defined in Emacs built-ins:
;; from: Emacs files.el   ; use: `before-save-hook', `after-save-hook',
;;                        ;      `kill-buffer-hook'
;; from: Emacs indent.el  ; use: `standard-indent'
;; from: Emacs version.el ; use: `emacs-major-version'
;;

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;* Customization
;;  -------------

(defun tbindent-indent-valid-p (n)
  "Return t if N is a valid indentation integer in 2-8 range, nil otherwise."
  (and (integerp n) (< n 9) (> n 1)))

(defgroup tbindent nil
  "Tabs Based Indentation."
  :group 'indent)

(defcustom tbindent-lighter " ⍈"
  "Mode line lighter used by `tbindent-mode'."
  :group 'tbindent
  :type 'string)

(defcustom tbindent-target-indent-width-default 4
  "Default target indentation width.
The indentation and tab width used by the `tbindent-mode' if none
is specified in `tbindent-target-indent-widths' for the mode."
  :group 'tbindent
  :type 'integer
  :safe 'tbindent-indent-valid-p)


(defcustom tbindent-target-indent-widths '((dart-mode     . 4)
                                           (dart-ts-mode  . 4)
                                           (gleam-ts-mode . 4))
  "Target indentation width per major mode.

When specified, the `tbindent-mode' automatically selects the specified
indentation width for tab-based indentation of the corresponding major
mode."
  :group 'tbindent
  :type '(repeat :tag "Indent target for:"
                 (cons
                  (symbol  :tag "mode name        ")
                  (integer :tag "indentation width" :value 4))))

(defcustom tbindent-extra-mode-indent-vars nil
  "User-specified indentation variable specifications for modes.
This is a alist mapping the major mode name to the name of one variable, a list
of variables or a list of (vars . offset).  This identifies the name of the
indentation control variable or variables used by the mode and if necessary an
width offset applied to the variable: var = width + offset.

By adding entries into this list, you can add information that complements or
overrides the entries in the hard-coded `tbindent--mode-indent-vars'.

For example if you use the old `ada-mode' you could add the entry that maps it
to `ada-indent' variable."
  :group 'tbindent
  :type '(repeat
          (list
           (symbol :tag "mode name   ")
           (choice :tag "use"
                   (symbol :tag "indent control variable name")
                   (repeat :tag "Several variables"
                    (symbol :tag "indent control variable name"))
                   (repeat :tag "Several (var . offset) cells"
                    (cons
                     (symbol :tag "indent control variable name ")
                     (integer :tag "offset from tab-width applied")))))))

;;* Mode Specific Indentation Width Utilities
;;  -----------------------------------------
;;
;; - `tbindent-mode-indentation-width'
;;   - `tbindent-mode-indent-control-vars'
;;     - `tbindent--indent-vars-for'
;;       -d: tbindent-extra-mode-indent-vars
;;       -d: tbindent--mode-indent-vars
;;     - `tbindent-string-ends-with-p'

;; Credit Note: the following table was originally derived from code
;;              that resides inside dtrt-indent.el and indent-control.el
;;        See:  https://github.com/jscheid/dtrt-indent
;;              https://github.com/jcs-elpa/indent-control
;;
;; Note: `package-lint-current-buffer' reports 17 false-positive errors
;;       on the following declaration when Emacs 24.3 is the target
;;       instead of 29.1.
;;       His author have acknowledged those being false positive as described
;;       in: https://github.com/purcell/package-lint/issues/304
;;       For the moment, I prevent the false positive from triggering by
;;       restricting to Emacs >= 29.1 and commenting out ada-mode.
;; [: TODO 2025-11-13, by Pierre Rouleau: remove all that once package-lint
;;                                       is fixed.]
;;
(defconst tbindent--mode-indent-vars
  ;; Mode                Variable, list of variables, list of (var . offset)
  '((actionscript-mode   actionscript-indent-level)
    ;; (ada-mode     ada-indent) ; generates package-lint false-positive error
    (ada-ts-mode         ada-ts-mode-indent-offset)
    (apache-mode         apache-indent-level)
    (awk-mode            c-basic-offset)
    (bash-ts-mode        sh-basic-offset) ; Shell Script - use SMIE if available
    (c-mode              c-basic-offset)  ; C
    (c-ts-mode           c-ts-mode-indent-offset)
    (c++-mode            c-basic-offset) ; C++
    (c++-ts-mode         c-ts-mode-indent-offset)
    (cmake-mode          cmake-tab-width) ; CMake
    (cmake-ts-mode       cmake-ts-mode-indent-offset)
    (coffee-mode         coffee-tab-width)
    (coq-mode            coq-indent-basic)
    (cperl-mode          cperl-indent-level) ; Perl
    (cperl-mode          cperl-indent-level)
    (crystal-mode        crystal-indent-level) ; Crystal (Ruby) - use SMIE if available
    (csharp-mode         (c-basic-offset csharp-mode-indent-offset))
    (css-mode            css-indent-offset) ; CSS - use SMIE if available
    (less-css-mode       css-indent-offset)
    (scss-mode           css-indent-offset)
    (ssass-mode          ssass-tab-width)
    (dart-mode           tab-width)
    (dart-ts-mode        dart-ts-mode-indent-offset)
    (dockerfile-mode     dockerfile-indent-offset)
    (d-mode              c-basic-offset) ; D
    (elixir-mode         elixir-smie-indent-basic)
    (elm-mode            elm-indent-offset)
    (emacs-lisp-mode     lisp-body-indent)
    (enh-ruby-mode       enh-ruby-indent-level)
    (erlang-mode         erlang-indent-level) ; Erlang
    (ess-mode            ess-indent-offset)
    (f90-mode            (f90-associate-indent
                          f90-continuation-indent
                          f90-critical-indent
                          f90-do-indent
                          f90-if-indent
                          f90-program-indent
                          f90-type-indent))
    (feature-mode        (feature-indent-offset
                          feature-indent-level))
    (fsharp-mode         (fsharp-continuation-offset
                          fsharp-indent-level
                          fsharp-indent-offset))
    (gdscript-mode       gdscript-indent-offset)
    (gleam-ts-mode       gleam-ts-indent-offset)
    (go-ts-mode          go-ts-mode-indent-offset)
    (gpr-ts-mode         gpr-ts-mode-indent-offset)
    (groovy-mode         groovy-indent-offset) ; Groovy
    (jenkinsfile-mode    groovy-indent-offset)
    (haskell-mode        (haskell-indent-spaces
                          haskell-indent-offset
                          haskell-indentation-layout-offset
                          haskell-indentation-left-offset
                          haskell-indentation-starter-offset
                          haskell-indentation-where-post-offset
                          haskell-indentation-where-pre-offset
                          shm-indent-spaces))
    (haxe-mode           c-basic-offset)
    (haxor-mode          haxor-tab-width)
    (idl-mode            c-basic-offset)
    (jade-mode           jade-tab-width)
    (java-mode           c-basic-offset) ; Java
    (java-ts-mode        java-ts-mode-indent-offset)
    (jde-mode            c-basic-offset) ; Java (JDE)
    (javascript-mode     js-indent-level)
    (js-mode             js-indent-level) ; JavaScript
    (js-ts-mode          js-indent-level)
    (js-json-mode        js-indent-level)  ; JSON
    (js2-mode            js2-basic-offset) ; JavaScript-IDE
    (js2-jsx-mode        (js2-basic-offset sgml-basic-offset))
    (js3-mode            js3-indent-level) ; JavaScript-IDE
    (json-mode           js-indent-level)  ; JSON
    (json-ts-mode        json-ts-mode-indent-offset)
    (julia-mode          julia-indent-offset)
    (kotlin-mode         kotlin-tab-width)
    (lisp-mode             lisp-body-indent)
    (lisp-interaction-mode lisp-body-indent)
    (livescript-mode       livescript-tab-width)
    (lua-mode            lua-indent-level) ; Lua
    (magik-mode          magik-indent-level)
    (matlab-mode         matlab-indent-level)
    (meson-mode          meson-indent-basic)
    (mips-mode           mips-tab-width)
    (mustache-mode       mustache-basic-offset)
    (nasm-mode           nasm-basic-offset)
    (nginx-mode          nginx-indent-level)
    (nxml-mode           (nxml-child-indent nxml-attribute-indent))
    (objc-mode           c-basic-offset) ; Objective C
    (octave-mode         octave-block-offset)
    (nxml-mode           nxml-child-indent)   ; XML
    (pascal-mode         pascal-indent-level) ; Pascal
    (perl-mode           perl-indent-level)   ; Perl
    (php-mode            c-basic-offset)      ; PHP
    (pike-mode           c-basic-offset)
    (plantuml-mode       plantuml-indent-level) ; PlantUML
    (protobuf-mode       c-basic-offset)        ; Protobuf
    (pug-mode            pug-tab-width)         ; Pug
    (puppet-mode         puppet-indent-level)
    (ps-mode             ps-mode-tab)
    (python-mode         (python-indent-offset
                          py-indent-offset ; used by the badly maintained python-mode.
                          python-indent-levels))
    (raku-mode           raku-indent-offset) ; Perl6/Raku
    (rjsx-mode           (js-indent-level sgml-basic-offset))
    (ruby-mode           ruby-indent-level)     ; Ruby - use SMIE if available
    (enh-ruby-mode       enh-ruby-indent-level) ; Ruby - use SMIE if available
    (rust-mode           rust-indent-offset)    ; Rust - use SMIE if available
    (rust-ts-mode        rust-ts-mode-indent-offset)
    (rustic-mode         rustic-indent-offset) ; Rust - use SMIE if available
    (scala-mode          scala-indent:step)    ; Scala - use SMIE if available
    (sgml-mode           sgml-basic-offset)    ; SGML
    (shader-mode         shader-indent-offset)
    (slim-mode           slim-indent-offset)
    (sml-mode            sml-indent-level)
    (sql-mode            sql-indent-offset)
    (svelte-mode         svelte-basic-offset)
    (sh-mode             sh-basic-offset) ; Shell Script - use SMIE if available
    (swift-mode          swift-mode:basic-offset) ; Swift
    (tcl-mode            (tcl-indent-level tcl-continued-indent-level))
    (terra-mode          terra-indent-level)
    (typescript-mode     typescript-indent-level) ; Typescript
    (typescript-ts-base-mode typescript-ts-mode-indent-offset)
    (verilog-mode        (verilog-indent-level
                          verilog-indent-level-behavioral
                          verilog-indent-level-declaration
                          verilog-indent-level-module
                          verilog-cexp-indent
                          verilog-case-indent))
    (vhdl-mode           vhdl-basic-offset) ; VHDL
    (web-mode            (web-mode-attr-indent-offset
                          web-mode-attr-value-indent-offset
                          web-mode-code-indent-offset
                          web-mode-css-indent-offset
                          web-mode-markup-indent-offset
                          web-mode-sql-indent-offset
                          web-mode-block-padding
                          web-mode-script-padding
                          web-mode-style-padding)) ; HTML
    (xquery-mode         xquery-mode-indent-width) ; XQuery
    (yaml-mode           yaml-indent-offset)       ; YAML
    (zig-mode            zig-indent-offset))
  "Map mode name to indentation control variable(s) it uses.
This alist maps the mode name to one of 3 possible entities:
- The name of the single variable that controls indentation for the mode, and
  which must have the same value as `tab-width'.
- A list holding the names of each variable that control various aspects of
  the mode's indentation.  Each of these variables must be set to the same
  value as `tab-width'.
- A list of (varname . offset) cons cell(s).  The car of the cons cell is the
  name of the indentation control variable.  The cdr of the cons cell is the
  offset that must be applied to `tab-width' to get the indentation value.

IMPORTANT:
 Note that the `tbindent-mode' only works for buffer where the `tab-width'
 can be set to the same value as the indentation variable or all indentation
 variables.")


(defun tbindent-string-ends-with-p (text suffix)
  "Return t if TEXT string does end with SUFFIX string, nil otherwise.
Ignore case differences if IGNORE-CASE is non-nil."
  (let ((text-len (length text))
        (suffix-len (length suffix)))
    (and (>= text-len suffix-len)
         (eq t (compare-strings suffix nil nil
                                text (- text-len suffix-len) nil)))))

(defun tbindent--indent-vars-for (mode)
  "Return indentation variable(s) for specified major MODE.
It may return:
- a single variable symbol, the indentation variable for the MODE,
- a list of indentation variable symbols,
- a list of (varname . offset) cons cells.

First look in the user-specified `tbindent-extra-mode-indent-vars' table.
If nothing found, then look into `tbindent--mode-indent-vars'."
  (let ((vars (cadr (assoc mode tbindent-extra-mode-indent-vars))))
    (unless vars
      (setq vars (cadr (assoc mode tbindent--mode-indent-vars))))))

(defun tbindent-mode-indent-control-vars (&optional mode)
  "Return list of indentation control vars for current major mode or MODE.
Return nil if none found."
  (let* ((mode (or mode major-mode))
         (vars (tbindent--indent-vars-for mode)))
    (unless vars
      (when (tbindent-string-ends-with-p (symbol-name mode) "-ts-mode")
        (setq mode (intern (format "%s-mode" (tbindent-file-type-for mode))))
        (setq vars (tbindent--indent-vars-for mode))))
    (if (listp vars)
        vars
      (list vars))))

(defun tbindent-mode-indentation-width (&optional mode)
  "Return the indentation width used by current major mode or MODE.
Return the value of the indentation control variable used for the
current major mode (or the specified MODE) if there is one.  If there
are several, return the value of the first one.  Return the value of
`standard-indent' otherwise."
  (let ((vars (tbindent-mode-indent-control-vars mode)))
    (when vars
      (symbol-value (car vars)))))

;; ---------------------------------------------------------------------------
;;* Mode Symbol Manipulation Utilities
;;  ----------------------------------
;;
;; These utilities create symbols based on the name of the current major mode,
;; taking into account classic and tree-sitter based modes.  The tree-sitter
;; based mode is expected to have a name that ends with the -ts-mode suffix,
;; while the classic mode is expected to have a name that ends with the -mode
;; suffix.
;;
;;
;;  - `tbindent-major-mode-symbol-value-or'
;;    - `tbindent-major-mode-symbol-value'
;;      - `tbindent-major-mode-symbol-for'
;;        - `tbindent-string-with-major-mode'
;;          - `tbindent-file-type-for'


(defun tbindent-file-type-for (major-mode-symbol)
  "Return the file type name string for the specified MAJOR-MODE-SYMBOL.

That's the symbol name stripped off the '-mode' or '-ts-mode' suffix."
  (let ((sname (symbol-name major-mode-symbol)))
    (substring sname
               0
               (- (length (if (string-match "-ts-mode" sname)
                              "-ts-mode"
                            "-mode"))))))

(defun tbindent-string-with-major-mode (symbol-format-string)
  "Return a string formatted with the single %s replaced by the major mode.

The \"%s\" in the SYMBOL-FORMAT-STRING is replaced by the name of the
major-mode.  That's the prefix string before the \"-mode\" portion of
the major mode name of the current buffer."
  (format symbol-format-string
          (tbindent-file-type-for major-mode)))

(defun tbindent-major-mode-symbol-for (symbol-format-string)
  "Return the major-mode specific symbol for specified buffer.

The symbol name is identified by the SYMBOL-FORMAT-STRING which must
contain one \"%s\" that is replaced by the prefix string before the
\"-mode\" (or \"-ts-mode\") of the major mode of the the current buffer."
  (intern
   (tbindent-string-with-major-mode symbol-format-string)))

(defun tbindent-major-mode-symbol-value (symbol-format-string)
  "Return the value of major-mode specific symbol for specified buffer.

The symbol name is identified by the SYMBOL-FORMAT-STRING which must
contain one \"%s\" that is replaced by the prefix string before the
\"-mode\" (or \"-ts-mode\") of the major mode of the current buffer."
  (symbol-value (tbindent-major-mode-symbol-for symbol-format-string)))

(defun tbindent-major-mode-symbol-value-or (symbol-format-string default-value)
  "Return value or default of major-mode specific symbol for specified buffer.
The symbol name is identified by the SYMBOL-FORMAT-STRING which must
contain one \"%s\" that is replaced by the prefix string before the
\"-mode\" (or \"-ts-mode\") of the major mode of the the current buffer.
If nothing exists for the current major-mode return DEFAULT-VALUE."
  (condition-case nil
      (tbindent-major-mode-symbol-value symbol-format-string)
    (error default-value)))

;; ---------------------------------------------------------------------------
;;* Control Tab Width
;; -------------------
;;
;; This defines the `tbindent-set-tab-width' command, a command that changes
;; the buffer local value of `tab-width' and the value of the only or all
;; indentation control variables used for the current major mode.  When these
;; all have the same values, it allows a modification of the indentation
;; rendering width.
;;
;; The call hierarchy is:
;;
;; * `tbindent-set-tab-width'
;;   - `tbindent-mode-indent-control-vars'
;;   - `tbindent-read-number'
;;

(defvar-local tbindent--original-tab-width nil
  "Tab width value used before `tbindent-indent-with-tabs' is used.")

(defvar-local tbindent--last-set-tab-width nil
  "Tab width set by last `tbindent-set-tab-width' call.")

(defun tbindent-read-number (prompt default history-symbol)
  "Emacs version sensitive `read-number'.
Prompts with PROMPT, use DEFAULT value and the HISTORY-SYMBOL to track
reply history."
  (with-no-warnings
    (if (>= emacs-major-version 28)
        (read-number prompt default history-symbol)
      (read-number prompt default))))

;;;###autoload
(defun tbindent-set-tab-width (n)
  "Set the tab and indent width used in current buffer to N.

Set the buffer local value of `tab-width' and indent control variables
used by the current buffer.
Return the new `tab-width' or nil if unchanged."
  (interactive (list (tbindent-read-number "New tab-width: " tab-width
                                           'tbindent-set-tab-width-history)))
  (let ((control-vars (tbindent-mode-indent-control-vars))
        (current-tab-width tab-width)
        (offset nil))
    ;;
    (while (not (and (< n 9) (> n 1)))
      (setq n (tbindent-read-number "Enter valid tab-width in 2-8 range: "
                                    current-tab-width
                                    'tbindent-set-tab-width-history)))
    ;;
    (when (not (= n current-tab-width))
      (message
       "tbindent: changed buffer's tab-width from %d to %d"
       current-tab-width n)
      (when control-vars
        (dolist (var control-vars)
          (if (consp var)
              ;; a (symbol . offset)
              (progn
                (setq offset (cdr var))
                (setq var (car var))
                (when (boundp var)
                  (set (make-local-variable var) (+ n offset))))
            ;; just a symbol
            (when (boundp var)
              (set (make-local-variable var) n)))))
      ;; Always set `tab-width' to the new value.
      (setq-local tab-width n)
      (setq-local tbindent--last-set-tab-width n))))

;; ---------------------------------------------------------------------------
;;* Changing Indentation Scheme Between and Space-Based and Tabs-Based
;;  ------------------------------------------------------------------
;;
;; Two commands change the buffer's indentation from
;; space-based to tabs-based and vice-versa:
;;
;; - `tbindent-indent-with-tabs' converts space-based indentation to tab-based
;;   indentation.
;; - `tbindent-indent-with-spaces' converts tabs-based indentation to
;;   space-based indentation.
;;
;; The code hierarchy is:
;;
;;  * `tbindent-indent-with-tabs'
;;    - `tbindent-tabify-all-indent'
;;      - `tbindent-inside-code'
;;     . `tbindent-set-tab-width'
;;  * `tbindent-indent-with-spaces'
;;     . `tbindent-set-tab-width'


(defun tbindent-inside-code (&optional pos)
  "Return non-nil when point or POS is in code, nil if in comment or string.
Note that this changes the search match data!"
  (let* ((pos (or pos (point)))
         (syntax (syntax-ppss pos)))
    (and (not (nth 3 syntax))
         (not (nth 4 syntax)))))


(defun tbindent-tabify-all-indent ()
  "Convert multiple spaces in indent to tabs when possible.

Process complete buffer: a group of spaces in the leading indentation is
partially replaced by tabs when this can be done without changing the
column they end at.  Comments and strings are not modified.

The variable `tab-width' controls the spacing of tab stops.
This is a indentation specific `tabify' function."
  (save-excursion
    (save-restriction
      ;; Process entire buffer.
      (goto-char (point-min))
      (let ((indent-tabs-mode t)
            (inside-code nil))
        (while (re-search-forward "^[ \t]* [ \t]+" nil t)
          ;; In white-space indentation: adjust to TABs were possible.
          (save-match-data
            (setq inside-code (tbindent-inside-code (point))))
          (when inside-code
            (let ((end-col (current-column))
                  (beg-col (save-excursion (goto-char (match-beginning 0))
                                           (skip-chars-forward "\t")
                                           (current-column))))
              (unless (= (/ end-col tab-width) (/ beg-col tab-width))
                ;; The spacing (after some leading TABs which we wouldn't
                ;; want to touch anyway) does not straddle a TAB boundary,
                ;; so it neither contains a TAB, nor will we be able to use
                ;; a TAB here anyway: there's nothing to do.
                (delete-region (match-beginning 0) (point))
                (indent-to end-col)))))))))

(defvar tbindent-mode)                  ; prevent byte compiler warnings

;;;###autoload
(defun tbindent-indent-with-tabs (&optional with-tab-width by-minor-mode)
  "Convert current buffer to use tabs for indentation.

If the optional WITH-TAB-WIDTH numerical argument is specified, after
conversion to tab-based indentation change the tab width to that
specified value.  If the argument is not specified, prompt for the tab
width to use.

Requirement: before using this command, the buffer local `tab-width' must be
             equal to the indentation width used by the code, which should be
             the value used by the indentation control variable for the mode.

This command is only available when the `tbindent-mode' is turned off.
Since it is used internally by `tbindent-mode', the BY-MINOR-MODE parameter
must only be set by the call from `tbindent-mode'."
  (interactive
   (if (and current-prefix-arg
            (not (consp current-prefix-arg)))
       (list (prefix-numeric-value current-prefix-arg))
     (list (tbindent-read-number
            "Indent with tab width: "
            tab-width
            (tbindent-major-mode-symbol-for
             "pel-indent-with-tabs-history-for-%s")))))
  (if (or by-minor-mode (not tbindent-mode))
      (progn
        ;; First tabify indentation whitespace, replacing space-based
        ;; indentation with tabs that represent the specified tab width.
        (tbindent-tabify-all-indent)
        ;; Remember `tab-width' originally used in the buffer.
        ;; It should correspond with the indentation width.
        (unless tbindent--original-tab-width
          (setq-local tbindent--original-tab-width tab-width))
        ;; Adjust the tab and indentation width to the new selection.
        (tbindent-set-tab-width with-tab-width)
        ;; New indented code must now be indented with hard tabs.
        (indent-tabs-mode 1))
    (user-error "Command not available while tbindent-mode is active!")))

;;;###autoload
(defun tbindent-indent-with-spaces (&optional with-tab-width by-minor-mode)
  "Convert current buffer to use space for indentation.

Restore the space-based indentation scheme using the tab width that was
used before the first call to `tbindent-indent-with-tabs' unless the optional
WITH-TAB-WIDTH numerical argument is specified.  If an optional
numerical argument is specified, use that for tab width.

This command is only available when the `tbindent-mode' is turned off.
Since it is used internally by `tbindent-mode', the BY-MINOR-MODE parameter
must only be set by the call from `tbindent-mode'."
  (interactive "P")
  (if (or by-minor-mode (not tbindent-mode))
      (save-excursion
        (if with-tab-width
            (tbindent-set-tab-width with-tab-width)
          ;; Restore the original tab-width if it was stored in
          ;; `tbindent--original-tab-width'
          (when (or  tbindent--original-tab-width
                     tbindent--last-set-tab-width)
            (tbindent-set-tab-width tbindent--original-tab-width)))
        ;; Then untabify.  Note that hard-tabs inside strings and comments
        ;; will be replaced by spaces.  If this is a problem in some cases,
        ;; please let me know.
        (untabify (point-min) (point-max))
        ;; New indented code must now be indented with spaces.
        (indent-tabs-mode -1))
    (user-error "Command not available while tbindent-mode is active!")))


;; ---------------------------------------------------------------------------
;;* Manage auto-fill in tab-based indented buffer
;;  ---------------------------------------------
;;
;; When a buffer is loaded with the content of a file that uses a 2-space
;; indentation scheme and a maximum line length of 80 columns, we need to
;; adjust the `fill-column' value when then buffer holds the text that uses a
;; different indentation based on tabs that are rendered with a different
;; width. The code in this section deals with that.
;;
;; The value of the original `fill-column' used for the space-based
;; indentation file is remembered in the `tbindent--normalfile-fill-column'
;; buffer local variable.
;;
;; When the `tbindent-mode' is active, it replaces the function that performs
;; the automatic filling by `tbindent--normalfile-fill-column' which computes
;; the adjusted value of fill-column on each line by counting the number of
;; hard tab character present on the line and their impact on the fill-column.
;; That function is only called when automatic filling is activated.
;;
;; These are only used indirectly by the `tbindent-mode' as shown by the
;; following call hierarchy, where
;; `tbindent--install-indented-with-tabs-auto-fill' installs that function to
;; deal with automatic filling and `tbindent--restore-original-fill-function'
;; restores the original function when turning off `tbindent-mode':
;;
;;  * `tbindent-mode'
;;    - `tbindent--install-indented-with-tabs-auto-fill'
;;      > `tbindent-indented-with-tabs-do-auto-fill'
;;      - `tbindent--adjusted-fill-column'
;;    - `tbindent--restore-original-fill-function'

(defvar-local tbindent--normalfile-fill-column nil
  "The `fill-column' value used for the normal space indented file format.")

(defun tbindent--adjusted-fill-column (space-indent-width viewed-tab-width
                                                          &optional position)
  "Return adjusted fill column for tab-indented line at POSITION or point.

That is the `fill-column' that can be used in the tab-indented buffer to
correspond to what `fill-column' is inside the real space-indented file.
- SPACE-INDENT-WIDTH corresponds to what the file normally uses.
- VIEWED-TAB-WIDTH corresponds to what is used in the buffer."
  (save-excursion
    (when position (goto-char position))
    (let* ((extra-columns-per-tab (- viewed-tab-width space-indent-width))
           (line-start-pos (progn (forward-line 0) (point)))
           (tab-count      (count-matches "\t" line-start-pos (line-end-position)))
           (extra-columns  (* tab-count extra-columns-per-tab)))
      ;; Cache the real, file-specific, `fill-column' value in buffer local
      ;; variable.
      (unless tbindent--normalfile-fill-column
        (setq-local tbindent--normalfile-fill-column fill-column))
      ;; return what fill column should be for this line
      (+ tbindent--normalfile-fill-column extra-columns))))

(defvar-local tbindent--normal-auto-fill-function nil
  "Remember function `auto-fill-function' normally used for normal files.")

(defvar-local tbindent--space-based-indent-width nil
  "Original space based indentation width for the file.")

(defun tbindent-indented-with-tabs-do-auto-fill ()
  "Perform the auto-fill inside a tabs-indented buffer.
Adjust the buffer-local `fill-column' based on the indentation scheme used and
in the normal file and the tabs-based indentation used inside the buffer, then
  execute the `do-auto-fill'"
  ;; Adjust the fill-column to what it should be if the indentation had been
  ;; reconverted back to 2-space indents and then execute the fill function.
  (let ((fill-column (tbindent--adjusted-fill-column
                      tbindent--space-based-indent-width
                      tab-width)))
    (funcall tbindent--normal-auto-fill-function)))

(defun tbindent--install-indented-with-tabs-auto-fill ()
  "Install the tabs-indented aware auto fill function."
  ;; Cache the `auto-fill-function' for the buffer.
  (unless tbindent--normal-auto-fill-function
    (setq-local tbindent--normal-auto-fill-function normal-auto-fill-function)
    (make-local-variable 'normal-auto-fill-function)
    (setq-local normal-auto-fill-function
                'tbindent-indented-with-tabs-do-auto-fill))
  (when auto-fill-function
    (setq-local auto-fill-function
                (function tbindent-indented-with-tabs-do-auto-fill))))

(defun tbindent--restore-original-fill-function ()
  "Restore original fill function."
  (when tbindent--normal-auto-fill-function
    (setq-local normal-auto-fill-function tbindent--normal-auto-fill-function)
    (when auto-fill-function
      (setq-local auto-fill-function tbindent--normal-auto-fill-function))))

;; ---------------------------------------------------------------------------
;;* Saving Buffer to File
;;  ---------------------
;;
;; While `tbindent-mode' is active in a buffer, its content must first be
;; converted back to the original space-based indentation scheme before
;; storing it inside a file because it was converted from that space-based
;; indentation into a tabs-based indentation for ease of viewing.
;;
;; Two functions are used as hooks:
;;
;; - `tbindent--before-save-or-kill' is called just before the buffer is
;;   stored into the file.  It remembers the indentation tab-width used by the
;;   specially formatted buffer into the buffer local
;;   `tbindent--tab-width-used-during-tab-based-indent' variable and then
;;   converts the buffer content back to space-based indentation.
;; - `tbindent--after-save' is called just after saving the buffer contents to
;;   its file. It reconverts the buffer to a tab-based indentation using the
;;   appropriate width remembered in the variable.

(defvar-local tbindent--tab-width-used-during-tab-based-indent nil)

(defun tbindent--before-save-or-kill ()
  "Disable tab-based indentation and restore native space-base indent.
This is performed just before saving a buffer to a file or killing it."
  (setq-local tbindent--tab-width-used-during-tab-based-indent tab-width)
  (tbindent-indent-with-spaces nil :by-minor-mode))

(defun tbindent--after-save ()
  "Restore tab-based indentation with same width used before buffer save."
  (if tbindent--tab-width-used-during-tab-based-indent
      (progn
        (tbindent-indent-with-tabs
         tbindent--tab-width-used-during-tab-based-indent
         :by-minor-mode)
        (set-buffer-modified-p nil))
    (display-warning 'tbindent
                     "tbindent--after-save: unknown indentation width!"
                     :error)))
;; ---------------------------------------------------------------------------
;;* Minor Mode
;;  ----------

(defun tbindent-target-indent-width-for (&optional mode)
  "Return target indentation width requested for current major mode or MODE.
Read the value from the `tbindent-target-indent-widths'.  If not found return
`tbindent-target-indent-width-default'."
  (let* ((mode (or mode major-mode))
         (spec (assoc mode tbindent-target-indent-widths)))
    (if spec
        (cdr spec)
      tbindent-target-indent-width-default)))

(defvar-local tbindent--original-tab-width nil
  "Value of buffer tab-width used before `tbindent-mode' activation.")

;;;###autoload
(define-minor-mode tbindent-mode
  "Minor mode that automatically converts buffer to tab-based indentation.

Once the mode is active, change the visual indentation with the
`tbindent-set-tab-width' command with \\[tbindent-set-tab-width].

IMPORTANT: Note that `tbindent-mode' only works for buffer where
`tab-width' has the same value as the indentation control variable.  It
checks that when activating the mode and will refuse to activate it when
the conditions are not met issuing a descriptive user-error instead."
  :lighter tbindent-lighter
  (let ((warning-message-printed nil)
        (mode-indentation-width (tbindent-mode-indentation-width)))
    (if tbindent-mode
        ;; When turning mode on
        ;; --------------------
        (if mode-indentation-width
            ;; Indentation width variable is known.
            (progn
              ;; Remember original tab-width
              (setq tbindent--original-tab-width tab-width)
              ;; Adjust local tab-width is necessary.
              (unless (eq tab-width mode-indentation-width)
                (message "tbindent: changing %s value of tab-width from %d to %d"
                         (current-buffer)
                         tab-width
                         mode-indentation-width)
                (setq-local tab-width mode-indentation-width))
              ;; On modified buffer, allow user to save first.  If user
              ;; quit, catch and activate the mode anyway, without saving.
              (condition-case nil
                  (when (and (buffer-modified-p)
                             (y-or-n-p (format "Save modified %S first? "
                                               (current-buffer))))
                    (save-buffer))
                (quit
                 (message
                  "tbindent on: indenting with tabs enabled, buffer not saved!")
                 (setq warning-message-printed t)))
              ;; Proceed
              (unless warning-message-printed
                (message
                 "tbindent on: converting %s to tab-based indent, width=%d ..."
                 (current-buffer)
                 tab-width ))
              (with-silent-modifications
                ;; Remember the original space based indentation width
                (setq-local tbindent--space-based-indent-width
                            (tbindent-mode-indentation-width))

                ;; activate indentation with tabs using either the
                ;; indentation width specified by customization (if that
                ;; symbol exists and is non-nil or the native tab-width
                ;; matching indentation width
                (tbindent-indent-with-tabs
                 (tbindent-target-indent-width-for major-mode)
                 :by-minor-mode)
                ;; Install a special auto-fill function that is aware that
                ;; each tab in the buffer corresponds to the file original
                ;; space indentation scheme.
                (tbindent--install-indented-with-tabs-auto-fill))
              ;; The buffer was modified by replacing spaces with tabs but
              ;; since we want to use it as if it was normal, don't show
              ;; the buffer modified unless it already was.
              (unless warning-message-printed
                (set-buffer-modified-p nil))
              ;; schedule operation before and after buffer save.
              (unless (memq 'tbindent--before-save-or-kill  before-save-hook)
                (add-hook 'before-save-hook #'tbindent--before-save-or-kill
                          -100
                          'local))
              (unless (memq 'tbindent--before-save-or-kill kill-buffer-hook)
                (add-hook 'kill-buffer-hook #'tbindent--before-save-or-kill
                          -100
                          'local))
              (unless (memq 'tbindent--after-save after-save-hook)
                (add-hook 'after-save-hook #'tbindent--after-save
                          +100
                          'local))

              (unless warning-message-printed
                (message "tbindent on: indenting with tabs enabled; width=%d."
                         tab-width)))
          ;; Indentation control variable is unknown!
          (setq-local tbindent-mode nil)
          (user-error "\
Can't activate tbindent-mode in %s: Unknown indentation control variable for '%s'!
Please identify it in the `tbindent-extra-mode-indent-vars' alist."
                      (current-buffer)
                      major-mode))

      ;; When turning mode off
      ;; ---------------------
      ;; Restore original space-based indentation scheme
      (with-silent-modifications
        (tbindent-indent-with-spaces nil :by-minor-mode))
      ;; Restore original variable values
      (setq  tab-width tbindent--original-tab-width)
      (tbindent--restore-original-fill-function)
      ;; Remove the hooks
      (when (memq 'tbindent--before-save-or-kill before-save-hook)
        (remove-hook 'before-save-hook #'tbindent--before-save-or-kill 'local))
      (when (memq 'tbindent--before-save-or-kill kill-buffer-hook)
        (remove-hook 'kill-buffer-hook #'tbindent--before-save-or-kill 'local))
      (when (memq 'tbindent--after-save after-save-hook)
        (remove-hook 'after-save-hook #'tbindent--after-save 'local))
      ;; Inform user.
      (message "tbindent off: restored file indentation scheme."))))

;;; --------------------------------------------------------------------------
(provide 'tbindent)

;; Local variables:
;; time-stamp-format: "%Y%02m%02d.%02H%02M"
;; time-stamp-start: "Package-Version:[ \t]+\\\\?"
;; time-stamp-end: "\n"
;; time-stamp-line-limit: 15
;; End:

;;; tbindent.el ends here
