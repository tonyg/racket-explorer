#lang racket/gui
;; frame% subclasses specialized for explorers and similar tools.

(provide tool-frame%
         default-make-tool-keymap
         current-make-tool-keymap)

(define tool-frame%
  (class frame%
    (field [keymap ((current-make-tool-keymap))])
    (super-new)
    (define/override (on-subwindow-char receiver event)
      (or (send keymap handle-key-event this event)
          (super on-subwindow-char receiver event)))
    (define/public (tool:close-window)
      (send this show #f))))

(define (system-shortcut-format str) ;; augh
  (format "~a:~a"
          (case (system-type)
            [(windows unix) ":c"]
            [(macos macosx) ":d"])
          str))

(define (default-make-tool-keymap)
  (define km (new keymap%))
  (send km map-function (system-shortcut-format "w") "tool:close-window")
  ;; ^ Heck. This overrides the :c:w Unix emacs-style binding to cut-clipboard.
  (send km set-grab-key-function
        (lambda (op-str _top-keymap target _event)
          (and op-str
               (dynamic-send target (string->symbol op-str)))))
  km)

(define current-make-tool-keymap (make-parameter default-make-tool-keymap))
