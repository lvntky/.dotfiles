;;; guru-mode-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from guru-mode.el

(autoload 'guru-mode "guru-mode" "\
A minor mode that teaches you to use Emacs effectively.

This is a minor mode.  If called interactively, toggle the `guru
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `guru-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(put 'guru-global-mode 'globalized-minor-mode t)
(defvar guru-global-mode nil "\
Non-nil if Guru-Global mode is enabled.
See the `guru-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `guru-global-mode'.")
(custom-autoload 'guru-global-mode "guru-mode" nil)
(autoload 'guru-global-mode "guru-mode" "\
Toggle Guru mode in all buffers.
With prefix ARG, enable Guru-Global mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Guru mode is enabled in all buffers where `guru-mode' would do it.

See `guru-mode' for more information on Guru mode.

(fn &optional ARG)" t)
(register-definition-prefixes "guru-mode" '("guru-"))

;;; End of scraped data

(provide 'guru-mode-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; guru-mode-autoloads.el ends here