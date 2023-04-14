;;; emms-vgm.el --- Video Game Music support for EMMS -*- lexical-binding: t -*-

;;; Commentary:
;;; This package adds Video Game Music simple playback support for EMMS, based on various command line Video Game Music players.

;;; Code:
(require 'cl-lib)

(require 'emms)
(require 'emms-player-simple)
(require 'emms-vgm-m3u)

(eval-when-compile (require 'cl-lib))

(defconst emms-vgm-player-excluded-extensions '("ogg" "mp3" "flac" "ape" "aac" "wav" "wma" "opus" "aiff"))

(defvar emms-vgm-player-endless-loop-p nil)

(defun emms-vgm-player-toggle-endless-loop ()
  (interactive)
  (message "VGM endless loop %s" (if (setq emms-vgm-player-endless-loop-p (not emms-vgm-player-endless-loop-p)) "enabled" "disabled")))

(defun emms-vgm-player-supports-endless-loop-p (player)
  (not (null (member 'endless-loop (emms-player-get player 'features)))))

(defun emms-vgm-player-supports-track-index-p (player)
  (not (null (member 'track-index (emms-player-get player 'features)))))

(defvar emms-vgm-track-index nil)

(defun emms-vgm-track-index-playable-p ()
  (and (emms-vgm-player-supports-track-index-p emms-player-playing-p)
       (not (emms-track-get (emms-playlist-current-selected-track) 'index))))

(defun emms-vgm-track-index-reset ()
  (setq emms-vgm-track-index nil))

(defun emms-vgm-track-set-index (index)
  (interactive "nEnter track index: ")
  (if (emms-vgm-track-index-playable-p)
      (progn
        (when emms-player-playing-p
          (let ((emms-vgm-track-index nil))
            (emms-stop)))
        (setq emms-vgm-track-index index)
        (emms-start)
        (message "Current track index: %d" emms-vgm-track-index))
    (emms-vgm-track-index-playable-p)))

(defun emms-vgm-next-track-index ()
  (interactive)
  (emms-vgm-track-set-index (1+ (or emms-vgm-track-index 0))))

(defun emms-vgm-previous-track-index ()
  (interactive)
  (emms-vgm-track-set-index (1- (or emms-vgm-track-index 0))))

(defun emms-vgm-previous-track-or-index (function)
  (condition-case nil
      (progn
        (emms-playlist-current-select-previous)
        (emms-playlist-current-select-next)
        (funcall function))
    (error (if (emms-vgm-track-index-playable-p)
               (emms-vgm-previous-track-index)
             (funcall function)))))

(advice-add #'emms-previous :around #'emms-vgm-previous-track-or-index)

(defun emms-vgm-next-track-or-index (function)
  (condition-case nil
      (progn
        (emms-playlist-current-select-next)
        (emms-playlist-current-select-previous)
        (funcall function))
    (error (if (emms-vgm-track-index-playable-p)
               (emms-vgm-next-track-index)
             (funcall function)))))

(advice-add #'emms-next :around #'emms-vgm-next-track-or-index)

(defun emms-vgm-player-start (player cmdname params)
  (let ((process (apply #'start-process
                        emms-player-simple-process-name
                        nil
                        cmdname
                        params)))
    (set-process-sentinel process #'emms-player-simple-sentinel))
  (emms-player-started player))

(defmacro define-emms-vgm-player (name extensions command &rest args)
  (let* ((flatten-args (flatten-list args))
         (supports-loop (member 'loop flatten-args))
         (supports-index (member 'index flatten-args))
         (supports-params (member 'params flatten-args))
         (requires-file (member 'file flatten-args))
         (player-name (intern (concat "emms-player-" (symbol-name name))))
         (player-extensions-name (intern (concat "emms-player-" (symbol-name name) "-extensions")))
         (hash-table-name (intern (concat "emms-player-" (symbol-name name) "-extension-hash-table")))
         (playable-p-name (intern (concat "emms-player-" (symbol-name name) "-playable-p")))
         (player-start-name (intern (concat "emms-player-" (symbol-name name) "-start")))
         (player-command-name (intern (concat "emms-player-" (symbol-name name) "-command-name")))
         (player-params-name (intern (concat "emms-player-" (symbol-name name) "-parameters"))))
    `(progn
       (defgroup ,player-name nil
         ,(concat "EMMS player for " (symbol-name name) ".")
         :group 'emms-player
         :prefix ,(concat (symbol-name player-name) "-"))
       (defcustom ,player-command-name ,command
         "The command name of alsaplayer."
         :type 'string)
       ,(when supports-params
          `(defcustom ,player-params-name nil
             ,(concat "The arguments to `" (symbol-name player-command-name) "'.")
             :type
             '(repeat string)))
       (defconst ,player-extensions-name ,extensions)
       (defconst ,hash-table-name
         (let ((hash-table (make-hash-table :test 'equal)))
           (dolist (extension (cl-set-difference ,player-extensions-name emms-vgm-player-excluded-extensions :test #'string-equal) hash-table)
             (puthash extension t hash-table))))
       (defcustom ,player-name
         (emms-player #',player-start-name #'emms-player-simple-stop #',playable-p-name)
         "A player for EMMS." :type
         '(cons symbol alist))
       (emms-player-set ,player-name 'pause 'emms-player-simple-pause)
       (emms-player-set ,player-name 'resume 'emms-player-simple-resume)
       (defun ,playable-p-name (track)
         "Return non-nil when we can play this track."
         (and
          (executable-find ,player-command-name)
          (memq
           (emms-track-type track)
           '(file))
          (gethash (downcase (string-trim-left (file-name-extension (emms-track-name track)) "mini")) ,hash-table-name nil)))
       (emms-player-set ,player-name 'features ',(cl-remove-if #'null (list (when supports-loop 'endless-loop)
                                                                            (when supports-index 'track-index))))
       (defun ,player-start-name (track)
         "Start the player process."
         (let ,(cl-remove-if #'null (list (when requires-file `(file (emms-track-name track)))
                                          (when supports-index `(index (or (emms-track-get track 'index) emms-vgm-track-index)))
                                          (when supports-params `(params ,player-params-name))
                                          (when supports-loop `(loop emms-vgm-player-endless-loop-p))))
           (emms-vgm-player-start
            ,player-name ,player-command-name (flatten-list (list ,@args))))))))

(add-hook 'emms-player-finished-hook #'emms-vgm-track-index-reset)
(add-hook 'emms-player-stopped-hook #'emms-vgm-track-index-reset)
(add-hook 'emms-playlist-selection-changed-hook #'emms-vgm-track-index-reset)

(provide 'emms-vgm)
;;; emms-vgm.el ends here
