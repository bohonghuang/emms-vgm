;;; emms-vgm-m3u.el --- Video Game Music m3u playlist support for EMMS -*- lexical-binding: t -*-

;;; Commentary:
;;; This package parses Video Game Music specified m3u playlist to make it recognized by EMMS.

;;; Code:
(require 'parsec)

(require 'emms-source-playlist)

(defun emms-vgm-m3u-parsec-number ()
  (parsec-re "[[:digit:]]+"))

(defun emms-vgm-m3u-parsec-string ()
  (parsec-many-s
   (parsec-or
    (progn (parsec-str "\\,") ",")
    (parsec-none-of ?, ?\n ?\r))))

(defun emms-vgm-m3u-parsec-separator ()
  (parsec-re "[[:blank:]]*,[[:blank:]]*"))

(defun emms-vgm-m3u-parsec-track ()
  (append (list (parsec-until-s (parsec-str "::")))
          (parsec-sepby (emms-vgm-m3u-parsec-string)
                        (emms-vgm-m3u-parsec-separator))))

(defun emms-vgm-m3u-transform-track (track)
  (pcase (parsec-with-input (emms-track-name track) (emms-vgm-m3u-parsec-track))
    (`(,name
       ,plugin
       ,index
       ,title
       ,_
       ,_
       ,_)
     (let ((track (emms-dictionary '*track*))
           (emms-cache-modified-function nil))
       (pcase-let ((`(,title ,artist ,copyright) (split-string title "[[:blank:]]+-[[:blank:]]+")))
         (emms-track-set track 'info-title title)
         (when artist (emms-track-set track 'info-artist artist))
         (when copyright (emms-track-set track 'info-copyright copyright)))
       (emms-track-set track 'name name)
       (emms-track-set track 'type 'file)
       (emms-track-set track 'index (+ (string-to-number index) (if (string= plugin "GBS") 1 0)))
       track))
    (_ track)))

(advice-add #'emms-source-playlist-parse-m3u :filter-return (apply-partially #'mapcar #'emms-vgm-m3u-transform-track))

(provide 'emms-vgm-m3u)
;;; emms-vgm-m3u.el ends here
