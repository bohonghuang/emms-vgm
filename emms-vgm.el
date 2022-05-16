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

(defun emms-vgm-player-simple-regexp (&rest extensions)
  (concat "\\." "\\(mini\\)*" (s-chop-prefix "\\." (apply #'emms-player-simple-regexp (cl-set-difference extensions emms-vgm-player-excluded-extensions :test #'string-equal)))))

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

(defun emms-vgm-track-index-offset (offset)
  (if (emms-vgm-track-index-playable-p)
    (progn
      (when emms-player-playing-p
        (let ((emms-vgm-track-index nil))
          (emms-stop)))
      (setq emms-vgm-track-index (+ (or emms-vgm-track-index 0) offset))
      (emms-start)
      (message "Current track index: %d" emms-vgm-track-index))
    (message "Indexed track playing is not support by current player or track")))

(defun emms-vgm-track-index-next ()
  (interactive)
  (emms-vgm-track-index-offset +1))

(defun emms-vgm-track-index-previous ()
  (interactive)
  (emms-vgm-track-index-offset -1))

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
          (gethash (string-trim-left (file-name-extension (emms-track-name track)) "mini") ,hash-table-name nil)))
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

(defun emms-vgm-player-start (player cmdname params)
  (let ((process (apply #'start-process
                        emms-player-simple-process-name
                        nil
                        cmdname
                        params)))
    (set-process-sentinel process #'emms-player-simple-sentinel))
  (emms-player-started player))

(define-emms-vgm-player vgmstream
  '("2dx9" "aaap" "aax" "acm" "adp" "adpcm" "ads" "adx" "afc" "agsc" "ahx" "aifc" "aix"
    "amts" "as4" "asd" "asf" "asr" "ass" "ast" "at3" "aud" "aus" "baf" "baka" "bao"
    "bar" "bcstm" "bg00" "bgw" "bh2pcm" "bmdx" "bns" "bnsf" "bo2" "brstm"
    "caf" "capdsp" "ccc" "cfn" "cnk" "ddsp" "de2" "dec" "dmsg" "dsp" "dvi"
    "dxh" "eam" "emff" "enth" "fag" "filp" "fsb" "genh" "gca" "gcm" "gcsw" "gcw"
    "gms" "gsp" "hca" "hgc1" "his" "hps" "hwas" "idsp" "idvi" "ikm" "ild"
    "int" "isd" "ish" "ivaud" "ivb" "joe" "kces" "kcey" "khv" "kraw"
    "laac" "leg" "lflac" "logg" "lopus" "lps" "lsf" "lstm" "lwav" "matx" "mc3"
    "mca" "mcg" "mi4" "mib" "mic" "mih" "mihb" "mpdsp" "msa" "msf" "mss"
    "msvp" "mta2" "mtaf" "mus" "musc" "musx" "mwv" "myspd" "ndp" "npsf"
    "nwa" "ogl" "p3d" "pcm" "pdt" "pk" "pnb" "ps2stm" "psh" "psw" "raw"
    "rkv" "rnd" "rrds" "rsd" "rsf" "rstm" "rwar" "rwav" "rws" "rwsd" "rwx"
    "rxw" "s14" "sab" "sad" "sap" "sb0" "sb1" "sb2" "sb3" "sb4" "sb5"
    "sb6" "sb7" "sc" "scd" "sd9" "sdt" "seg" "sfs" "sgb" "sgd" "sgh" "sgx"
    "sl3" "sm0" "sm1" "sm2" "sm3" "sm4" "sm5" "sm6" "sm7" "smp" "smpl"
    "snd" "sng" "sns" "sps" "spsd" "spw" "ss2" "ssm" "sss" "ster" "sth"
    "stm" "stma" "str" "strm" "sts" "stx" "svag" "svs" "swav" "swd" "tec"
    "thp" "tk5" "tydsp" "ulw" "um3" "vag" "vas" "vgs" "vig" "vjdsp" "voi"
    "vpk" "vs" "vsf" "waa" "wac" "wad" "wam" "was" "wavm" "wb" "wem" "wii"
    "wp2" "wsd" "wsi" "wvs" "xa" "xa2" "xa30" "xma" "xmu" "xss" "xvas"
    "xwav" "xwb" "xwh" "ydsp" "ymf" "zsd" "zwdsp")
  "vgmstream123" (when loop "-c") params file)

(define-emms-vgm-player playgsf
  '("gbs")
  "playgsf" "-c" "-q" (when loop "-e") params file)

(define-emms-vgm-player gbsplay
  '("gbs")
  "gbsplay" (when loop "-l") params file (if-let ((index (and index (number-to-string index))))
                                             (list index index)))

(define-emms-vgm-player gme
  '("ay" "gbs" "gym" "hes" "kss" "nsf" "nsfe" "sap" "spc" "vgm" "vgz")
  "gst-launch-1.0" "filesrc" "location" "=" file "!" "gmedec" "!" "autoaudiosink")

(define-emms-vgm-player zxtune
  '("2dx9" "2msf" "2sf" "669" "7z" "aac" "aax" "ac3" "acb" "acm" "adp"
    "ads" "adx" "ahx" "aifc" "aiff" "aix" "akb" "amf" "ams" "ape" "as0"
    "asc" "asc0" "asc1" "asc2" "asf" "ast" "aud" "aus" "awb" "awc" "ay"
    "ayc" "baf" "bao" "bgw" "bik" "bmp" "bnk" "bwav" "c67" "caf" "cc3"
    "cc4" "cc4plus" "charpres" "chi" "ckd" "cks" "cop" "cpk" "cstm" "cwav"
    "dbm" "dmf" "dmm" "dsf" "dsk" "dsm" "dsp" "dspw" "dsq" "dst" "dsym"
    "dtm" "dtt" "dvi" "emod" "esv" "et1" "far" "fdi" "flac" "fmt" "fnk"
    "fsb" "fstm" "ftc" "fwav" "gam" "gamplus" "gbs" "gca" "gdm" "genh"
    "gsf" "gtk" "gtr" "gym" "gz" "h4m" "hca" "hes" "hobeta" "hps" "hrip"
    "hrum" "hrust" "hvl" "hwas" "idsp" "ifs" "ikm" "imf" "ims" "imus"
    "isd" "isws" "it" "j2b" "kcey" "kss" "kssx" "ktss" "kvs" "lha" "liq"
    "logg" "lopus" "lwav" "lzh" "lzh1" "lzh2" "lzs" "m4a" "mab" "mca"
    "mdl" "med" "megalz" "mic" "mo3" "mod" "mp3" "mpc" "msf" "msp" "mt2"
    "mtc" "mtm" "mtx" "mul" "musc" "muse" "musx" "naac" "ncsf" "nps" "nsf"
    "nsfe" "nub" "nus3" "nxa" "ogg" "okt" "oma" "opus" "p3d" "pack2" "pcd"
    "pdt" "plm" "pona" "psc" "psf" "psf2" "psg" "psm" "pt1" "pt2" "pt24"
    "pt3" "ptm" "ptu13" "rak" "rar" "raw" "rfrm" "rmt" "rpgmvo" "rsd"
    "rstm" "rtm" "rwav" "rws" "rxw" "s3m" "s98" "sab" "sad" "sap" "scd"
    "schl" "scl" "sd9" "sdf" "seb" "seg" "sfx" "sid" "smk" "smp" "sna128"
    "snr" "spc" "sps" "spsd" "sqd" "sqt" "ssf" "st1" "st3" "stc" "stim"
    "stm" "stp" "stp1" "stp2" "str" "strm" "stx" "svag" "swav" "symmod"
    "tak" "tcb" "td0" "tf0" "tfc" "tfd" "tfe" "thp" "tlz" "tlzp" "trd"
    "trush" "ts" "txt" "ult" "umx" "usf" "v2m" "vag" "vgm" "vgs" "vpk"
    "vtx" "vxn" "wav" "wave" "wem" "wmsf" "xa" "xm" "xma" "xnb" "xvag"
    "xwb" "xwc" "xwm" "xws" "xwv" "ydsp" "ym" "z80" "z80v145" "z80v20"
    "z80v30" "zdata" "zip" "zsd" "zss" "zxstate" "zxzip")
  "zxtune123" (when loop "--loop") params
  (concat file (when index (format "?#%d" index))))

(define-emms-vgm-player audacious
  (append emms-player-vgmstream-extensions emms-player-gme-extensions '("mid" "psf" "psf2" "sid" "vtx" "flac" "ogg" "2sf"))
  "audacious" "-H" "-99" params file)

(defconst emms-vgm-players-default '(emms-player-audacious
                                     emms-player-gme
                                     emms-player-playgsf
                                     emms-player-zxtune
                                     emms-player-gbsplay
                                     emms-player-vgmstream
                                     emms-player-fluidsynth)
  "The default players provided by `emms-vgm', sorted by functional integrity.")

(add-hook 'emms-player-finished-hook #'emms-vgm-track-index-reset)
(add-hook 'emms-player-stopped-hook #'emms-vgm-track-index-reset)
(add-hook 'emms-playlist-selection-changed-hook #'emms-vgm-track-index-reset)

(provide 'emms-vgm)
;;; emms-vgm.el ends here
