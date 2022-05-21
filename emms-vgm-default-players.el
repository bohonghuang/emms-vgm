;;; emms-vgm-default.el --- Predefined players for emms-vgm -*- lexical-binding: t -*-

;;; Commentary:

;; This file defines a collection of command line players used to play Video Game Music.

;;; Code:
(require 'emms-vgm)

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
  '("gsf")
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

(defconst emms-vgm-default-players '(emms-player-audacious
                                     emms-player-gme
                                     emms-player-playgsf
                                     emms-player-zxtune
                                     emms-player-vgmstream
                                     emms-player-gbsplay
                                     emms-player-fluidsynth)
  "The default players provided by `emms-vgm', sorted by functional integrity.")

(provide 'emms-vgm-default-players)
;;; emms-vgm-default-players.el ends here
