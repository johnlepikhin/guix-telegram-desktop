;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
;;; Copyright © 2022, 2023 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2023 Saku Laesvuori <saku@laesvuori.fi>
;;; Copyright © 2023 Lu Hui <luhux76@gmail.com>
;;; Copyright © 2023 Camilo Q.S. (Distopico) <distopico@riseup.net>
;;; Copyright © 2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2024 dan <i@dan.games>
;;; Copyright © 2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2025 Zheng Junjie <z572@z572.online>
;;; Copyright © 2025 Evgenii Lepikhin <johnlepikhin@gmail.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (telegram-desktop packages telegram-desktop)
  #:use-module (srfi srfi-26)
  #:use-module (gnu packages)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages animation)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages c)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages fcitx5)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages language)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lxqt)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages telephony)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix utils))

;; Use abseil-cpp-20250127 because the latest version (20250814) removed
;; template-based absl::Nonnull which is required by tg_owt.
;; The newer abseil replaced it with macros which are not compatible.
(define abseil-cpp-cxxstd17-compat
  (hidden-package
   (package/inherit abseil-cpp-20250127
     (arguments
      (substitute-keyword-arguments (package-arguments abseil-cpp-20250127)
        ((#:configure-flags flags #~'())
         #~(cons* "-DCMAKE_CXX_STANDARD=17"
                  #$flags)))))))

(define %telegram-version "7.1.3")

(define libyuv-for-telegram-desktop
  (let ((commit "04821d1e7d60845525e8db55c7bcd41ef5be9406")
        (revision "2440"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://chromium.googlesource.com/libyuv/libyuv")
            (commit commit)))
      (file-name (git-file-name
                  "libyuv-for-telegram-desktop"
                  (git-version "0" revision commit)))
      (sha256
       (base32
        "1fsvc0f8mckrdzys8lnlnbw6676mjamm6p3ghr2h9liqfa83s6wg")))))

(define cppgir-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://gitlab.com/mnauw/cppgir.git")
          (commit "47cf94f83b54cda59018135601e19d7fb0c77776")))
    (file-name
     (git-file-name "cppgir-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1p5rigy4vc034kgm2zabn8q8sd4drbdhbmygmqzzd3mppbfkw69v"))))

(define tde2e-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/tdlib/td.git")
          (commit "51743dfd01dff6179e2d8f7095729caa4e2222e9")))
    (file-name
     (git-file-name "tde2e-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "009ayf0hlcm6h5fdas5l5qnw46f5s9v1xr5v5ad4k9p9yxmzrq3n"))))

(define cmake-helpers-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/cmake_helpers.git")
          (commit "ce47e85122cfa87d4ba4550049377f5167d31226")))
    (file-name
     (git-file-name "cmake-helpers-for-telegram-desktop" %telegram-version))
    (patches
     (map (lambda (patch)
            (search-path
             (map (cut string-append <> "/telegram-desktop/packages/patches")
                  %load-path)
             patch))
          '("telegram-desktop-unbundle-cppgir-v2.patch")))
    (sha256
     (base32
      "1ik84p16hkgy9k25ds8v0fc3h4pwqvf5ridf06fv690qqs1w554b"))))

(define codegen-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/codegen.git")
          (commit "51cc8c564555914f0cf2f9eba9e9ea9df339192a")))
    (file-name
     (git-file-name "codegen-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0nzlvik4qraq8k4czndza65p2rw6g9abkm5bisvi2rnbjsdsdd2y"))))

(define lib-base-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_base.git")
          (commit "0a0af4d897e12ff75605e615127559b9ed22ff58")))
    (file-name
     (git-file-name "lib-base-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1c2xmnmdkmyd6vswsrbr0pw0d5mjla4nzfvbln8rxk96fcb6qsab"))))

(define lib-crl-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_crl.git")
          (commit "7a165302fed408c84b2d1c2513e35a21a141da44")))
    (file-name
     (git-file-name "lib-crl-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1lxain8npmsjc4milid8862j19pn1rzvic2aj09aq4xdf5i7yhjn"))))

(define lib-lottie-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_lottie.git")
          (commit "192a35ab144bb52260f112433363f397363aec1e")))
    (file-name
     (git-file-name "lib-lottie-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1a08s9zkiy3gi73cvpi40dzdc6j6n7miy4l9hz8rwkvs891li30s"))))

(define lib-qr-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_qr.git")
          (commit "6fdf60461444ba150e13ac36009c0ffce72c4c83")))
    (file-name
     (git-file-name "lib-qr-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1i5n3qvjkf4nv8k75cc0a71xyvpklc4nzg4k2zsfr2pxk0cy7hkw"))))

(define lib-rpl-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_rpl.git")
          (commit "c57cccffb01d85570decd7fccb88419c9a682e63")))
    (file-name
     (git-file-name "lib-rpl-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0s9sza56fsrcd366ii6isc3w27ii2n9k48drn2q4vrq4jy1hy9ln"))))

(define lib-spellcheck-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_spellcheck.git")
          (commit "ac399e5c25d6a56b9e222ef4a52f548d911635ba")))
    (file-name
     (git-file-name "lib-spellcheck-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1zqjzxaghg7pwhidias3d0f0pa0ja673fcywizva9yc4xh11h009"))))

(define lib-storage-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_storage.git")
          (commit "ccdc72548a5065b5991b4e06e610d76bc4f6023e")))
    (file-name
     (git-file-name "lib-storage-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0b11ix95dzpkz335q0a6b5yg8qhj33s4fgj9ppl37pszcqq1j3wi"))))

(define lib-tl-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_tl.git")
          (commit "aa7791326d9b8516fa0f8086599414c7bd645382")))
    (file-name
     (git-file-name "lib-tl-for-telegram-desktop" %telegram-version))
    (patches
     (map (lambda (patch)
            (search-path
             (map (cut string-append <> "/telegram-desktop/packages/patches")
                  %load-path)
             patch))
          '("lib-tl-for-telegram-memcpy.patch")))
    (sha256
     (base32
      "0hg3c5p3175wz7dradc2jla6aj1firk59g9hh6b5dyb5f6qk6xks"))))

(define lib-translate-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_translate.git")
          (commit "09c10726220e6862ca91d39b5dec5119aa0177bc")))
    (file-name
     (git-file-name "lib-translate-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "05z9bz2gcc952afr19q9z00hnrdx1j3ww7bddazpb2f4b86xi5yc"))))

(define lib-ui-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_ui.git")
          (commit "ca3578b0e16ac6ac59e06a37df5cee2d4475d52a")))
    (file-name
     (git-file-name "lib-ui-for-telegram-desktop" %telegram-version))
    (patches
     (map (lambda (patch)
            (search-path
             (map (cut string-append <> "/telegram-desktop/packages/patches")
                  %load-path)
             patch))
          '("lib-ui-accessible-orientation-qt69.patch")))
    (sha256
     (base32
      "06zkl7mwaf2wgc1zd2z45wh9dsiqr5fkkczcdxszfqg6mqlzpl1h"))))

(define lib-webrtc-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_webrtc.git")
          (commit "52636e86eaa493de670daf71959d000b281bd153")))
    (file-name
     (git-file-name "lib-webrtc-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1y50q6b33r506qn4pw0sn6cmxhwjcxdb79qrw7laz45l1iw1zhm0"))))

(define lib-webview-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_webview.git")
          (commit "71301e35911f0801ee2890c8b4f04555e34d4f72")))
    (file-name
     (git-file-name "lib-webview-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0c7rk57bspcsj4x92alqpk0hz4d4pgpgylma2bi7abhvmjc1ycgz"))))

(define tgcalls-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/TelegramMessenger/tgcalls.git")
          (commit "2faee3b5524f54d56c91c2058c00e11c656a74b3")))
    (file-name
     (git-file-name "tgcalls-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0azwkwj1l91qrv41nn2ik3h1q4j849dki43xwzmqdfdwjbqgcv7v"))))

(define fcitx5-qt-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/fcitx/fcitx5-qt.git")
          (commit "c743b12e6780edf1dcfe9071531c80f050cacb95")))
    (file-name
     (git-file-name "fcitx5-qt-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1qqq2h4jkh81x3ali4vjk4vnyxp63ma9kqspkhz791gkrqshkl56"))))

(define-public webrtc-for-telegram-desktop
  (let ((commit "26068e29bfa8d74a9dc9c8f7f94172fafbc262b8")
        (revision "491"))
    (hidden-package
     (package
       (name "webrtc-for-telegram-desktop")
       (version
        (git-version "0" revision commit))
       (source
        (origin
          (method git-fetch)
          (uri
           (git-reference
            (url "https://github.com/desktop-app/tg_owt.git")
            (commit commit)))
          (file-name
           (git-file-name name version))
          (sha256
           (base32 "0c4p6maab1nv75ldslqxr36zcm94h2ll0r0n9yqz55lgwdgxdpw7"))
          (modules '((guix build utils)
                     (ice-9 ftw)
                     (srfi srfi-1)))
          (snippet
           #~(begin
               (let ((keep
                      '("rnnoise"
                        ;; Not available in Guix.
                        "pffft")))
                 (with-directory-excursion "src/third_party"
                   (for-each delete-file-recursively
                             (lset-difference string=?
                                              (scandir ".")
                                              (cons* "." ".." keep)))))
               ;; Unbundle abseil-cpp, crc32c and openh264.
               (substitute* "CMakeLists.txt"
                 (("\\include\\(cmake\\/libopenh264\\.cmake\\)")"")
                 (("\\include\\(cmake\\/libabsl\\.cmake\\)")"")
                 (("\\include\\(cmake\\/libcrc32c\\.cmake\\)")""))))))
       (build-system cmake-build-system)
       (arguments
        (list
         #:tests? #f                    ; No target
         #:phases
         #~(modify-phases %standard-phases
             (add-after 'unpack 'unpack-additional-sources
               (lambda _
                 (let* ((third-party (string-append (getcwd) "/src/third_party"))
                        (libyuv-to (string-append third-party "/libyuv")))
                   (copy-recursively #$libyuv-for-telegram-desktop
                                     libyuv-to)))))))
       (native-inputs (list pkg-config python-wrapper yasm))
       (inputs
        (list abseil-cpp-cxxstd17-compat
              crc32c
              ffmpeg
              glib
              glibmm
              libdrm
              libglvnd
              libjpeg-turbo
              libsrtp
              libvpx
              libxcomposite
              libxdamage
              libxext
              libxfixes
              libxrandr
              libxrender
              libxtst
              mesa
              openh264
              openssl
              opus
              pipewire
              protobuf))
       (synopsis "WebRTC support for Telegram Desktop")
       (description "WebRTC-for-Telegram-Desktop is a custom WebRTC fork by
Telegram project, for its use in telegram desktop client.")
       (home-page "https://github.com/desktop-app/tg_owt")
       (license
        (list
         ;; Abseil-CPP
         license:asl2.0
         ;; LibYuv
         (license:non-copyleft "file:///src/third_party/libyuv/LICENSE")
         ;; PFFFT
         (license:non-copyleft "file:///src/third_party/pffft/LICENSE")
         ;; RnNoise
         license:gpl3
         ;; LibSRTP, Crc32c and Others
         license:bsd-3))))))

;; Since cmake_helpers dropped DESKTOP_APP_USE_PACKAGED_RLOTTIE, the
;; desktop-app fork of rlottie is compiled from the bundled sources.
(define rlottie-for-telegram-desktop
  (origin
   (method git-fetch)
   (uri (git-reference
         (url "https://github.com/desktop-app/rlottie.git")
         (commit "dcfe6f62f27522cda4f06f6d6b497ed7fd0917e9")))
   (file-name
    (git-file-name "rlottie-for-telegram-desktop" %telegram-version))
   (sha256
    (base32
     "0lg0mjcpp7drr1wb7aqxik7014xkh1ng7hcfihjvdi02gvmpzf3k"))))

(define cld3-for-telegram-desktop
  (origin
   (method git-fetch)
   (uri (git-reference
         (url "https://github.com/google/cld3.git")
         (commit "b48dc46512566f5a2d41118c8c1116c4f96dc661")))
   (file-name
    (git-file-name "cld3-for-telegram-desktop" %telegram-version))
   (sha256
    (base32
     "0ayrrhfdwrf4260h9fsirkhhfrcvc3qqnh6h9wj3ixij2lq0wwqb"))))

(define libprisma-for-telegram-desktop
  (origin
   (method git-fetch)
   (uri (git-reference
         (url "https://github.com/desktop-app/libprisma")
         (commit "23b0d70f9709da9b38561d5706891a134d18df76")))
   (file-name
    (git-file-name "libprisma-for-telegram-desktop" %telegram-version))
   (sha256
    (base32
     "0fg4x4ikj7f3706bmfvkwq4smxc98qr3cgpm25w48n4ys6wfgadg"))))

;; Telegram Desktop 7.x links a desktop-app fork of cmark-gfm; there is no
;; system-package fallback in cmake/external/cmark_gfm.
(define cmark-gfm-for-telegram-desktop
  (origin
   (method git-fetch)
   (uri (git-reference
         (url "https://github.com/desktop-app/cmark-gfm.git")
         (commit "d7d4a24a9ebfa7581994ea6df0297662bfeaf413")))
   (file-name
    (git-file-name "cmark-gfm-for-telegram-desktop" %telegram-version))
   (sha256
    (base32
     "1xil7lq50qvz623r8v3d92iggb871j9wsc3s7z6v4x7nrn3x8mg0"))))

;; MicroTeX fork with Telegram-specific fixes; cmake/external/microtex has no
;; packaged fallback either.
(define microtex-for-telegram-desktop
  (origin
   (method git-fetch)
   (uri (git-reference
         (url "https://github.com/desktop-app/MicroTeX.git")
         (commit "61aaa7cc354de91d5898ffb0b2a6c62628d9a76f")))
   (file-name
    (git-file-name "microtex-for-telegram-desktop" %telegram-version))
   (sha256
    (base32
     "1isplcrdwyadlgz251817096y793cnmqms3ial36hvbigjq60qf4"))))

;; Header-only task scheduler, not available in Guix.
(define tmc-for-telegram-desktop
  (origin
   (method git-fetch)
   (uri (git-reference
         (url "https://github.com/tzcnt/TooManyCooks.git")
         (commit "b86af81982860c96295a7e95e4c60cb335615cef")))
   (file-name
    (git-file-name "tmc-for-telegram-desktop" %telegram-version))
   (sha256
    (base32
     "1lpmddx0plhl2g3h86lbv184iaf38gsyjkd1bczv7q9xq4fclppr"))))

(define kimageformats-for-telegram-desktop
  (origin
   (method git-fetch)
   (uri (git-reference
         (url "https://github.com/KDE/kimageformats.git")
         (commit "df82311a1081e576c4ac020204578bb8a81b21ec")))
   (file-name
    (git-file-name "kimageformats-for-telegram-desktop" %telegram-version))
   (sha256
    (base32
     "0pazpqm1arm9685hv6hfygvpmqfyfy2mx6w5rzjfy8w84hm6fl79"))))

(define nimf-for-telegram-desktop
  (origin
   (method git-fetch)
   (uri (git-reference
         (url "https://github.com/hamonikr/nimf.git")
         (commit "498ec7ffab3ac140c2469638a14451788f03e798")))
   (file-name
    (git-file-name "nimf-for-telegram-desktop" %telegram-version))
   (sha256
    (base32
     "0xsyh12cn19sa699ly5hs7s9356x7nsiy17ghin91ajwdrlci5vh"))))

(define hime-for-telegram-desktop
  (origin
   (method git-fetch)
   (uri (git-reference
         (url "https://github.com/hime-ime/hime.git")
         (commit "9b3e6f9ab59d1fe4d9de73d3bf0fed7789f921c5")))
   (file-name
    (git-file-name "hime-for-telegram-desktop" %telegram-version))
   (sha256
    (base32
     "1b98ql30ihbqz1zwlm0c3c1qmxp3gdz96wsic3h6ypqcr3xyj28f"))))

(define-public tde2e
  (package
    (name "tde2e")
    (version "tdlib-51743df")
    (source tde2e-for-telegram-desktop)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f  ; No tests in E2E-only mode
           #:build-type "Release"           ; Reduce memory requirements
           #:configure-flags
           #~(list "-DTD_E2E_ONLY=ON")))
    (native-inputs
     (list gperf))
    (inputs
     (list openssl zlib))
    (home-page "https://github.com/tdlib/td")
    (synopsis "End-to-end encryption library for Telegram")
    (description
     "TdE2E provides end-to-end encryption functionality for Telegram
applications, extracted from the TDLib project.  This library enables
secure group calls with end-to-end encryption.")
    (license license:boost1.0)))

(define-public telegram-desktop
  (package
    (name "telegram-desktop")
    (version %telegram-version)
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/telegramdesktop/tdesktop.git")
         (commit
          (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "15dkj3pzh4h5fr3isplpc722y7wczscihhrs10yrcf74sxcr2hd2"))
       (patches
        (map (lambda (patch)
               (search-path
                (map (cut string-append <> "/telegram-desktop/packages/patches")
                     %load-path)
                patch))
             '(;; Make it compatible with GCC 11.
               "telegram-desktop-qguiapp.patch"
               "telegram-desktop-hashmap-incomplete-value.patch"
               ;; Fix LaunchMaps for xdg-desktop-portal < 1.19.1
               "telegram-desktop-fix-launch-maps.patch")))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-1)))
       (snippet
        #~(begin
            (let ((keep
                   '(;; Not available in Guix.
                     "tgcalls" "cld3" "fcitx5-qt" "kimageformats" "nimf" "hime"
                     "cmark-gfm" "MicroTeX" "TooManyCooks" "rlottie")))
              (with-directory-excursion "Telegram/ThirdParty"
                (for-each delete-file-recursively
                          (lset-difference string=?
                                           (scandir ".")
                                           (cons* "." ".." keep)))))))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:tests? #f                      ; No target
           #:build-type "Release"           ; Reduce memory requirements
           #:imported-modules
           `(,@%qt-build-system-modules
             (guix build glib-or-gtk-build-system))
           #:modules
           '((guix build qt-build-system)
             ((guix build glib-or-gtk-build-system)
              #:prefix glib-or-gtk:)
             (guix build utils)
             (ice-9 match))
           #:configure-flags
           #~(list
              ;; Client applications must provide their own API-ID and API-HASH,
              ;; see also <https://core.telegram.org/api/obtaining_api_id>.
              ;; Here, we snarf the keys from the official Snaps, which are
              ;; also stored in <#$source/snap/snapcraft.yaml>.
              "-DTDESKTOP_API_ID=611335"
              "-DTDESKTOP_API_HASH=d524b414d21f4d37f08684c1df41ac9c"
              "-DDESKTOP_APP_DISABLE_CRASH_REPORTS=ON"
              "-DDESKTOP_APP_DISABLE_AUTOUPDATE=ON")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'unpack-additional-sources
                 (lambda _
                   (for-each make-file-writable (find-files "."))
                   (for-each
                    (match-lambda
                      ((dst src)
                       (copy-recursively src dst)
                       (for-each make-file-writable (find-files dst))))
                    '(("cmake" #$cmake-helpers-for-telegram-desktop)
                      ("cmake/external/glib/cppgir" #$cppgir-for-telegram-desktop)
                      ("Telegram/codegen" #$codegen-for-telegram-desktop)
                      ("Telegram/lib_base" #$lib-base-for-telegram-desktop)
                      ("Telegram/lib_crl" #$lib-crl-for-telegram-desktop)
                      ("Telegram/lib_lottie" #$lib-lottie-for-telegram-desktop)
                      ("Telegram/lib_qr" #$lib-qr-for-telegram-desktop)
                      ("Telegram/lib_rpl" #$lib-rpl-for-telegram-desktop)
                      ("Telegram/lib_spellcheck" #$lib-spellcheck-for-telegram-desktop)
                      ("Telegram/lib_storage" #$lib-storage-for-telegram-desktop)
                      ("Telegram/lib_tl" #$lib-tl-for-telegram-desktop)
                      ("Telegram/lib_translate" #$lib-translate-for-telegram-desktop)
                      ("Telegram/lib_ui" #$lib-ui-for-telegram-desktop)
                      ("Telegram/lib_webrtc" #$lib-webrtc-for-telegram-desktop)
                      ("Telegram/lib_webview" #$lib-webview-for-telegram-desktop)
                      ("Telegram/ThirdParty/cld3" #$cld3-for-telegram-desktop)
                      ("Telegram/ThirdParty/fcitx5-qt" #$fcitx5-qt-for-telegram-desktop)
                      ("Telegram/ThirdParty/libprisma" #$libprisma-for-telegram-desktop)
                      ("Telegram/ThirdParty/tgcalls" #$tgcalls-for-telegram-desktop)
                      ("Telegram/ThirdParty/kimageformats"
                       #$kimageformats-for-telegram-desktop)
                      ("Telegram/ThirdParty/nimf" #$nimf-for-telegram-desktop)
                      ("Telegram/ThirdParty/hime" #$hime-for-telegram-desktop)
                      ("Telegram/ThirdParty/cmark-gfm"
                       #$cmark-gfm-for-telegram-desktop)
                      ("Telegram/ThirdParty/MicroTeX"
                       #$microtex-for-telegram-desktop)
                      ("Telegram/ThirdParty/TooManyCooks"
                       #$tmc-for-telegram-desktop)
                      ("Telegram/ThirdParty/rlottie"
                       #$rlottie-for-telegram-desktop)))))
               (add-after 'unpack-additional-sources 'setup-expected-lite-for-cppgir
                 (lambda _
                   ;; cppgir needs expected-lite, provide it from system package
                   (let ((expected-lite-include
                          (string-append #$(this-package-input "expected-lite")
                                         "/include")))
                     (copy-recursively
                      expected-lite-include
                      "cmake/external/glib/cppgir/expected-lite/include"))))
               (add-after 'unpack-additional-sources 'use-system-xdg-desktop-portal
                 (lambda _
                   (substitute* (list "Telegram/CMakeLists.txt"
                                      "Telegram/lib_base/CMakeLists.txt")
                     (("\\$\\{third_party_loc\\}/xdg-desktop-portal/data")
                      (string-append
                       #$(this-package-native-input "xdg-desktop-portal")
                       "/share/dbus-1/interfaces")))))
               ;; Remove a problematic 'constexpr' keyword, otherwise
               ;; compilation fails with GCC 11.
               (add-after 'use-system-xdg-desktop-portal 'patch-libwebview
                 (lambda _
                   (substitute* "Telegram/lib_webview/webview/webview_interface.h"
                     (("constexpr ") ""))))
               (add-after 'install 'glib-or-gtk-compile-schemas
                 (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
               (add-after 'glib-or-gtk-compile-schemas 'glib-or-gtk-wrap
                 (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap))
               (add-after 'glib-or-gtk-wrap 'create-telegram-desktop-symlink
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (bin (string-append out "/bin")))
                     (with-directory-excursion bin
                       (symlink "Telegram" "telegram-desktop"))))))))
    (native-inputs
     (list cpp-ada-url-parser
           gperf
           `(,glib "bin")
           gobject-introspection
           `(,gtk+ "bin")
           pkg-config
           python-wrapper
           ;; Provides qsb, needed since 7.x to bake the QRhi shaders.
           qtshadertools
           xdg-desktop-portal))
    (inputs
     (list abseil-cpp-cxxstd17-compat
           alsa-lib
           boost
           c++-gsl
           crc32c
           expected-lite
           ffmpeg
           glib
           glibmm-2.76
           gtk+
           hime
           hunspell
           kcoreaddons
           kimageformats
           libdispatch
           libexpected
           libfido2
           libjpeg-turbo
           libvpx
           libxcb
           lz4
           minizip
           nimf
           openal
           openssl
           opus
           plasma-wayland-protocols
           pulseaudio
           protobuf
           qrcodegen-cpp
           qtdeclarative
           qtimageformats
           qtsvg
           qtwayland
           range-v3
           tde2e
           rnnoise
           wayland
           wayland-protocols
           webkitgtk-for-gtk3
           webrtc-for-telegram-desktop
           xcb-util-keysyms
           xxhash
           zlib))
    (synopsis "Telegram Desktop")
    (description "Telegram desktop is the official desktop version of the
Telegram instant messenger.")
    (home-page "https://github.com/telegramdesktop/tdesktop")
    (license
     (list
      ;; ThirdParty
      license:lgpl3
      ;; Others
      license:gpl3+))))
