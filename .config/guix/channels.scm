;; [[file:../../Guix.org::*Channels][Channels:1]]
(cons*
 (channel
  (name 'channel-q)
  (url "file:///home/pavel/_channel-q"))
 (channel
  (name 'flat)
  (url "https://github.com/flatwhatson/guix-channel.git")
  (introduction
   (make-channel-introduction
    "33f86a4b48205c0dc19d7c036c85393f0766f806"
    (openpgp-fingerprint
     "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))
 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  ;; (commit "d54973e47b89fe5772a5b6e2d0c0b86acb089e27")
  (introduction
   (make-channel-introduction
    "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
    (openpgp-fingerprint
     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
 (channel
  ;; What can possibly go wrong, huh
  (name 'guix-gaming-games)
  (url "https://gitlab.com/guix-gaming-channels/games.git")
  ;; Enable signature verification:
  (introduction
   (make-channel-introduction
    "c23d64f1b8cc086659f8781b27ab6c7314c5cca5"
    (openpgp-fingerprint
     "50F3 3E2E 5B0C 3D90 0424  ABE8 9BDC F497 A4BB CC7F"))))
 %default-channels)
;; Channels:1 ends here
