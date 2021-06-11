;; [[file:../../../Guix.org::*blue][blue:1]]
(use-modules (gnu))
(use-modules (gnu system nss))
(use-modules (gnu packages bash))
(use-modules ((gnu packages base) #:select (coreutils glibc)))
(use-modules (gnu packages certs))
(use-modules (gnu packages version-control))
(use-modules (gnu packages vim))
(use-modules (gnu packages gnome))
(use-modules (gnu packages xorg))
(use-modules (gnu packages wm))
(use-modules (gnu packages openbox))
(use-modules (srfi srfi-1))
(use-modules (guix channels))
(use-modules (guix inferior))
(use-modules (nongnu packages linux))
(use-modules (nongnu system linux-initrd))

(use-service-modules desktop networking ssh xorg)
(use-package-modules ssh)
(define %my-desktop-services
  (modify-services %desktop-services
                   (network-manager-service-type config =>
                                                 (network-manager-configuration (inherit config)
                                                                                (vpn-plugins (list network-manager-openvpn))))))


(operating-system
  (kernel
   (let*
       ((channels
         (list (channel
                (name 'nonguix)
                (url "https://gitlab.com/nonguix/nonguix")
                (commit "46c1d8bcca674d3a71cd077c52dde9552a89873d"))
               (channel
                (name 'guix)
                (url "https://git.savannah.gnu.org/git/guix.git")
                (commit "f463f376e91ccc1fe4ab68d5e822b5d71a1234f5"))))
        (inferior
         (inferior-for-channels channels)))
     (first (lookup-inferior-packages inferior "linux" "5.12.8"))))
  ;; (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (locale "en_US.utf8")
  (timezone "Europe/Moscow")
  (keyboard-layout (keyboard-layout "us,ru" #:options '("grp:alt_shift_toggle")))
  (users (cons* (user-account
                 (name "pavel")
                 (comment "Pavel")
                 (group "users")
                 (home-directory "/home/pavel")
                 (supplementary-groups
                  '("wheel"  ;; sudo
                    "netdev" ;; network devices
                    "audio"
                    "video"
                    "input"
                    "tty"
                    ;; "docker"
                    "lp")))
                %base-user-accounts))
  
  (packages
   (append
    (list nss-certs
  	    git
          i3-gaps
          openbox
          xterm
  	    vim)
    %base-packages))
 (host-name "blue")

 (bootloader
  (bootloader-configuration
   (bootloader grub-bootloader)
   (target "/dev/sda")
   (keyboard-layout keyboard-layout)))

 (swap-devices
  (list (uuid "d9ca4f8b-4bb1-420e-9371-3558731bada1")))

 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device
           (uuid "179fbd75-3c7f-4de2-8c4f-4c30939b8a3f"
                 'ext4))
          (type "ext4"))
         %base-file-systems)))
;; blue:1 ends here
