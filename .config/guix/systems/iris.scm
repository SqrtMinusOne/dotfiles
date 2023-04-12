;; [[file:../../../Guix.org::*iris][iris:1]]
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
(use-modules (gnu services docker))
(use-modules (gnu services cups))
(use-modules (gnu services virtualization))
(use-modules (srfi srfi-1))
(use-modules (guix channels))
(use-modules (guix inferior))
(use-modules (nongnu packages linux))
(use-modules (nongnu system linux-initrd))

(use-service-modules desktop networking ssh xorg nix)
(use-package-modules ssh)
(define %my-base-services
  (cons*
   (service openssh-service-type)
   (screen-locker-service i3lock "i3lock")
   (extra-special-file "/lib64/ld-linux-x86-64.so.2" (file-append glibc "/lib/ld-linux-x86-64.so.2"))
   (service nix-service-type)
   (service cups-service-type
            (cups-configuration
             (web-interface? #t)))
   (service docker-service-type)
   (service libvirt-service-type
            (libvirt-configuration
             (unix-sock-group "libvirt")
             (tls-port "16555")))
   (service virtlog-service-type)
   (bluetooth-service #:auto-enable? #f)
   (modify-services %desktop-services
                    (network-manager-service-type
                     config =>
                     (network-manager-configuration
                      (inherit config)
                      (vpn-plugins (list network-manager-openvpn))))
                    (guix-service-type
                     config =>
                     (guix-configuration
                      (inherit config)
                      (substitute-urls
                       (append (list "https://substitutes.nonguix.org")
                               %default-substitute-urls))
                      (authorized-keys
                       (append (list (local-file "./signing-key.pub"))
                               %default-authorized-guix-keys)))))))

(operating-system
 (kernel
   (let*
       ((channels
         (list (channel
                (name 'nonguix)
                (url "https://gitlab.com/nonguix/nonguix")
                (commit "213be7ee6676fc4a3db0e3ac9ce5cd79e2ed209e"))
               (channel
                (name 'guix)
                (url "https://git.savannah.gnu.org/git/guix.git")
                (commit "6311493d7a6271bfbc51f4693857f9a12fe9965d"))))
        (inferior
         (inferior-for-channels channels)))
     (first (lookup-inferior-packages inferior "linux" "6.2.9"))))
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
                   "docker"
                   "scanner"
                   "libvirt"
                   "lp")))
               %base-user-accounts))
 
 (packages
  (append
   (list nss-certs
 	    git
         i3-gaps
         i3lock
         openbox
         xterm
 	    vim)
   %base-packages))

 (host-name "iris")
 (services (cons*
            (set-xorg-configuration
             (xorg-configuration
              (keyboard-layout keyboard-layout)))
            %my-base-services))

 (bootloader (bootloader-configuration
              (bootloader grub-bootloader)
              (targets (list "/dev/sdb"))
              (keyboard-layout keyboard-layout)))
 (swap-devices (list (swap-space
                      (target (uuid
                               "bc284384-ff00-4fbc-abda-1c46f69c0505")))))
 (mapped-devices (list (mapped-device
                        (source (uuid
                                 "21876acb-e05a-4731-8df0-ba5761910ca8"))
                        (target "cryptroot")
                        (type luks-device-mapping))))

 (file-systems (cons* (file-system
                       (mount-point "/")
                       (device "/dev/mapper/cryptroot")
                       (type "ext4")
                       (dependencies mapped-devices))
                      (file-system
                       (mount-point "/boot/efi")
                       (device (uuid "782E-F6D3"
                                     'fat32))
                       (type "vfat")) %base-file-systems)))
;; iris:1 ends here
