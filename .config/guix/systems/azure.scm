;; [[file:../../../Guix.org::*azure][azure:1]]
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

(define %backlight-udev-rule
  (udev-rule
   "90-backlight.rules"
   (string-append "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\""
                  "\n"
                  "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))

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

 (host-name "azure")
 (services (cons*
            (set-xorg-configuration
             (xorg-configuration
              (keyboard-layout keyboard-layout)))
            (modify-services %my-base-services
                             (elogind-service-type config =>
                                                   (elogind-configuration (inherit config)
                                                                          (handle-lid-switch-external-power 'suspend)))
                             (udev-service-type config =>
                                                (udev-configuration (inherit config)
                                                                    (rules (cons %backlight-udev-rule
                                                                                 (udev-configuration-rules config))))))))

 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (target "/boot/efi")
   (keyboard-layout keyboard-layout)))

 (swap-devices
  (list (uuid "4b2dedb3-b111-4e69-8c05-6daa2b072c76")))

 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device (file-system-label "my-root"))
          (type "ext4"))
	     (file-system
	      (mount-point "/boot/efi")
	      (device "/dev/sda1")
	      (type "vfat"))
         %base-file-systems)))
;; azure:1 ends here
