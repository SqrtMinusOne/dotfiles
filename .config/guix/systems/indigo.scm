;; [[file:../../../Guix.org::*indigo][indigo:1]]
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
   (modify-services %desktop-services
                    (network-manager-service-type
                     config =>
                     (network-manager-configuration
                      (inherit config)
                      (vpn-plugins (list network-manager-openvpn)))))))


(operating-system
 (kernel
  (let*
      ((channels
        (list (channel
               (name 'nonguix)
               (url "https://gitlab.com/nonguix/nonguix")
               (commit "d3c5eea0cbfe3e5bfbcf1fe15bc916fefacc624f"))
              (channel
               (name 'guix)
               (url "https://git.savannah.gnu.org/git/guix.git")
               (commit "cf88c967afbf15c58efb0ba37d6638f1be9a0481"))))
       (inferior
        (inferior-for-channels channels)))
    (first (lookup-inferior-packages inferior "linux" "5.12.9"))))
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

 (host-name "indigo")
 (services (cons*
            (set-xorg-configuration
             (xorg-configuration
              (keyboard-layout keyboard-layout)))
            %my-base-services))

 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (target "/boot/efi")
   (keyboard-layout keyboard-layout)))

 (swap-devices
  (list (uuid "059a2c26-8f70-4986-adf0-1a2e7b511404")))

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
;; indigo:1 ends here
