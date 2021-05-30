;; [[file:../../../Guix.org::*blue][blue:1]]
(use-modules (gnu))
(use-modules (gnu system nss))
(use-modules (gnu packages certs))
(use-modules (gnu packages version-control))
(use-modules (gnu packages vim))
(use-modules (gnu packages wm))
(use-modules (gnu packages openbox))
(use-modules (nongnu packages linux))
(use-modules (nongnu system linux-initrd))

(use-service-modules desktop networking ssh xorg)
(use-package-modules ssh)

(operating-system
  ;; Use the full Linux kernel
  (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (locale "en_US.utf8")
  (timezone "Europe/Moscow")
  
  ;; US/RU keyboard layout
  (keyboard-layout (keyboard-layout "us,ru" #:options '("grp:alt_shift_toggle")))
  
  ;; User accounts
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
  
  ;; Base packages
  (packages
   (append
    (list nss-certs
  	    git
          i3-gaps
          openbox
  	    vim)
    %base-packages))
  
  ;; Services
  (services
   (append
    (list (service openssh-service-type)
          (set-xorg-configuration
           (xorg-configuration
            (keyboard-layout keyboard-layout))))
    %desktop-services))
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
