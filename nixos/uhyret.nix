# Import this from /etc/nixos/configuration.nix with e.g.
#
# imports =
#   [ ./hardware-configuration.nix
#     /home/athas/.config/nixos/uhyret.nix
#   ];

{ config, pkgs, ... }:

{
  boot.kernelPackages = pkgs.linuxPackages_5_4;

  boot.kernelPatches = [ {
    name = "make-rocm-work";
    patch = null;
    extraConfig = ''
                ZONE_DEVICE y
                HMM_MIRROR y
                DRM_AMDGPU_USERPTR y
                '';
  } ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.useOSProber = true;

  # Give me the good manpages as well.
  documentation.dev.enable = true;

  networking.hostName = "uhyret";
  # networking.nameservers = [ "192.168.1.1" ]; # Local router.

  # Select internationalisation properties.
  i18n = {
    consoleFont = "ter-i32b";
    consolePackages = with pkgs; [ terminus_font ];
    consoleUseXkbConfig = true;
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Amsterdam";

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.rocmTargets = ["gfx900"]; # Vega 64

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget curl git glxinfo emacs file htop tree coreutils-full autossh
    killall pass zip unzip nmap sshfs ranger neofetch xdg_utils pstree
    texlive.combined.scheme-full groff imagemagick ott graphviz
    evince firefox mplayer gimp inkscape gnupg feh imv
    pandoc
    sway rxvt_unicode xterm dmenu bemenu xwayland alacritty wl-clipboard grim
    numix-cursor-theme xorg.xcursorthemes hicolor_icon_theme
    xorg.xev xdotool
    pavucontrol
    transmission-gtk
    libreoffice
    ispell aspell aspellDicts.en aspellDicts.en-computers aspellDicts.da
    mime-types shared-mime-info
    lm_sensors
    cloc rsync cowsay figlet bc
    dosbox
    whois
    groff
    exif
    rlwrap

    memtest86plus

    # Hacking stuff
    gcc gdb clang cmake gnumake stack hlint cabal-install ghc
    zlib zlib.dev binutils # futhark
    automake autoconf pkg-config libtool
    nix-diff nix-prefetch-git
    valgrind tmux oclgrind
    mono powershell
    gforth
    mosml
    manpages
    ispc
    go
    smlnj
    idris

    libGL_driver opencl-info
    lynx

    python3 python3Packages.pip python3Packages.setuptools

    vgo2nix

    rocm-opencl-runtime rocminfo

    # Proprietary
    steam
    steam-run
  ];

  fonts.fonts = with pkgs; [
    dejavu_fonts
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    liberation_ttf
    fira-code
    fira-code-symbols
    mplus-outline-fonts
    dina-font
    proggyfonts
    cm_unicode
    corefonts
    libertine
    monoid
    sudo-font
  ];

  nixpkgs.overlays =
    [ (import /home/athas/repos/nixos-rocm)
    ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };
  programs.sway.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Emacs daemon.
  services.emacs.install = true;

  # Suspend when I press the power button.
  services.logind.extraConfig = "HandlePowerKey=suspend";

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  services.cron = {
    enable = true;
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;

  hardware.opengl.enable = true;
  hardware.opengl.extraPackages = [ pkgs.rocm-opencl-icd ];

  # Enable the X11 windowing system.
  services.xserver.enable = false;
  services.xserver.layout = "us";
  services.xserver.xkbVariant = "altgr-intl";
  services.xserver.xkbOptions = "ctrl:nocaps";

  # Desktop environment
  services.xserver.displayManager.gdm.enable = false;
  services.xserver.desktopManager.gnome3.enable = false;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.athas = {
    isNormalUser = true;
    extraGroups = [ "wheel" "video" ]; # Enable ‘sudo’ for the user.
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?

}
