let
  nixpkgs = builtins.fetchTarball {
    url    = "https://github.com/nixos/nixpkgs/archive/2436c27541b2f52deea3a4c1691216a02152e729.tar.gz";
    sha256 = "0p98dwy3rbvdp6np596sfqnwlra11pif3rbdh02pwdyjmdvkmbvd";
  };
  pkgs = import nixpkgs {};
  bashWrapper = with pkgs; writeScript "shell-wrapper" ''
    #! ${stdenv.shell}
    export PS1="\e[0;34m\u@\h \w $ \e[m"
    exec -a bash ${bashInteractive}/bin/bash --noprofile "$@"
  '';
in with pkgs; stdenv.mkDerivation {
  name = "apicl";
  buildInputs = [
    nix cacert curl
    git tmux gnumake
    sbcl
    lispPackages.quicklisp
    openssl
  ];
  shellHook = ''
  export LANG=en_US.UTF-8
    export SHELL="${bashWrapper}"
    export NIX_PATH=nixpkgs=${nixpkgs}
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${openssl.out}/lib
  '';
}
