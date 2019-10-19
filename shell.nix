with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "playground-shell";
  buildInputs = [
    sbcl
    lispPackages.quicklisp
    openssl
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${openssl.out}/lib
  '';
}
