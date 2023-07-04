{
  description = "scsh gui library";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.03";

  outputs = { self, nixpkgs }: {

    packages.x86_64-linux.default =
      # Notice the reference to nixpkgs here.
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name = "scx";
        src = self;
        # builder = "${pkgs.bash}/bin/bash";
        # args = [ ./builder.sh ];
        # gcc = pkgs.gcc;
        # coreutils = pkgs.coreutils;
        # system = builtins.currentSystem;
        buildInputs = [ texinfo Xaw3d pkg-config scheme48 ];
        nativeBuildInputs = [ autoconf gnumake ];
        enableParallelBuilding = true;
        preConfigure = ''
          if test -n "''$`dontStrip-}"; then
            export STRIP=none
          fi
        '';
        configurePhase = "./configure --prefix=/usr/local/ --enable-static=no --with-scsh-includes=${scsh} ";
        buildPhase = "make";
        installPhase = "make install DESTDIR=$out";
      };
  };
}
