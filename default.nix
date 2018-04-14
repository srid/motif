{}:

(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
  };

  android.frontend = {
    executableName = "frontend";
    applicationId = "org.example.frontend";
    displayName = "Example Android App";
  };

  ios.frontend = {
    executableName = "frontend";
    bundleIdentifier = "org.example.frontend";
    bundleName = "Example iOS App";
  };

  shells = {
    ghc = ["common" "backend" "frontend"];
    ghcjs = ["common" "frontend"];
  };

  overrides = self: super: let 
    # servant >= 0.12
    # Not using v0.13 as they require latest http-types which jsaddle doesn't work with.
    servant= pkgs.fetchFromGitHub {
      owner = "haskell-servant";
      repo = "servant";
      rev = "7e9910b27ee42c635bb71f87bbdaa6056ab22c23";
      sha256 = "0j2j6x8wdc3jzpixaq4wrpb4873bj4jvqmfbhcybdqx8cl8v36yp";
    }; 

    servant-reflex = pkgs.fetchFromGitHub {
      owner = "imalsogreg";
      repo = "servant-reflex";
      rev = "115da63dbfac37c1fd4f5e652466778ac461e148";
      sha256 = "1r829j3qy7vwcng7xpwfp2w5605i43w5x8g5skgd1iz7a5mfmq5i";
    };

    skipTest = pkgs.haskell.lib.dontCheck;
    in 
    {
      # Comment this out when building for Android. 
      # https://github.com/reflex-frp/reflex-platform/pull/281#discussion_r181538045
      reflex-dom = pkgs.haskell.lib.addBuildDepend (pkgs.haskell.lib.enableCabalFlag super.reflex-dom "use-warp") self.jsaddle-warp;

      servant = skipTest (self.callCabal2nix "servant" "${servant}/servant" {});
      servant-server = self.callCabal2nix "servant-server" "${servant}/servant-server" {};
      servant-client = self.callCabal2nix "servant-client" "${servant}/servant-client" {};
      servant-client-core = self.callCabal2nix "servant-client-core" "${servant}/servant-client-core" {};
      servant-client-ghcjs = self.callCabal2nix "servant-client-ghcjs" "${servant}/servant-client-ghcjs" {};

      servant-reflex = self.callCabal2nix "servant-reflex" "${servant-reflex}" {};
    };
})
