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

  overrides = self: super: { 
    # Comment this out when building for Android. 
    # https://github.com/reflex-frp/reflex-platform/pull/281#discussion_r181538045
    reflex-dom = pkgs.haskell.lib.addBuildDepend (pkgs.haskell.lib.enableCabalFlag super.reflex-dom "use-warp") self.jsaddle-warp;
  };
})
