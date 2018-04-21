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
# Setup: Encountered missing dependencies:
# aeson >=1.2.3.0 && <1.4,
# attoparsec >=0.13.2.0 && <0.14,
# base-compat >=0.9.3 && <0.11,
# http-media >=0.7.0 && <0.8,
# http-types ==0.12.*,
# mmorph >=1.1.0 && <1.2,
# text >=1.2.3.0 && <1.3
# and 
# Configuring jsaddle-0.9.4.0...
# Setup: Encountered missing dependencies:
# http-types >=0.8.6 && <0.12
    servant= pkgs.fetchFromGitHub {
      owner = "haskell-servant";
      repo = "servant";
      rev = "0f0c8f7f900670a64336d7ec1cb98c45cea90c52";
      sha256 = "19pivi2a943cjzmvi8ipbj3i42vs382g2fq1y08xa64j4w86sxni";
      #rev = "7e9910b27ee42c635bb71f87bbdaa6056ab22c23";
      #sha256 = "0j2j6x8wdc3jzpixaq4wrpb4873bj4jvqmfbhcybdqx8cl8v36yp";
    }; 
    http-types = pkgs.fetchFromGitHub {
      owner = "aristidb";
      repo = "http-types";
      rev = "f392b7a59774663176374a423037e2f06ba3b30d";
      sha256 = "0br1wn8sgf03qf35g4zl32bx4k03cqqbv9wf789ab3pxcl0cm1ix";
    };
    jsaddle = pkgs.fetchFromGitHub {
      owner = "srid";
      repo = "jsaddle";
      rev = "d7a5c5cf4d4a23b0529cf4d9160164841d036000";
      sha256 = "0qaa4zka8xwbpc743dga65np0f5kr5wkq1s4szrylb2ghj6f58f0";
    };
    aeson = pkgs.fetchFromGitHub {
      owner = "bos";
      repo = "aeson";
      rev = "8e58f82db806424ea3690ed1637375f2aadc7940";
      sha256 = "0i62cx7i26zhr93cbajkv4qh3d7f00wxp06j6wy67xsmrf44m1dm";
    };
    attoparsec = pkgs.fetchFromGitHub {
      owner = "bos";
      repo = "attoparsec";
      rev = "c8030ed56df344b4c7238e58c5349e64f70c7bc9";
      sha256 = "1v94nwkm050vwhsv55rvw0d8syvivx7f6b6wz04lss3pfcrj4qwl";
    };
    base-compat = pkgs.fetchFromGitHub {
      owner = "haskell-compat";
      repo = "base-compat";
      rev = "877917365da629da7f76afa35ff99524504603dd";
      sha256 = "1nski1fg9ba9xadr57656lj1w01lq95ljcf6m6zq4mm3qdm0x3nh";
    };
    http-media = pkgs.fetchFromGitHub {
      owner = "zmthy";
      repo = "http-media";
      rev = "501f8c0b90fe2f1675f7d19f32bc7c5b4a67c7bb";
      sha256 = "1rjxk0r09xh2m2f8wyzhhvmmdfmlbmn7s126vgkdxkqanarf0m46";
    };
    mmorph = pkgs.fetchFromGitHub {
      owner = "Gabriel439";
      repo = "Haskell-MMorph-Library";
      rev = "c557fd52358d15c395c33b63a2e7e318160d735c";
      sha256 = "0a96q893zzj8zsq815qzmk341ykjdk7qh8rpp541hj40f53k55ir";
    };
    text = pkgs.fetchFromGitHub {
      owner = "haskell";
      repo = "text";
      rev = "a02c2dafafa425bd5f36c8629e98b98daf1cfa1e";
      sha256 = "0rh9mb023f0s56ylzxz9c3c1y09lpl6m69ap5bnpdi0dz7fm6s85";
    };

    servant-reflex = pkgs.fetchFromGitHub {
      owner = "imalsogreg";
      repo = "servant-reflex";
      rev = "115da63dbfac37c1fd4f5e652466778ac461e148";
      sha256 = "1r829j3qy7vwcng7xpwfp2w5605i43w5x8g5skgd1iz7a5mfmq5i";
    };

    reflex-servant = pkgs.fetchFromGitHub {
      owner = "Compositional";
      #owner = "srid";
      repo = "reflex-servant";
      #rev = "bec3a37ba037f35f51e39d4fdfc2855139f7c917";
      #sha256 = "1fc3i6kygzlbrvlssnhkald2jfj3nj6024mbdzv6gmv9y8rbxbal";
      rev = "a58a50d8ea418719f6366a296906838503311d76";
      sha256 = "0q23zcmm6yffm2z5mqlkg22d8av1a2wfm9w5yyhj4pbxl0gpkvfk";
    };

    semantic-reflex = pkgs.fetchFromGitHub {
      owner = "tomsmalley";
      repo = "semantic-reflex";
      rev = "b6075f6b052de5071b16c9d29526dda44ea4c092";
      sha256 = "1scwwcvj0ycd6zd5i01d1w03c4xifrrs5ywwyylp38fqz62zm21g";
    };

    skipTest = pkgs.haskell.lib.dontCheck;
    in 
    {
      # Comment this out when building for Android. 
      # https://github.com/reflex-frp/reflex-platform/pull/281#discussion_r181538045
      reflex-dom = pkgs.haskell.lib.addBuildDepend (pkgs.haskell.lib.enableCabalFlag super.reflex-dom "use-warp") self.jsaddle-warp;

      servant = skipTest (self.callCabal2nix "servant" "${servant}/servant" {});
      servant-server = skipTest (self.callCabal2nix "servant-server" "${servant}/servant-server" {});
      servant-client = skipTest (self.callCabal2nix "servant-client" "${servant}/servant-client" {});
      servant-client-core = skipTest (self.callCabal2nix "servant-client-core" "${servant}/servant-client-core" {});
      servant-client-ghcjs = skipTest (self.callCabal2nix "servant-client-ghcjs" "${servant}/servant-client-ghcjs" {});
      http-types = skipTest (self.callCabal2nix "http-types" "${http-types}" {});
      jsaddle = skipTest (self.callCabal2nix "jsaddle" "${jsaddle}/jsaddle" {});
      jsaddle-warp = skipTest (self.callCabal2nix "jsaddle-warp" "${jsaddle}/jsaddle-warp" {});
      base-compat = skipTest (self.callCabal2nix "base-compat" "${base-compat}" {});
      aeson = skipTest (self.callCabal2nix "aeson" "${aeson}" {});
      attoparsec = skipTest (self.callCabal2nix "attoparsec" "${attoparsec}" {});
      http-media = skipTest (self.callCabal2nix "http-media" "${http-media}" {});
      text = self.callCabal2nix "text" "${text}" {};
      mmorph = self.callCabal2nix "mmorph" "${mmorph}" {};
      # servant-reflex = self.callCabal2nix "servant-reflex" "${servant-reflex}" {};
      reflex-servant = skipTest (self.callCabal2nix "reflex-servant" "${reflex-servant}" {});

      semantic-reflex = self.callCabal2nix "semantic-reflex" "${semantic-reflex}/semantic-reflex" {};
    };
})
