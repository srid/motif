{ ... }:

# Launch an user systemd unit, storing database in Dropbox.
# Port 9001 is hardcoded, with the intention of reserving 90XX 
# for my apps.
{
  systemd.user.services.motif = 
    let 
      motif = import ./default.nix { };
      backend = motif.ghc.backend;
      frontend = motif.ghcjs.frontend;
    in {
      description = "Motif app";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = 
        { ExecStart = "${backend}/bin/backend 9001 %h/Dropbox/Apps/motifdb";
          WorkingDirectory = "${frontend}/bin/frontend.jsexe";
        };
    };
}

