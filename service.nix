{ ... }:

# Launch an *user* systemd unit, storing database in Dropbox.
# Port 9000 is hardcoded, with the intention of reserving 90XX 
# for my personal apps.
{
  systemd.user.services.motifapp = 
    let 
      motif = import ./release.nix;
    in {
      description = "Motif app";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = 
        { ExecStart = "${motif}/backend 9000 %h/Dropbox/Apps/motifdb";
          WorkingDirectory = "${motif}";
        };
    };
}

