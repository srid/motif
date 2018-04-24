# motif

[chronicle](https://github.com/srid/chronicle) resurrected.

## Development

```
./ghcid-frontend  # Runs jsaddle-warp
```

```
./ghcid-backend
```

Visit http://localhost:3003/

## Deployment on NixOS

Clone this repo and add the full path to the `service.nix` file to the imports section of your /etc/nixos/configuration.nix (you may alternatively use [home-manager](https://github.com/rycee/home-manager) should you wish to keep it all user-local).

Run `sudo nixos-rebuild switch`. This will add an *user* systemd unit called `motifapp`. You will need to launch it manually using:

```
systemctl --user start motifapp
```

Then visit http://localhost:9001/index.html in the browser.

Note that the server is hardcoded to use `~/Dropbox/Apps/motifdb` as the location of the (acid-state) database.

## Roadmap

See https://github.com/srid/motif/projects/1
