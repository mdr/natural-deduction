{ pkgs ? (import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/b83e7f5a04a3acc8e92228b0c4bae68933d504eb.tar.gz") { }) }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    sbt
    jdk17_headless
    nixfmt
    # node 16.15.0
    (with (import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/bfc6088781a266de9453acfdc40c70f74971d51c.tar.gz") { }); nodejs)
  ];
}
