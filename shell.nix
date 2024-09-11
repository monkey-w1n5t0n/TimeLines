{
  pkgs ? import <nixpkgs> { },
}:

let
  # Define the JDK version you want to use
  jdk = pkgs.jdk17;

  # Create a custom leiningen wrapper with the JDK we want
  customLein = pkgs.leiningen.override { jdk = jdk; };

in
pkgs.mkShell {
  buildInputs = with pkgs; [
    customLein
    clojure
    jdk
    glfw
    libGL
    xorg.libX11
    xorg.libXxf86vm
    xorg.libXcursor
    xorg.libXrandr
    xorg.libXi
    libpulseaudio
  ];

  shellHook = ''
    export LD_LIBRARY_PATH=${
      pkgs.lib.makeLibraryPath [
        pkgs.libGL
        pkgs.libpulseaudio
        pkgs.glfw
      ]
    }:$LD_LIBRARY_PATH
  '';
}
