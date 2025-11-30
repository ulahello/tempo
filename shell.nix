{ nixpkgs ? import <nixpkgs> {} }:
nixpkgs.mkShell {
  nativeBuildInputs = with nixpkgs;
    [ alire
      gnat14 gnat14Packages.gprbuild
      gnat14Packages.gnatprove
      just scdoc ];
}
