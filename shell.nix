{ nixpkgs ? import <nixpkgs> {} }:
nixpkgs.mkShell {
  nativeBuildInputs = with nixpkgs; [ gnat gprbuild alire ];
}
