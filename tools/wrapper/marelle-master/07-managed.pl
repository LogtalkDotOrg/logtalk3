%
%  07-managed.pl
%  marelle
%
%  Shorthand for completely managed packages.
%

:- multifile managed_pkg/1.

pkg(P) :- managed_pkg(P).

installs_with_apt(P) :- managed_pkg(P).
installs_with_brew(P) :- managed_pkg(P).
installs_with_brew_cask(P) :- managed_pkg(P).
installs_with_pacman(P) :- managed_pkg(P).
installs_with_pkgng(P) :- managed_pkg(P).
installs_with_ports(P) :- managed_pkg(P).
