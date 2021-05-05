%
%  sudo.pl
%  marelle
%

% sudo_tell/1.
%   Like tell/1, but works on files which you need to sudo to get privileges
%   for.
sudo_tell(Filename) :-
    expand_path(Filename, ExpFilename),
    join(['cat >', ExpFilename], BashCmd),
    process_create(
        '/usr/bin/sudo',
        ['bash', '-c', BashCmd],
        [stdin(pipe(Stream))]
    ),
    tell(Stream).
