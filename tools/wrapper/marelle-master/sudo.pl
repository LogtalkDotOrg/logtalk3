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
    which('sudo', Sudo),
    process_create(
        Sudo,
        ['/bin/sh', '-c', BashCmd],
        [stdin(pipe(Stream))]
    ),
    tell(Stream).

% sudo_or_empty/1.
%   Returns an empty string when ran as root, path to sudo with
%   a trailing space otherwise.
sudo_or_empty(Command) :-
    ( sh_output('whoami', root) ->
        Command = ''
    ;
        which('sudo', Sudo),
        join([Sudo, ' '], Command)
    ).

sudo_sh(Command0) :-
    sudo_or_empty(Sudo),
    join_if_list(Command0, Command),
    tmp_file_stream(text, File, Stream),
    write(Stream, Command),
    close(Stream),
    sh([Sudo, 'sh "', File, '"']),
    delete_file(File).
