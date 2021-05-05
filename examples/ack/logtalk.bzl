logtalk_filetype = FileType([".lgt", ".logtalk"])

def logtalk_library_impl(ctx):
  loader = ctx.files.loader
  output = ctx.outputs.out
  flags = ' '.join(ctx.attr.flags)
  ctx.action(
      inputs=loader,
      outputs=output,
    command = " && ".join([
      'ROOT=$$PWD',
      'TMP=$$(mktemp -d || mktemp -d -t bazel-tmp)',
      'mkdir -p $$TMP/collect',
      'echo "go :- '
        + "cd('$$ROOT'),"
        + 'logtalk_load('
        + "'$<',"
        + "[optimize(on), clean(off), scratch_directory('$$TMP/collect')]"
        + ').'
        + '">$$TMP/go.pl',
      'swilgt -g "go" -t "halt" $$TMP/go.pl',
      'rm -f $$TMP/collect/*loader*.pl',
      'cat $$(ls -t $$TMP/collect/*.pl) > $(@D)/' + output.path,
      'rm -rf $$TMP/collect'
    ]),
  )

logtalk_library = rule(
  implementation = logtalk_library_impl,
  attrs = {
    "loader": attr.label(allow_files=logtalk_filetype),
    "deps": attr.label_list(allow_files=False),
    "flags": attr.string_list(),
    "backend": attr.string(default = "swilgt"),
  },
  outputs = {"out": "%{name}.pl"},
)
