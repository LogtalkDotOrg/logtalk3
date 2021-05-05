# bazel build --verbose_failures --action_env=LOGTALKHOME=$LOGTALKHOME --action_env=LOGTALKUSER=$LOGTALKUSER --spawn_strategy=standalone --genrule_strategy=standalone //:ackerman

# Compiling a Logtalk application into a Prolog file
def lgt_to_pl(name, src, options=[]):
  srcs = [
  ]
  if not src in srcs:
    srcs.append(src)
  native.genrule(
    name = name,
    srcs = srcs,
    cmd = " && ".join([
      'ROOT=$$PWD',
      'TMP=$$(mktemp -d || mktemp -d -t bazel-tmp)',
      'mkdir -p $$TMP/collect',
      'echo "go :- '
        + "cd('$$ROOT'),"
        + 'logtalk_load('
        + "'$<',"
        + "[scratch_directory('$$TMP/collect')]"
        + ').'
        + '">$$TMP/go.pl',
      'swilgt -g "go" -t "halt" $$TMP/go.pl',
      'rm -f $$TMP/collect/*loader*.pl',
      'cat $$(ls -t $$TMP/collect/*.pl) > $(@D)/' + name + '.pl',
      'rm -rf $$TMP/collect'
    ]),
    outs = [name + ".pl"],
  )
