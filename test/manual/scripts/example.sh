#!/bin/bash
#
# Usage:
#   example FOO [BAR] [options]
#
# This scripts does nothing but printing its arguments to stderr & stdout
#
# Arguments:
#   FOO   foo argument
#   BAR   optional bar argument (can't be defaulted)
#
# Options:
#   -h --help
#   -f --foo=STRING   foo option
#   -b --bar=INT      optional bar option [default: 14]
PATH=.:$PATH; source docopts.sh --auto "$@"

echo "Continuing with following options: "
for a in ${!ARGS[@]} ; do
  echo "    $a = ${ARGS[$a]}"
done
echo ""

echo "Which is equivalent to:"
echo "    FOO = ${ARGS[FOO]}"
echo "    --help = ${ARGS[--help]}"
echo "    --bar = ${ARGS[--bar]}"
echo "    --foo = ${ARGS[--foo]}"
echo "    BAR = ${ARGS[BAR]}"
