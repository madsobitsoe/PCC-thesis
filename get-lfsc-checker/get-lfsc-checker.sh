#!/usr/bin/env bash

# utility function to download a file
function download {
  if [ -x "$(command -v wget)" ]; then
    wget -c -O "$2" "$1"
  elif [ -x "$(command -v curl)" ]; then
    curl -L "$1" >"$2"
  else
    echo "Can't figure out how to download from web.  Please install wget or curl." >&2
    exit 1
  fi
}

CVC_DIR=$(dirname $(dirname "$0"))
mkdir -p $CVC_DIR/deps
pushd $CVC_DIR/deps

BASE_DIR=`pwd`
mkdir -p $BASE_DIR/tmp/

##### LFSC
LFSC_DIR="$BASE_DIR/lfsc-checker"
mkdir -p $LFSC_DIR

# download and unpack LFSC
version="5f44ffb1241ca81dbb3118807546ba14ab9ea7a5"
download "https://github.com/cvc5/LFSC/archive/$version.tar.gz" $BASE_DIR/tmp/lfsc.tgz
tar --strip 1 -xzf $BASE_DIR/tmp/lfsc.tgz -C $LFSC_DIR

# build and install LFSC
pushd $LFSC_DIR
mkdir -p build && cd build
cmake -DCMAKE_INSTALL_PREFIX="$BASE_DIR" ..
make install
popd

##### signatures

# The LFSC signatures live in the main cvc5 repository
SIG_DIR="$BASE_DIR/../proofs/lfsc/signatures"

# install signatures and scripts
mkdir -p $BASE_DIR/share/lfsc
cp -r $SIG_DIR $BASE_DIR/share/lfsc

# based on https://github.com/CVC4/signatures/blob/master/lfsc/new/scripts/cvc4_gen_and_check.sh
cat << EOF > $BASE_DIR/bin/cvc5-lfsc-check.sh
#!/bin/bash

echo "=== Generate proof: \$@"
$BASE_DIR/bin/cvc5-proof.sh \$@ > proof.plf

echo "=== Check proof with LFSC"
$BASE_DIR/bin/lfsc-check.sh proof.plf
EOF
chmod +x $BASE_DIR/bin/cvc5-lfsc-check.sh

# based on https://github.com/CVC4/signatures/blob/master/lfsc/new/scripts/cvc4_gen.sh
cat << EOF > $BASE_DIR/bin/cvc5-proof.sh
#!/bin/bash

# call cvc5 and remove the first line of the output (should be "unsat")
\$@ --dump-proofs --proof-format=lfsc | tail -n +2
EOF
chmod +x $BASE_DIR/bin/cvc5-proof.sh

# based on https://github.com/CVC4/signatures/blob/master/lfsc/new/scripts/lfsc_check.sh
cat << EOF > $BASE_DIR/bin/lfsc-check.sh
#!/bin/bash

cat \$@ | grep WARNING
CHECK=\$(cat \$@ | grep check)
[ -z "\$CHECK" ] && echo "; WARNING: Empty proof!!!"

SIG_DIR=$BASE_DIR/share/lfsc/signatures
SIGS="\$SIG_DIR/core_defs.plf \\
    \$SIG_DIR/util_defs.plf \\
    \$SIG_DIR/theory_def.plf \\
    \$SIG_DIR/nary_programs.plf \\
    \$SIG_DIR/boolean_programs.plf \\
    \$SIG_DIR/boolean_rules.plf \\
    \$SIG_DIR/cnf_rules.plf \\
    \$SIG_DIR/equality_rules.plf \\
    \$SIG_DIR/arith_programs.plf \\
    \$SIG_DIR/arith_rules.plf \\
    \$SIG_DIR/strings_programs.plf \\
    \$SIG_DIR/strings_rules.plf \\
    \$SIG_DIR/quantifiers_rules.plf"
$BASE_DIR/bin/lfscc \$SIGS \$@ >& lfsc.out

# recover macros for applications of arity 1,2,3, and simplify builtin syntax for constants
#sed -i.orig 's/(f_ite [^ \)]*)/f_ite/g' lfsc.out
sed -i.orig 's/(\\ [^ ]* (\\ [^ ]* (\\ [^ ]* (apply (apply (apply f_\([^ ]*\) [^ ]*) [^ ]*) [^ ]*))))/\1/g; s/(\\ [^ ]* (\\ [^ ]* (apply (apply f_\([^ ]*\) [^ ]*) [^ ]*)))/\1/g; s/(\\ [^ ]* (apply f_\([^ ]*\) [^ ]*))/\1/g; s/(var \([^ ]*\) [^ \)]*)/var_\1/g; s/(int \([^ \)]*\))/\1/g; s/emptystr/""/g; s/int\.//g' lfsc.out

cat lfsc.out
rm lfsc.out
EOF
chmod +x $BASE_DIR/bin/lfsc-check.sh

popd

echo ""
echo "========== How to use LFSC =========="
echo "Generate a LFSC proof with cvc5:"
echo "  $CVC_DIR/deps/bin/cvc5-proof.sh cvc5 <options> <input file>"
echo "Check a generated proof:"
echo "  $CVC_DIR/deps/bin/lfsc-check.sh <proof file>"
echo "Run cvc5 and check the generated proof:"
echo "  $CVC_DIR/deps/bin/cvc5-lfsc-check.sh cvc5 <options> <input file>"
