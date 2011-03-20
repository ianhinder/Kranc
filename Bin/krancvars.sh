
export KRANCDIR=$(ls -d $(dirname ${BASH_ARGV[0]})/..)
if ! expr "$KRANCDIR">/dev/null : '/'; then
    # Relative path
    KRANCDIR="$PWD/$KRANCDIR"
fi

export PATH="$KRANCDIR/Bin:$PATH"
