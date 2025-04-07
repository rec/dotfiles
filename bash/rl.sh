# rl: Run a bash command, only print output if it fails
#
# Example: `rl build` prints nothing if the build succeeds, but the whole
# log if it fails
#
# The bash function `rl` runs its arguments, but redirects both
# stdout and stderr to a temporary output file, `/tmp/$USER/$$.out.txt`,
# where `$$` is a bash expression for the current process ID.
#
# If the command succeeds, there is no output; if it fails, `rl` echoes
# the contents of that output file to the terminal.
#
# Note that `rl` does not delete the output file, so you can look at it
# afterward even if the job succeeds - until it is overwritten by another
# `rl` job running on that same process.
#
# The set of possible process IDs is quite large, so you should periodically
# clean out `/tmp/$USER` if your machine doesn't get rebooted often.

rl() {
    tmp=/tmp/$USER
    procid=$$
    out=$tmp/$procid.out.txt

    if ! mkdir -p tmp ; then
        echo "Unable to mkdir -p $tmp"
        return 1
    fi

    if $@ > $out 2>&1 ; then
        return 0
    else
        cat $out
        return 1
    fi
}
