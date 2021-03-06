./target/release/main fib.lox &
pid=$!
dtrace -x ustackframes=100 -n "profile-200 /pid == $pid/ { @[ustack()] = count(); } tick-60s { exit(0); }"  -o out.user_stacks
cat out.user_stacks | inferno-collapse-dtrace > stacks.folded
cat stacks.folded | inferno-flamegraph > flamegraph.svg
