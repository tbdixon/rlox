./target/release/main fib.lox &
pid=$!
dtrace -x ustackframes=5000 -n "profile-1000 /pid == $pid/ { @[ustack()] = count(); } tick-10s { exit(0); }"  -o out.user_stacks
cat out.user_stacks | inferno-collapse-dtrace > stacks.folded
cat stacks.folded | inferno-flamegraph > flamegraph.svg
