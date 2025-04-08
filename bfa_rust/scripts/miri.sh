#!/usr/bin/env bash
# Script to run all Miri tests using BFA Rust.
#
# Requirement: the https://github.com/rust-lang/miri/ repository must be cloned
#              as a sibling directory to the bfa-ocaml repository.
#
# Behaviour: runs tests, one by one. Stops on the first failure.
#
# Usage: ./miri.sh
#

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
source $SCRIPT_DIR/common.sh

# Tests are in miri/tests/<pass/panic/fail>/<...test>.rs
MIRI_PATH=$(realpath $SCRIPT_DIR/../../../miri)

# Handle arguments:
CMD="bfa-rust exec-main"
STOP_ON_FAIL=true
STORE_PASSES=false

# we exclude some stuff, that is irrelevant or not worth testing:
# - ui.rs (not a test)
# - fail-dep/ and pass-dep/ (avoid downloading dependencies)
# - native-lib/ (don't handle it)
# - many-seeds/ (multithreading)
# - utils/ (not tests)
# - *stacked-borrows*, *stacked_borrows*
# - *concurrency*
TESTS=$(find $MIRI_PATH/tests -name '*.rs' ! -name 'ui.rs' \
  ! -path '*/fail-dep/*' ! -path '*/pass-dep/*' ! -path '*/native-lib/*' \
  ! -path '*/many-seeds/*' ! -path '*/utils/*' ! -path '*/stacked-borrows/*' \
  ! -path '*/concurrency/*' ! -path '*/stacked_borrows/*' \
  | sort)
while [[ $# -gt 0 ]]; do
    case $1 in
        -f|--filter)
            TESTS=$(echo $TESTS | xargs -n1 | grep $2)
            shift
            shift
            ;;
        -e|--exclude)
            TESTS=$(echo $TESTS | xargs -n1 | grep -v $2)
            shift
            shift
            ;;
        --no-compile)
            CMD="$CMD --no-compile"
            shift
            ;;
        --clean)
            CMD="$CMD --clean"
            shift
            ;;
        --smt)
            CMD="$CMD --dump-smt $SMT_FILE"
            shift
            ;;
        --ok)
            STOP_ON_FAIL=false
            shift
            ;;
        --store-passes)
            STORE_PASSES=true
            shift
            ;;
        -v)
            CMD="$CMD -v"
            shift
            ;;
        --help)
            echo -e "${BOLD}Options:$RESET"
            echo -e "  $CYAN-f, --filter <pattern>$RESET     Filter by a given pattern"
            echo -e "  $CYAN-e, --exclude <pattern>$RESET    Exclude by a given pattern"
            echo -e "  $CYAN--no-compile$RESET               Do not re-compile files"
            echo -e "  $CYAN--clean$RESET                    Clean the build directory"
            echo -e "  $CYAN--smt$RESET                      Dump SMT queries to a file"
            echo -e "  $CYAN--store-passes$RESET             Store passed tests in a file"
            echo -e "  $CYAN--ok$RESET                       Ignore errors"
            echo -e "  $CYAN-v$RESET                         Verbose logging"
            exit 1
            ;;
      -*|--*)
            echo "Unknown option $1"
            exit 1
            ;;
      *)
            echo "Unhandled arg $1"
            exit 1
            ;;
    esac
done

# Clean log file
echo -n > $LOG_FILE
if $STORE_PASSES; then
    echo -n > $PASS_FILE
fi

# Run all tests
build_rusteria
step=0
passed=0
failed=0
script_start=$(($(date +%s%N)/1000000))
for test in $TESTS; do
    test_rel_name=$(realpath --relative-to=$MIRI_PATH $test)
    # expect failure if "fail" or "panic" in test_rel_name
    expect_failure=$(grep -c -e "fail" -e "panic" <<< "$test_rel_name")

    echo -en "$(rainbow step)|${RESET} Running $test_rel_name ..."
    echo "Running $test" >> $LOG_FILE
    start=$(($(date +%s%N)/1000000))
    # redirect both stdout and stderr to the log file
    dune exec --no-build -- $CMD $test >> $LOG_FILE 2>>$LOG_FILE
    result=$?
    end=$(($(date +%s%N)/1000000))
    if [ $result -eq $expect_failure ]; then
        if [ $result -eq 0 ]; then
            echo -e " ${GREEN}passed${RESET} in $((end-start))ms"
        else
            echo -e " ${GREEN}failed (expected)${RESET} in $((end-start))ms"
        fi
        passed=$((passed+1))
        if $STORE_PASSES; then
            echo $test >> $PASS_FILE
        fi
    else
        if [ $result -eq 0 ] && [ $expect_failure -eq 1 ]; then
            echo -e " ${RED}passed (expected failure)${RESET}"
            failed=$((failed+1))
        elif [ $result -eq 1 ] && [ $expect_failure -eq 0 ]; then
            echo -e " ${RED}failed${RESET}"
            failed=$((failed+1))
        else
            echo -e " ${PURPLE}crashed: status code $result${RESET}"
        fi
        if $STOP_ON_FAIL; then
            exit 1
        fi
    fi

    echo -e "\n" >> $LOG_FILE
    step=$((step+1))
done

script_end=$(($(date +%s%N)/1000000))
script_duration=$((script_end-script_start))

# | Passed N/N tests in N ms.    Have a nice day 〜
echo -e "$(rainbow step)|${RESET} \
${BOLD}Result: \
${GREEN}$passed${RESET}\
${BOLD}/\
${RED}$failed${RESET}\
${BOLD}/$step tests in ${CYAN}$script_duration ms${RESET}    \
${PURPLE}Have a nice day 〜${RESET}"
