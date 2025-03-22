#!/usr/bin/env bash
# Script to run all Kani tests using BFA Rust.
#
# Requirement: the https://github.com/model-checking/kani repository must be cloned
#              as a sibling directory to the bfa-ocaml repository.
#
# Behaviour: runs tests, one by one. Stops on the first failure.
#
# Usage: ./kani.sh
#

# Formatting =^^=
PURPLE='\033[0;35m'
RED='\033[0;31m'
ORANGE='\033[0;33m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
CYAN='\033[0;36m'
BLUE='\033[0;34m'
BOLD='\033[1m'
RESET='\033[0m'

function rainbow() {
    n=$(($1 % 7))
    if [ $n -eq 0 ]; then
        echo -e "\033[38;5;197m"
    elif [ $n -eq 1 ]; then
        echo -e "\033[38;5;208m"
    elif [ $n -eq 2 ]; then
        echo -e "\033[38;5;220m"
    elif [ $n -eq 3 ]; then
        echo -e "\033[38;5;70m"
    elif [ $n -eq 4 ]; then
        echo -e "\033[38;5;74m"
    elif [ $n -eq 5 ]; then
        echo -e "\033[38;5;33m"
    elif [ $n -eq 6 ]; then
        echo -e "\033[38;5;127m"
    fi
}


# Tests are in kani/tests/<category...>/<test>.rs
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
KANI_LIB_PATH=$(realpath $SCRIPT_DIR/../kani_lib)
KANI_PATH=$(realpath $SCRIPT_DIR/../../../kani)

# Output files
touch "$SCRIPT_DIR/kani.log"
LOG_FILE=$SCRIPT_DIR/kani.log
SMT_FILE=$SCRIPT_DIR/smt.log
PASS_FILE=$SCRIPT_DIR/passes.log

# Handle arguments:
CMD="bfa-rust exec-main"
STOP_ON_FAIL=true
STORE_PASSES=false
TESTS=$(find $KANI_PATH/tests/kani -name '*.rs' | sort)
while [[ $# -gt 0 ]]; do
    case $1 in
        -d|--directory)
            TESTS=$(find $KANI_PATH/tests/$2 -name '*.rs' | sort)
            shift
            shift
            ;;
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
            echo -e "  $CYAN-d, --directory <path>$RESET     Specify the subdirectory to search for tests, default is kani"
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

# Build Rusteria
eval $(opam env)
dune build
if [ $? -ne 0 ]; then
    echo -e "${RED}${BOLD}Failed to build Rusteria!"
    exit 1
fi

# Build our Kani library
export KANI_LIB_PATH=$KANI_LIB_PATH
if [[ ! $CMD =~ "--no-compile" ]]; then
    (cd $KANI_LIB_PATH/kani && charon --only-cargo --lib --input ./src/)
fi

# Silence warnings
export RUSTFLAGS="-Awarnings"

# If on MacOS, use coreutils:
shopt -s expand_aliases
alias date="gdate"
alias realpath="grealpath"

# Run all tests
step=0
passed=0
failed=0
script_start=$(($(date +%s%N)/1000000))
for test in $TESTS; do
    test_rel_name=$(realpath --relative-to=$KANI_PATH $test)
    expect_failure=$(grep -c "kani-verify-fail" "$test")

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
