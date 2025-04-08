
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


# Build Rusteria
function build_rusteria() {
    eval $(opam env)
    dune build
    if [ $? -ne 0 ]; then
        echo -e "${RED}${BOLD}Failed to build Rusteria!"
        exit 1
    fi
}

# If on MacOS, use coreutils:
shopt -s expand_aliases
alias date="gdate"
alias realpath="grealpath"


SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

# Output files
touch "$SCRIPT_DIR/file.log"
LOG_FILE=$SCRIPT_DIR/file.log
SMT_FILE=$SCRIPT_DIR/smt.log
PASS_FILE=$SCRIPT_DIR/passes.log
