USAGE: ecukes [--script|--win] [file|dir] [options]

OPTIONS:
 -h, --help             Display this help message
 --script               Run Ecukes as a script/batch job
 --win                  Run Ecukes with full GUI window
 --new                  Create new Ecukes setup for project
 --list-steps [DOC]     Print all available steps defined for this project.
                        Call it like `--list-steps t' to include step docstring.
 --verbose              Show `message' output
 --dbg                  Run in debug mode (enable as much debug options in Emacs as possible)
 --tags TAG_EXPRESSION  Only execute the scenarios with tags matching TAG_EXPRESSION.
                        TAG_EXPRESSION Examples: --tags @dev, --tags @dev,~@local
                        A tag starting with ~ excluded from the scenarios.
