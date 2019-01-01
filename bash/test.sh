testme() {
    output=`python <<END

import os
DEFAULT_FILE = os.path.expanduser('~/.default_env')
print(DEFAULT_FILE)
END`
    echo $output
}
