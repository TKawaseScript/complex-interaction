# patch_sfanalysis.py  (Python 2.7)
# Purpose: patch sfanalysis.py in-place
# - add 'overwrite=False' to analyze_degree_sequences(...)
# - fix chained assignment: analysis.loc[fn]['col']=... -> analysis.loc[fn, 'col']=...

import io
import re
import os
import sys

SF_PATH = os.path.join('.', 'code', 'sfanalysis.py')
BAK_PATH = SF_PATH + '.bak'

def main():
    if not os.path.isfile(SF_PATH):
        sys.stderr.write("ERROR: %s not found.\n" % SF_PATH)
        sys.exit(1)

    # backup
    with io.open(SF_PATH, 'r', encoding='utf-8', errors='ignore') as f:
        s = f.read()
    with io.open(BAK_PATH, 'w', encoding='utf-8') as f:
        f.write(s)

    # add overwrite arg if missing
    s2 = re.sub(r"def\s+analyze_degree_sequences\(([^)]*)\):",
                r"def analyze_degree_sequences(\1, overwrite=False):",
                s)

    # fix chained assignment
    s2 = re.sub(r"analysis\.loc\[(.*?)\]\['([^']+)'\]\s*=",
                r"analysis.loc[\1, '\2'] = ",
                s2)

    # write back
    with io.open(SF_PATH, 'w', encoding='utf-8') as f:
        f.write(s2)

    sys.stdout.write("Patched %s (backup -> %s)\n" % (SF_PATH, BAK_PATH))

if __name__ == '__main__':
    main()
