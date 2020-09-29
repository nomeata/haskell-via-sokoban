#!/usr/bin/env perl
use File::Slurp;

while (<>) {
    s{LISTING\(([^)]*)\)}{
      "```haskell\n" . read_file("$1") . "```\n"
    }gxe;
    s{EDIT\(([^)]*)\)}{
      "https://code.world/haskell#" . (read_file("$1.hash") =~ s/\n$//r)
    }gxe;
    s{RUN\(([^)]*)\)}{
      '<iframe width="400" height="400" src="https://code.world/run.html?mode=haskell&amp;dhash=' . (read_file("$1.dhash") =~ s/\n$//r) . '"></iframe>'
    }gxe;
    print;
}
