Your task is to upgrade all the libraries to the latest version:

1. run @dev/gradle-upgrade.sh to figure out which libraries can be upgraded
2. then, figure out all the libraries that has a newer release versions (not SNAPSHOT, dev, alpha or beta), and upgrade
   them in build files.
3. run @dev/gradle-upgrade.sh again to verify the change
4finally, run `./dev/make-all.sh` to make sure that compilation can pass