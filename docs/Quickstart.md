# Quick start guide

[Getting started with Git guide.](http://rogerdudler.github.io/git-guide/)

To clone the repository into a new folder "nanofase":

```bash
git clone --recursive https://github.com/nerc-ceh/nanofase
```

The `--recursive` is important as it pulls in code from "submodules" located in the vendor folder. You can specify a different folder name after the url, e.g., `git clone --recursive https://github.com/nerc-ceh/nanofase custom_folder`.

To pull changes from Github:

```bash
git pull
```

After you've made changes, add the changes to the "staging" area and commit them:

```bash
git add --a
git commit -m "Commit message."
```
The `--a` option signifies that *all* changes should be staged. The commit needs a commit message, which is specified after the `-m` option.

These changes are now commited locally. To push to the Github repository:

```bash
git push origin master
```

This pushes the current branch of your local repo to the "master" branch of the remote repo with name "origin", which if you've cloned the repo as above, will be https://github.com/nerc-ceh/nanofase.