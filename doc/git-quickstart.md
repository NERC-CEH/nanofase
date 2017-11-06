# Git quickstart guide

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

## Branching

[Good tutorial on branches.](https://www.atlassian.com/git/tutorials/using-branches)

To see a list of all *local* branches. The current branch will have an asterisk * next to its name:

```bash
git branch
```

The `-a` and `-r` flags can be used to show all branches (local and remote), or just remote branches, respectively:

```bash
git branch -a
git branch -r
```

To create a new local branch:

```bash
git branch <branch-name>
```

To switch to a branch:

```bash
git checkout <branch-name>
```

If you have unstaged or uncomitted files on the branch you are switching from, Git will *try* to carry these changes over to the branch you are changed to. If it can't do this, it will issue a warning that you must *commit* or *stash* you changes. [See here](https://stackoverflow.com/questions/22053757/checkout-another-branch-when-there-are-uncommitted-changes-on-the-current-branch) for a good description of how this process works.

To push to a remote branch (if it doesn't exist on remote, it will be created and set up to track your local branch):

```bash
git push origin <branch-name>
```

To get a remote branch that doesn't currently exist locally, use the `git checkout` command:

```bash
git checkout <remote-branch-name>
```

Git will automatically create a new local branch and set it to track the remote branch.

To delete a branch:

```bash
git branch -d <branch-name>
```

If your branch has unmerged changes (changes that haven't been merged into another branch), Git won't let you delete the branch. If you wish to force delete a branch, regardless of unmerged changes:

```bash
git branch -D <branch-name>
```