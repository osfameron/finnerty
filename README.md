# Finnerty

Finnerty is a project to auto-generate screencast presentations from `git` commit history.

## Current status

Some code sketches on `git diff` parsing, diff-match-patch bindings etc.

## Talks

* LambdaLounge August 2020 talk on [Parsing `git diff` in PureScript](https://youtu.be/93mDq9tSevQ?t=2261)

## Rationale

When I created a [video tutorial on Haskell](https://www.packtpub.com/gb/application-development/learning-haskell-programming), the process was largely as follows:

1) develop the code I wanted to explain, checking into `git` as I went
2) tidy the git history, rebasing where appropriate to chunk like sections together
3) record myself typing out code, and evaluating/testing it
4) re-record it as there were too many distracting mistakes
5) record voiceover explanation
6) edit the video to speed up or pause typing to match the voiceover

This was time-consuming, repetitive, and error-prone. If typing from memory, the
code might end up having subtly different formatting or variable names. So instead, I would type
the code while having the `git diff` output available in a separate (not recorded) window.

Though I made the [repository](https://github.com/osfameron/words) available for students, I didn't
take full advantage of this as a resource. For example, I could have signposted the videos with
labels showing the commit SHA (or tags).

In some cases I changed the code later. I would then re-record the section where I typed it.
However, subsequent clips, where that code was shown in passing (e.g. while navigating between parts of the codebase)
might show the outdated version of the code.

### Insights

Typing the changes in a `git diff` is akin to being a very inefficient, buggy `patch` program.

The process of editing git history with `rebase` is very aligned with the
didactic process of teaching.

* Sure, to get from A to B, you may have gone down a series of failed paths, but
  is that process confusing rather than illuminating?
* You may want to split a particularly thorny section into its own section
  (commit) to describe it better.
* Did you *cough* test last, but you want to show the world to test first?

## What could this look like?

For each commit, an "editor" will show the changes being made.

Code output (whether that's some text, a visualisation of a data-structure returned, or an error message)
will be shown when it changes.

Additional explanatory material ("slides") can be shown too.

## Implementation thoughts

Git has a useful "word diff" mode which shows changes on a word level. Diff is
very easily confused, but you can get a reasonably good output using:

	git diff -U0 --function-context --word-diff --word-diff-regex='(\\w+|.)'

This prefers a diff matching a whole "word" but will accept arbitrary characters (punctuation etc.) too.

Function Context is most useful. If you're not using a C-like language, you'll want to configure this in `.gitconfig`
for example:

```
[diff "purescript"]
xfuncname = "^\\S"
```

To parse the output, you'd want to use `--word-diff=porcelain`. This output is *almost* usable, but not quite,
as it doesn't distinguish between newlines in input and output.
[This post](http://git.661346.n2.nabble.com/Understanding-and-improving-word-diff-td5717239.html) goes into
some useful detail on the topic.

Instead, Google's [`diff-match-patch`](https://github.com/google/diff-match-patch/) gives comparable results,
with a tweakable efficiency cleanup parameter.

The main issue with this is that you don't get the Function Context for free, so have to instead find the
boundaries of functions (in input and output) separately.

### Frontend

This could be for example:

* a web app, which analyses a git repo available either locally on the web
  server or on github etc.
* an Electron app, which can then both display the output, and also query a git
  repo locally. This might have the benefit of being able to set the working copy
  to the current commit dynamically, in case you also want to record other content
  (in the editor, terminal, or by running the program in its current state)

### Command Output

Rather than running a command dynamically on presentation (a security risk) the author is encouraged
to run a watch command which runs an entrypoint and saves output in an appropriate file, to be committed
along with the source (and rebased/squashed as appropriate.)

The frontend will then simply display this when it sees the named file change in a commit.

Output could be text / markdown / diagrams created by the program as images, or via some source like
https://mermaid-js.github.io/mermaid/

### Slides

Every commit which introduces a new file in a given directory could display it as a file. For example any
changes to:

	.finnerty/slides/*.markdown

(Again, this could be markdown or similar outputs. Perhaps the command output would be done using exactly
the same mechanism?)

## Origin of the name

https://en.wikipedia.org/wiki/Player_Piano_(novel)
