# Description

Please include a summary of the change and a link to which issue is fixed. Please also include relevant motivation and context. List any dependencies that are required for this change.

Fixes # (issue)

# Discussion

Mention there any suspicious parts of the new code, or the ideas that you'd like to discuss in regards to this change.

# Checklist:
Everything in this checklist is required for each PR.  Please do not approve a PR that does not have all of these items.

- [ ] Git hygiene:
  - I have pulled from master before submitting this PR
  - There are no merge conflicts.
- [ ] I've added the new dependencies as:
  - ASDF dependencies,
  - Git submodules,
    ```sh
	cd /path/to/nyxt/checkout
    git submodule add https://gitlab.common-lisp.net/nyxt/py-configparser _build/py-configparser
    ```
  - and Guix dependencies.
- [ ] My code follows the style guidelines for Common Lisp code. See:
  - [Norvig & Pitman's Tutorial on Good Lisp Programming Style (PDF)](https://www.cs.umd.edu/~nau/cmsc421/norvig-lisp-style.pdf)
  - [Google Common Lisp Style Guide](https://google.github.io/styleguide/lispguide.xml)
- [ ] I have performed a self-review of my own code.
- [ ] My code has been reviewed by at least one peer.  (The peer review to approve a PR counts.  The reviewer must download and test the code.)
- [ ] Documentation:
  - All my code has docstrings and `:documentation`s written in the aforementioned style.  (It's OK to skip the docstring for really trivial parts.)
  - I have updated the existing documentation to match my changes.
  - I have commented my code in hard-to-understand areas.
  - I have updated the `changelog.lisp` with my changes if it's anything user-facing (new features, important bug fix, compatibility breakage).
    - Changelog update should be a separate commit.
  - I have added a `migration.lisp` entry for all compatibility-breaking changes.
  - (If this changes something about the features showcased on Nyxt website) I have these changes described in the new/existing article at Nyxt website or will notify one of maintainters to do so.
- [ ] Compilation and tests:
  - My changes generate no new warnings.
  - I have added tests that prove my fix is effective or that my feature works.  (If possible.)
  - I ran the tests locally (`(asdf:test-system :nyxt)` and `(asdf:test-system :nyxt/gi-gtk)`) and they pass.
