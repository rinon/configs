# configs

Currently I have only polished and committed my emacs config, but
more configs will be added (eventually).

I use [dotbot](https://git.io/dotbot) to symlink my config files from a
(private) config repo into the correct locations in my home folder.

## Emacs config

All files under `emacs/` should be copied or symlinked into `~/.emacs.d/`. You
will probably need to customize this config to get it working in your
environment, since I haven't tested this in a new emacs installation. I
recommend copying snippets into your existing emacs config rather than trying to
reuse this config wholesale.

*Important:* I byte-compile my emacs config for speed (`M-x byte-compile-file`
and point it at `~/.emacs.d/init.el`). If you don't want to byte-compile your
init, remove the `eval-when-compile` call and just require `use-package`
instead.

I use `~/.emacs.d/init.el` rather than `~/.emacs` to keep all emacs related
files in a single directory.

This config heavily relies on a snapshot of the excellent org-mode
customizations found
at [http://doc.norang.ca/org-mode.html](http://doc.norang.ca/org-mode.html). I
have included this snapshot in `emacs/lisp/org-mode.el` although I recommend
reading the original guide and regenerating your own version.

This config includes a customized version of rcirc that allows connections to
multiple servers via ZNC on the same host. If you do not need this functionality
you should remove `emacs/lisp/rcirc.el` and install the upstream version via
ELPA.
