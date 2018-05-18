# ~jflatow

To bootstrap on a machine w/ my *Dropbox*:

````
cd ~/Dropbox/Code/stencil/users/jflatow
````

Otherwise, if not using *Dropbox*:

````
cd /usr/local/src/etc/users/jflatow
````

Then:

````
git clone git@github.com:jflatow/jflatow.git
cd jflatow
./install
````

To make a change to *emacs* prefs:

````
emacs .emacs
./install
````

## Notes to future self

On the occasion of my next reboot,
 there should be no ambiguity about how to recover my session.

All my instructions to myself for bootstrapping packages should live in this repo.
Notes for *emacs* are in `.emacs` itself.

TODO:
 macbook from scratch:
  /usr/local/{src,etc}
  /usr/local/src/lang/{C,JavaScript,Python,Plato,...}
  /usr/local/src/scratch/{emacs,web,...}
   latest emacs, Emacs.app
   Python3
    matplotlib
    ipython + Jupyter + notebeook setup?
   Chrome canary + React dev tools?
