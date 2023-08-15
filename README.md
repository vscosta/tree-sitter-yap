# tree-sitter-prolog

This is am adaptation of Shane Rukiza's tree-sitter-prolog. It
includes YAP extensions, more grammar constructs, built-ins and an
interface to emacs-29 treesitter package.


## How to use

To include in your project.

> npm install --save tree-sitter-yap

## How to run

* Clone git
* Install dependencies
  > npm install

  > npm install -g node-gyp

* Build
  > npm run build

* Run
  > tree-sitter generate

  > tree-sitter parse examples/example1.pl

  > tree-sitter test

  > npm test

## ToDo

* Implement CHR parsing.
* Better match ISO
* Add floats
* Add context to some operators e.g =.. is Var =.. List and is.

## When using in Atom apps.

Ensure that you have the latest version of tree-sitter-yap as
**"tree-sitter-cli": "^0.15.3"** is required.

## vsc updates

+ to use in emacs-29 enable
